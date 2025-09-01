# scripts/06_delta_length_sweep.R
# Multi-length Delta sweep for Classical Chinese (character tokens).
# For each chunk length, sample contiguous windows (per doc), then sweep MFW values.
# Compute ARI(hclust/kmeans), move stylo side-files to artifacts/, save results + plots.

pkgs <- c("yaml","stylo","mclust","dplyr","readr","fs","purrr","stringr","tidyr","ggplot2")
invisible(lapply(pkgs, require, character.only = TRUE))

`%||%` <- function(a,b) if (is.null(a)) b else a
ts_chr <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# helpers you already have
source("R/labels_from_filenames.R")  # author_from_fname()
source("R/length_windows.R")         # random_window(text, target_len, seed=NULL)

P <- yaml::read_yaml("config/params.yaml")

# ---------- Parameters ----------
# lengths: start -> end (inclusive), stepping down
start_len  <- P$sweep$start_len  %||% 8000
end_len    <- P$sweep$end_len    %||% 700
step_size  <- P$sweep$step_size  %||% 1000
mfw_values <- P$sweep$mfw_values %||% seq(100, 800, by = 100)

ngram_size    <- P$stylo$ngram_size %||% 1
results_dir   <- P$outputs$results_dir   %||% "reports/results"
artifacts_dir <- P$outputs$artifacts_dir %||% "artifacts/stylo"
fs::dir_create(results_dir)
fs::dir_create(artifacts_dir)

out_csv <- file.path(results_dir, "stylo_ari_length_sweep.csv")

# ---------- Junk sweeper (fast, no tempdir) ----------
sweep_stylo_junk <- function(target_dir, pre_files, run_started) {
  fs::dir_create(target_dir)
  # replace previous contents for this target
  old <- fs::dir_ls(target_dir, all = TRUE, type = "any", recurse = FALSE)
  if (length(old)) {
    fs::file_delete(old[fs::is_file(old)])
    fs::dir_delete(old[fs::is_dir(old)])
  }
  now <- fs::dir_ls(".", all = TRUE, type = "any", recurse = FALSE)
  candidates <- setdiff(now, pre_files)
  if (!length(candidates)) return(invisible(TRUE))
  
  info <- file.info(candidates)
  recent <- candidates[ which(info$mtime >= (run_started - 1)) ]
  
  junk_patterns <- c(
    "^stylo_.*",
    "^distance.*\\.txt$",
    "^.*_config.*\\.txt$",
    "^.*word.*list.*\\.txt$",
    "^.*features.*\\.txt$",
    "^.*table.*freq.*\\.txt$",
    "^.*\\.(csv|png|jpg|jpeg|svg|pdf)$"
  )
  is_junk <- function(path)
    any(grepl(paste(junk_patterns, collapse="|"), basename(path), ignore.case = TRUE))
  
  recent <- recent[ vapply(recent, is_junk, logical(1)) ]
  if (!length(recent)) return(invisible(TRUE))
  
  for (p in recent) {
    dest <- fs::path(target_dir, basename(p))
    if (fs::is_dir(p)) { fs::dir_copy(p, dest, overwrite = TRUE); fs::dir_delete(p) }
    else               { fs::file_move(p, dest) }
  }
  invisible(TRUE)
}

# ---------- Pick corpus (Enter = newest balanced set) ----------
corpus_dir <- fs::path_expand(
  readline("Path to starting chunks folder (Enter = newest balanced set): ")
)
if (corpus_dir == "") {
  cand <- fs::dir_ls("data/interim", type = "directory", recurse = FALSE)
  cand <- cand[grepl("fixed_\\d+_balanced_\\d+_", basename(cand))]
  if (!length(cand)) stop("No balanced fixed-* folders in data/interim.")
  corpus_dir <- file.path(cand[which.max(file.info(cand)$mtime)], "chunks")
  message("Using: ", corpus_dir)
}
stopifnot(dir.exists(corpus_dir))

# ---------- Load texts once ----------
files   <- fs::dir_ls(corpus_dir, glob = "*.txt", type = "file")
stopifnot(length(files) > 0)
texts   <- purrr::map_chr(files, readr::read_file)
fnames  <- basename(files)
authors0 <- author_from_fname(fnames)

# ---------- Sweep over lengths ----------
all_rows <- list(); i <- 0L
for (L in seq(from = start_len, to = end_len, by = -step_size)) {
  
  message("\n### Chunk length = ", L, " ###")
  
  # Build a temporary corpus for this length (do NOT touch your originals)
  tmp_dir <- fs::file_temp(pattern = paste0("len_", L, "_"))
  fs::dir_create(tmp_dir)
  
  for (j in seq_along(texts)) {
    # deterministic seed per file & length for reproducibility
    s <- (sum(utf8ToInt(fnames[j])) + L) %% .Machine$integer.max
    sample_text <- random_window(texts[j], L, seed = s)
    if (is.na(sample_text)) {
      # if a text is too short, skip it (keeps filenames aligned for stylo)
      next
    }
    readr::write_file(sample_text, file.path(tmp_dir, fnames[j]))
  }
  
  # ---------- MFW sweep for this length ----------
  for (M in mfw_values) {
    message(sprintf("L=%d  MFW=%d ...", L, M))
    
    run_started <- Sys.time()
    pre_files   <- fs::dir_ls(".", all = TRUE, type = "any", recurse = FALSE)
    
    res <- stylo(
      gui = FALSE,
      corpus.dir        = tmp_dir,
      corpus.lang       = "Other",
      analyzed.features = "c",          # character tokens
      ngram.size        = ngram_size,   # 1=uni, 2=bi, ...
      mfw.min           = M,
      mfw.max           = M,
      culling.min       = 0,
      culling.max       = 0,
      delete.pronouns   = FALSE,
      distance          = "delta",
      analysis.type     = "CA",
      display.on.screen = FALSE,
      write.png.file    = FALSE, write.pdf.file = FALSE,
      write.svg.file    = FALSE, write.jpg.file = FALSE,
      write.data.file   = FALSE
    )
    
    # move stylo side files -> artifacts/stylo/length_<L>/mfw_<M>/
    run_art_dir <- fs::path(artifacts_dir, sprintf("length_%04d/mfw_%04d", L, M))
    sweep_stylo_junk(run_art_dir, pre_files, run_started)
    
    # distances & ARIs
    D <- res$distance.table
    if (is.null(D) || !is.matrix(D) || any(!is.finite(D))) {
      warning(sprintf("Skipping: bad distance table (L=%d, MFW=%d).", L, M))
      i <- i + 1L
      all_rows[[i]] <- tibble::tibble(
        timestamp  = ts_chr(),
        chunk_len  = L,
        corpus     = tmp_dir,
        n_docs     = NA_real_,
        k          = NA_real_,
        ngram      = ngram_size,
        mfw_min    = M,
        mfw_max    = M,
        ari_hclust = NA_real_,
        ari_kmeans = NA_real_
      )
      next
    }
    
    labels_true <- author_from_fname(rownames(D))
    n_docs <- nrow(D)
    k_now  <- length(unique(labels_true))
    if (k_now < 2 || k_now > n_docs) {
      warning(sprintf("Skipping: 2<=k<=n_docs not met (k=%d, n_docs=%d).", k_now, n_docs))
      next
    }
    
    # hclust
    hc   <- res$hc
    if (is.null(hc)) hc <- hclust(as.dist(D), method = "ward.D2")
    cl_h <- cutree(hc, k = k_now)
    ari_h <- mclust::adjustedRandIndex(labels_true, cl_h)
    
    # k-means via MDS (use richer dims for stability)
    nd <- max(2, min(n_docs - 1, 50))
    X  <- cmdscale(as.dist(D), k = nd)
    set.seed(42)
    km <- kmeans(X, centers = k_now, nstart = 20)
    ari_k <- mclust::adjustedRandIndex(labels_true, km$cluster)
    
    i <- i + 1L
    all_rows[[i]] <- tibble::tibble(
      timestamp  = ts_chr(),
      chunk_len  = L,
      corpus     = tmp_dir,
      n_docs     = n_docs,
      k          = k_now,
      ngram      = ngram_size,
      mfw_min    = M,
      mfw_max    = M,
      ari_hclust = ari_h,
      ari_kmeans = ari_k
    )
    
    message(sprintf("L=%d  MFW=%d   ARI(hclust)=%.3f   ARI(kmeans)=%.3f",
                    L, M, ari_h, ari_k))
  }
  # (optional) you can remove tmp_dir here if you don't want to keep sampled texts
  # fs::dir_delete(tmp_dir)
}

# ---------- Save results (append-safe; timestamp as character) ----------
runs_tbl <- dplyr::bind_rows(all_rows) %>%
  dplyr::mutate(timestamp = as.character(timestamp))

if (file.exists(out_csv)) {
  old <- readr::read_csv(
    out_csv,
    col_types = readr::cols(
      timestamp  = readr::col_character(),
      chunk_len  = readr::col_double(),
      corpus     = readr::col_character(),
      n_docs     = readr::col_double(),
      k          = readr::col_double(),
      ngram      = readr::col_double(),
      mfw_min    = readr::col_double(),
      mfw_max    = readr::col_double(),
      ari_hclust = readr::col_double(),
      ari_kmeans = readr::col_double()
    ),
    show_col_types = FALSE
  )
  runs_tbl <- dplyr::bind_rows(old, runs_tbl)
}
readr::write_csv(runs_tbl, out_csv)
cat("Saved length-sweep results:", out_csv, "\n")

print(
  runs_tbl %>%
    dplyr::select(timestamp, chunk_len, mfw_max, ari_hclust, ari_kmeans) %>%
    tail(20),
  width = Inf
)

# ---------- Auto-plots ----------
tidy <- runs_tbl %>%
  tidyr::pivot_longer(c(ari_hclust, ari_kmeans),
                      names_to = "method", values_to = "ari") %>%
  dplyr::mutate(method = dplyr::recode(method,
                                       ari_hclust = "hclust",
                                       ari_kmeans = "kmeans")) %>%
  dplyr::filter(!is.na(ari))

# Heatmap per method
p_heat <- ggplot(tidy, aes(x = factor(mfw_max), y = chunk_len, fill = ari)) +
  geom_tile() +
  scale_y_reverse(breaks = sort(unique(tidy$chunk_len), decreasing = TRUE)) +
  scale_fill_viridis_c(limits = c(0,1), option = "C") +
  facet_wrap(~ method, ncol = 1) +
  labs(x = "MFW", y = "Chunk length (chars)",
       fill = "ARI",
       title = "Delta clustering: ARI heatmap by length × MFW") +
  theme_minimal(base_size = 12)

heat_png <- file.path(results_dir, "length_mfw_ari_heatmap.png")
ggsave(heat_png, p_heat, width = 8, height = 10, dpi = 150)
cat("Saved heatmap:", heat_png, "\n")

# Lines: mean ARI vs chunk length, colored by MFW
lines_df <- tidy %>%
  dplyr::group_by(method, chunk_len, mfw_max) %>%
  dplyr::summarise(mean_ari = mean(ari, na.rm = TRUE), .groups = "drop")

p_lines <- ggplot(lines_df,
                  aes(x = chunk_len, y = mean_ari,
                      color = factor(mfw_max), group = mfw_max)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 1.8) +
  scale_x_reverse(breaks = sort(unique(lines_df$chunk_len), decreasing = TRUE)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Chunk length (chars)", y = "Mean ARI",
       color = "MFW",
       title = "Delta clustering: Mean ARI vs chunk length") +
  facet_wrap(~ method) +
  theme_minimal(base_size = 12)

lines_png <- file.path(results_dir, "length_vs_ari_by_mfw.png")
ggsave(lines_png, p_lines, width = 9, height = 5.5, dpi = 150)
cat("Saved line plot:", lines_png, "\n")
