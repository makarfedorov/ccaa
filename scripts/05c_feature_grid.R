# Sweep features on full sample: MFW × ngram.size × distance
# Saves per-run ARIs + a summary by (ngram, distance).
# Moves stylo side-files to artifacts/stylo/features/<dist>/ng<ngram>/mfw_<M>.

pkgs <- c("yaml","stylo","mclust","dplyr","readr","fs")
invisible(lapply(pkgs, require, character.only = TRUE))

`%||%` <- function(a,b) if (is.null(a)) b else a
ts_chr <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
source("R/labels_from_filenames.R")  # author_from_fname()

P <- yaml::read_yaml("config/params.yaml")

# --- config (edit in params.yaml if you wish) ---
mfw_values  <- P$sweep$mfw_values %||% seq(100, 800, by = 100)
ngram_values <- P$sweep$ngram_values %||% 1:4
distances   <- P$sweep$distances %||% c("delta","cosine","argamon","euclidean", "eder", "simple", "canberra")

results_dir   <- P$outputs$results_dir   %||% "reports/results"
artifacts_dir <- P$outputs$artifacts_dir %||% "artifacts/stylo"
fs::dir_create(results_dir); fs::dir_create(artifacts_dir)

out_detail  <- file.path(results_dir, "stylo_ari_feature_grid.csv")
out_summary <- file.path(results_dir, "stylo_ari_feature_grid_summary.csv")

# --- pick corpus (Enter = newest balanced set) ---
corpus_dir <- fs::path_expand(readline("Path to chunks folder (Enter = newest balanced set): "))
if (corpus_dir == "") {
  cand <- fs::dir_ls("data/interim", type="directory", recurse=FALSE)
  cand <- cand[grepl("fixed_\\d+_balanced_\\d+_", basename(cand))]
  stopifnot(length(cand) > 0)
  corpus_dir <- file.path(cand[which.max(file.info(cand)$mtime)], "chunks")
  message("Using: ", corpus_dir)
}
stopifnot(dir.exists(corpus_dir))

# --- compute total tokens (characters) once ---
files <- fs::dir_ls(corpus_dir, glob="*.txt", type="file")
tokens_total <- sum(vapply(files, function(f) nchar(readr::read_file(f)), integer(1)))

# --- junk sweeper (fast) ---
sweep_stylo_junk <- function(target_dir, pre_files, run_started) {
  fs::dir_create(target_dir)
  # clean target
  old <- fs::dir_ls(target_dir, all=TRUE, type="any", recurse=FALSE)
  if (length(old)) { fs::file_delete(old[fs::is_file(old)]); fs::dir_delete(old[fs::is_dir(old)]) }
  # new items at project root
  now <- fs::dir_ls(".", all=TRUE, type="any", recurse=FALSE)
  candidates <- setdiff(now, pre_files); if (!length(candidates)) return(invisible(TRUE))
  info <- file.info(candidates)
  recent <- candidates[ which(info$mtime >= (run_started - 1)) ]
  junk_patterns <- c(
    "^stylo_.*","^distance.*\\.txt$","^.*_config.*\\.txt$",
    "^.*word.*list.*\\.txt$","^.*features.*\\.txt$","^.*table.*freq.*\\.txt$",
    "^.*\\.(csv|png|jpg|jpeg|svg|pdf)$"
  )
  is_junk <- function(p) any(grepl(paste(junk_patterns, collapse="|"), basename(p), ignore.case=TRUE))
  recent <- recent[ vapply(recent, is_junk, logical(1)) ]
  if (!length(recent)) return(invisible(TRUE))
  for (p in recent) {
    dest <- fs::path(target_dir, basename(p))
    if (fs::is_dir(p)) { fs::dir_copy(p, dest, overwrite=TRUE); fs::dir_delete(p) }
    else               { fs::file_move(p, dest) }
  }
  invisible(TRUE)
}

rows <- list(); i <- 0L

for (ng in ngram_values) {
  for (dist in distances) {
    message("\n=== ngram = ", ng, "    distance = ", dist, " ===")
    for (M in mfw_values) {
      message(sprintf("   MFW=%d ...", M))
      run_started <- Sys.time()
      pre_files   <- fs::dir_ls(".", all=TRUE, type="any", recurse=FALSE)
      
      res <- stylo(
        gui = FALSE,
        corpus.dir        = corpus_dir,
        corpus.lang       = "Other",
        analyzed.features = "c",       # characters
        ngram.size        = ng,        # 1..4
        mfw.min           = M,
        mfw.max           = M,
        culling.min       = 0,
        culling.max       = 0,
        delete.pronouns   = FALSE,
        distance.measure  = dist,      # "delta","cosine",...
        analysis.type     = "CA",      # avoids GUI plotting side-effects
        display.on.screen = FALSE,
        write.png.file    = FALSE, write.pdf.file = FALSE,
        write.svg.file    = FALSE, write.jpg.file = FALSE,
        write.data.file   = FALSE
      )
      
      # move stylo side files to artifacts/stylo/features/<dist>/ng<ng>/mfw_<M>/
      run_art_dir <- fs::path(artifacts_dir, "features", dist, sprintf("ng%d", ng), sprintf("mfw_%04d", M))
      sweep_stylo_junk(run_art_dir, pre_files, run_started)
      
      D <- res$distance.table
      if (is.null(D) || !is.matrix(D) || any(!is.finite(D))) {
        warning(sprintf("Skipping: bad distance table (ng=%d dist=%s MFW=%d).", ng, dist, M))
        i <- i + 1L
        rows[[i]] <- dplyr::tibble(
          timestamp  = ts_chr(),
          corpus     = corpus_dir,
          tokens     = tokens_total,
          n_docs     = NA_real_,
          k          = NA_real_,
          ngram      = ng,
          distance   = dist,
          mfw_min    = M,
          mfw_max    = M,
          ari_hclust = NA_real_,
          ari_kmeans = NA_real_
        )
        next
      }
      
      labels_true <- author_from_fname(rownames(D))
      n_docs <- nrow(D); k_now <- length(unique(labels_true))
      if (k_now < 2 || k_now > n_docs) {
        warning(sprintf("Skipping: 2<=k<=n_docs not met (k=%d n_docs=%d).", k_now, n_docs))
        next
      }
      
      # hclust ARI
      hc <- if (is.null(res$hc)) hclust(as.dist(D), method="ward.D2") else res$hc
      cl_h <- cutree(hc, k = k_now)
      ari_h <- mclust::adjustedRandIndex(labels_true, cl_h)
      
      # k-means via MDS + ARI
      nd <- max(2, min(n_docs - 1, 50))
      X  <- cmdscale(as.dist(D), k = nd)
      set.seed(42)
      km <- kmeans(X, centers = k_now, nstart = 20)
      ari_k <- mclust::adjustedRandIndex(labels_true, km$cluster)
      
      i <- i + 1L
      rows[[i]] <- dplyr::tibble(
        timestamp  = ts_chr(),
        corpus     = corpus_dir,
        tokens     = tokens_total,
        n_docs     = n_docs,
        k          = k_now,
        ngram      = ng,
        distance   = dist,
        mfw_min    = M,
        mfw_max    = M,
        ari_hclust = ari_h,
        ari_kmeans = ari_k
      )
      
      message(sprintf("      ARI(hclust)=%.3f  ARI(kmeans)=%.3f", ari_h, ari_k))
    }
  }
}

# --- write detailed rows (append-safe) ---
detail_tbl <- dplyr::bind_rows(rows) %>% dplyr::mutate(timestamp = as.character(timestamp))
if (file.exists(out_detail)) {
  old <- readr::read_csv(
    out_detail,
    col_types = readr::cols(
      timestamp  = readr::col_character(),
      corpus     = readr::col_character(),
      tokens     = readr::col_double(),
      n_docs     = readr::col_double(),
      k          = readr::col_double(),
      ngram      = readr::col_double(),
      distance   = readr::col_character(),
      mfw_min    = readr::col_double(),
      mfw_max    = readr::col_double(),
      ari_hclust = readr::col_double(),
      ari_kmeans = readr::col_double()
    ),
    show_col_types = FALSE
  )
  detail_tbl <- dplyr::bind_rows(old, detail_tbl)
}
readr::write_csv(detail_tbl, out_detail)
cat("Saved feature-grid detail:", out_detail, "\n")

# --- build & write summary by (ngram, distance) across all MFWs of THIS RUN ---
this_run <- dplyr::bind_rows(rows)
summary_tbl <- this_run %>%
  dplyr::filter(!is.na(ari_hclust) & !is.na(ari_kmeans)) %>%
  dplyr::group_by(ngram, distance) %>%
  dplyr::summarise(
    timestamp    = ts_chr(),
    corpus       = unique(corpus_dir)[1],
    tokens       = unique(tokens_total)[1],
    mfw_min_grid = min(mfw_values),   # grid min (not per-result)
    mfw_max_grid = max(mfw_values),   # grid max
    n_mfw_used   = dplyr::n(),
    mean_ari_hclust   = mean(ari_hclust),
    median_ari_hclust = median(ari_hclust),
    sd_ari_hclust     = sd(ari_hclust),
    mean_ari_kmeans   = mean(ari_kmeans),
    median_ari_kmeans = median(ari_kmeans),
    sd_ari_kmeans     = sd(ari_kmeans),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(mean_ari_hclust))

if (file.exists(out_summary)) {
  old_sum <- readr::read_csv(
    out_summary,
    col_types = readr::cols(
      timestamp        = readr::col_character(),
      corpus           = readr::col_character(),
      tokens           = readr::col_double(),
      ngram            = readr::col_double(),
      distance         = readr::col_character(),
      mfw_min_grid     = readr::col_double(),
      mfw_max_grid     = readr::col_double(),
      n_mfw_used       = readr::col_double(),
      mean_ari_hclust   = readr::col_double(),
      median_ari_hclust = readr::col_double(),
      sd_ari_hclust     = readr::col_double(),
      mean_ari_kmeans   = readr::col_double(),
      median_ari_kmeans = readr::col_double(),
      sd_ari_kmeans     = readr::col_double()
    ),
    show_col_types = FALSE
  )
  summary_tbl <- dplyr::bind_rows(old_sum, summary_tbl)
}
readr::write_csv(summary_tbl, out_summary)
cat("Saved feature-grid summary:", out_summary, "\n")

print(summary_tbl, width = Inf)
