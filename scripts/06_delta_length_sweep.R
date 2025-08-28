# scripts/06_delta_length_sweep.R
# Reduce balanced chunks to various lengths, run stylo (Delta), cluster, compute ARI.
# Per-run and per-length summaries; CA workaround enabled.

pkgs <- c("yaml","fs","readr","dplyr","stylo","mclust","tidyr")
invisible(lapply(pkgs, require, character.only = TRUE))

# If you have %||% elsewhere, define here too:
`%||%` <- function(a,b) if (is.null(a)) b else a

source("R/length_windows.R")
source("R/labels_from_filenames.R")

P <- yaml::read_yaml("config/params.yaml")

# Input: path to balanced 8k chunks (press Enter to auto-pick newest)
balanced_chunks_dir <- fs::path_expand(
  readline(prompt = "Path to balanced chunks folder (press Enter to auto-pick newest): ")
)
if (balanced_chunks_dir == "") {
  cand <- fs::dir_ls("data/interim", type = "directory", recurse = FALSE)
  cand <- cand[grepl("fixed_\\d+_balanced_\\d+_", basename(cand))]
  if (length(cand) == 0) stop("No balanced fixed-* folders found in data/interim.")
  balanced_chunks_dir <- file.path(cand[which.max(file.info(cand)$mtime)], "chunks")
  message("Using: ", balanced_chunks_dir)
}
if (!dir.exists(balanced_chunks_dir)) stop("Missing folder: ", balanced_chunks_dir)

# Parameters from config
lengths    <- P$sweep$lengths
replicates <- P$sweep$replicates
seed_base  <- P$sweep$seed_base

ngram_size <- P$stylo$ngram_size %||% 1
mfw_min    <- P$stylo$mfw_min    %||% 100
mfw_max    <- P$stylo$mfw_max    %||% 500

results_dir <- P$outputs$results_dir
fs::dir_create(results_dir)
runs_parent <- fs::path("data","interim","length_sweep_runs")
fs::dir_create(runs_parent)

# Source files & labels
src_files <- fs::dir_ls(balanced_chunks_dir, glob = "*.txt", type = "file")
if (length(src_files) == 0) stop("No .txt files in: ", balanced_chunks_dir)

src_tbl <- tibble::tibble(file = src_files, author = author_from_fname(src_files))
authors <- sort(unique(src_tbl$author))
k <- length(authors)

# Core loop
all_rows <- list(); i <- 0L
for (L in lengths) {
  message("\n=== Length: ", L, " ===")
  for (r in seq_len(replicates)) {
    seed <- seed_base + 1000L*L + r
    
    # 1) Reduced corpus for this run
    run_dir  <- fs::path(runs_parent, sprintf("len_%05d_rep_%02d", L, r))
    out_cdir <- fs::path(run_dir, "chunks")
    fs::dir_create(out_cdir, recurse = TRUE)
    
    for (f in src_tbl$file) {
      txt <- readr::read_file(f)
      s <- as.integer((seed + sum(utf8ToInt(basename(f)))) %% .Machine$integer.max)
      subtxt <- random_window(txt, L, seed = s)
      if (!is.na(subtxt)) readr::write_file(subtxt, fs::path(out_cdir, basename(f)))
    }
    
    out_files <- fs::dir_ls(out_cdir, glob = "*.txt", type = "file")
    if (length(out_files) != length(src_files)) {
      warning("Some files shorter than ", L, " were skipped in ", run_dir)
    }
    
    # 2) stylo (Delta) with CA workaround
    res <- stylo(
      gui = FALSE,
      corpus.dir   = out_cdir,
      corpus.lang  = "Other",
      ngram.size   = ngram_size,
      mfw.min      = mfw_min,
      mfw.max      = mfw_max,
      culling.min  = 0, culling.max = 0,
      distance     = "delta",
      analysis.type = "CA",   # <-- avoids internal var bug
      display.on.screen = FALSE,
      write.png.file = FALSE, write.pdf.file = FALSE,
      write.svg.file = FALSE, write.jpg.file = FALSE, write.data.file = FALSE
    )
    
    D <- res$distance.table
    if (is.null(D)) { warning("No distance table for L=", L, ", rep ", r); next }
    
    labels_true <- author_from_fname(rownames(D))
    
    # 3) Hierarchical + ARI
    hc   <- hclust(as.dist(D), method = "ward.D2")
    cl_h <- cutree(hc, k = k)
    ari_h <- mclust::adjustedRandIndex(labels_true, cl_h)
    
    # 4) K-means via MDS + ARI
    n <- nrow(D); nd <- max(2, min(n - 1, 50))
    X <- cmdscale(as.dist(D), k = nd)
    set.seed(seed)
    km <- kmeans(X, centers = k, nstart = 20)
    ari_k <- mclust::adjustedRandIndex(labels_true, km$cluster)
    
    i <- i + 1L
    all_rows[[i]] <- tibble::tibble(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      source_corpus = balanced_chunks_dir,
      run_dir   = run_dir,
      length    = L,
      replicate = r,
      k         = k,
      n_docs    = nrow(D),
      ngram     = ngram_size,
      mfw_min   = mfw_min,
      mfw_max   = mfw_max,
      ari_hclust = ari_h,
      ari_kmeans = ari_k
    )
    
    message(sprintf("L=%d rep=%d  ARI(hclust)=%.3f  ARI(kmeans)=%.3f", L, r, ari_h, ari_k))
  }
}

# Save per-run
res_tbl <- dplyr::bind_rows(all_rows)
per_run_csv <- fs::path(results_dir, "delta_ari_length_sweep_runs.csv")
readr::write_csv(res_tbl, per_run_csv)

# Summary per length & method
sum_tbl <- res_tbl %>%
  tidyr::pivot_longer(c(ari_hclust, ari_kmeans), names_to="method", values_to="ari") %>%
  group_by(length, method) %>%
  summarise(mean_ari = mean(ari, na.rm=TRUE),
            sd_ari   = sd(ari, na.rm=TRUE),
            n_runs   = sum(!is.na(ari)),
            .groups="drop") %>%
  arrange(length, method)

summary_csv <- fs::path(results_dir, "delta_ari_length_sweep_summary.csv")
readr::write_csv(sum_tbl, summary_csv)

message("\nPer-run: ", per_run_csv)
message("Summary: ", summary_csv)
print(sum_tbl)
