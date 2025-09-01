# scripts/05b_mfw_sweep.R
# Sweep over MFW values (character tokens), compute ARI (hclust + kmeans),
# collect stylo side-files into artifacts/stylo/mfw_<M>/ for each run (fast, no tempdir).

pkgs <- c("yaml","stylo","mclust","dplyr","readr","fs","stats")
invisible(lapply(pkgs, require, character.only = TRUE))

`%||%` <- function(a,b) if (is.null(a)) b else a
source("R/labels_from_filenames.R")  # author_from_fname()

P <- yaml::read_yaml("config/params.yaml")

# ----- choose corpus (Enter = newest balanced set) -----
corpus_dir <- fs::path_expand(
  readline("Path to chunks folder (Enter = newest balanced set): ")
)
if (corpus_dir == "") {
  cand <- fs::dir_ls("data/interim", type = "directory", recurse = FALSE)
  cand <- cand[grepl("fixed_\\d+_balanced_\\d+_", basename(cand))]
  if (!length(cand)) stop("No balanced fixed-* folders in data/interim.")
  corpus_dir <- file.path(cand[which.max(file.info(cand)$mtime)], "chunks")
  message("Using: ", corpus_dir)
}
stopifnot(dir.exists(corpus_dir))

# ----- grid of MFW values (edit as needed) -----
mfw_values <- P$sweep$mfw_values %||% seq(100, 800, by = 100)

# ----- params -----
ngram_size <- P$stylo$ngram_size %||% 1
results_dir <- P$outputs$results_dir %||% "reports/results"
artifacts_base <- P$outputs$artifacts_dir %||% "artifacts/stylo"
fs::dir_create(results_dir)
fs::dir_create(artifacts_base)

out_csv <- file.path(results_dir, "stylo_ari_mfw_sweep.csv")

# helper: move stylo side files created during this run into target_dir
sweep_stylo_junk <- function(target_dir, pre_files, run_started) {
  fs::dir_create(target_dir)
  # clean target
  old <- fs::dir_ls(target_dir, all = TRUE, type = "any", recurse = FALSE)
  if (length(old)) {
    fs::file_delete(old[fs::is_file(old)])
    fs::dir_delete(old[fs::is_dir(old)])
  }
  # consider only *new* items at project root
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
    if (fs::is_dir(p)) {
      fs::dir_copy(p, dest, overwrite = TRUE)
      fs::dir_delete(p)
    } else {
      fs::file_move(p, dest)
    }
  }
  invisible(TRUE)
}

# ----- sweep -----
all_rows <- list(); i <- 0L

for (M in mfw_values) {
  message("\n=== MFW = ", M, " ===")
  
  # mark root contents before stylo writes
  run_started <- Sys.time()
  pre_files   <- fs::dir_ls(".", all = TRUE, type = "any", recurse = FALSE)
  
  # run stylo (fast: no tempdir / no with_dir)
  res <- stylo(
    gui = FALSE,
    corpus.dir        = corpus_dir,
    corpus.lang       = "Other",
    analyzed.features = "c",         # characters
    ngram.size        = ngram_size,  # 1=unigrams, 2=bigrams, ...
    mfw.min           = M,
    mfw.max           = M,
    culling.min       = 0,
    culling.max       = 0,
    delete.pronouns   = FALSE,
    distance          = "delta",
    analysis.type     = "CA",        # avoids plotting var bug
    display.on.screen = FALSE,
    write.png.file    = FALSE, write.pdf.file = FALSE,
    write.svg.file    = FALSE, write.jpg.file = FALSE,
    write.data.file   = FALSE
  )
  
  # move stylo side files for this run
  run_art_dir <- fs::path(artifacts_base, sprintf("mfw_%04d", M))
  sweep_stylo_junk(run_art_dir, pre_files, run_started)
  cat(sprintf("Moved stylo side files to: %s\n", run_art_dir))
  
  # extract distances and labels
  D <- res$distance.table
  if (is.null(D) || !is.matrix(D) || any(!is.finite(D))) {
    warning("Skipping MFW=", M, ": distance table missing/non-finite.")
    i <- i + 1L
    all_rows[[i]] <- dplyr::tibble(
      timestamp  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      corpus     = corpus_dir,
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
  k      <- length(unique(labels_true))
  if (k < 2 || k > n_docs) {
    warning(sprintf("Bad labels/docs at MFW=%d (k=%d, n_docs=%d).", M, k, n_docs))
    next
  }
  
  # hclust + ARI
  hc   <- res$hc
  if (is.null(hc)) hc <- hclust(as.dist(D), method = "ward.D2")
  cl_h <- cutree(hc, k = k)
  ari_h <- mclust::adjustedRandIndex(labels_true, cl_h)
  
  # k-means via MDS + ARI (use richer embedding for parity with hclust)
  nd <- max(2, min(n_docs - 1, 50))
  X  <- cmdscale(as.dist(D), k = nd)
  set.seed(42)
  km <- kmeans(X, centers = k, nstart = 20)
  ari_k <- mclust::adjustedRandIndex(labels_true, km$cluster)
  
  i <- i + 1L
  all_rows[[i]] <- dplyr::tibble(
    timestamp  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    corpus     = corpus_dir,
    n_docs     = n_docs,
    k          = k,
    ngram      = ngram_size,
    mfw_min    = M,
    mfw_max    = M,
    ari_hclust = ari_h,
    ari_kmeans = ari_k
  )
  
  message(sprintf("MFW=%d  ARI(hclust)=%.3f  ARI(kmeans)=%.3f", M, ari_h, ari_k))
}

# ----- write results (append-safe) -----
runs_tbl <- dplyr::bind_rows(all_rows)

if (file.exists(out_csv)) {
  old <- readr::read_csv(
    out_csv,
    col_types = readr::cols(
      timestamp = readr::col_character(),
      corpus = readr::col_character(),
      n_docs = readr::col_double(),
      k = readr::col_double(),
      ngram = readr::col_double(),
      mfw_min = readr::col_double(),
      mfw_max = readr::col_double(),
      ari_hclust = readr::col_double(),
      ari_kmeans = readr::col_double()
    ),
    show_col_types = FALSE
  )
  runs_tbl <- dplyr::bind_rows(old, runs_tbl)
}
readr::write_csv(runs_tbl, out_csv)
cat("Saved sweep results:", out_csv, "\n")

# quick look
print(
  runs_tbl %>%
    dplyr::select(timestamp, mfw_max, ari_hclust, ari_kmeans) %>%
    tail(10),
  width = Inf
)
