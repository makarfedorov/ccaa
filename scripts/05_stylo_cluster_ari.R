# scripts/05_stylo_cluster_ari.R
# One-shot: run stylo (Delta) on a corpus dir, cluster (hclust + kmeans), compute ARI.
# - analysis.type = "CA" workaround
# - analyzed.features = "c" (character tokens)
# - timestamp stored as character for consistent appends

pkgs <- c("yaml","stylo","mclust","dplyr","readr","fs","stats")
invisible(lapply(pkgs, require, character.only = TRUE))

`%||%` <- function(a,b) if (is.null(a)) b else a

# Helper: extract author from "作者名_2.txt" or "作者名_2"
source("R/labels_from_filenames.R")

P <- yaml::read_yaml("config/params.yaml")

# ----- Pick corpus folder (Enter = newest balanced set) -----
corpus_dir <- fs::path_expand(
  readline("Path to chunks folder (press Enter to auto-pick newest balanced set): ")
)
if (corpus_dir == "") {
  cand <- fs::dir_ls("data/interim", type = "directory", recurse = FALSE)
  cand <- cand[grepl("fixed_\\d+_balanced_\\d+_", basename(cand))]
  if (length(cand) == 0) stop("No balanced fixed-* folders found in data/interim.")
  corpus_dir <- file.path(cand[which.max(file.info(cand)$mtime)], "chunks")
  message("Using: ", corpus_dir)
}
if (!dir.exists(corpus_dir)) stop("Missing folder: ", corpus_dir)

# Quick sanity: chunk lengths
files <- fs::dir_ls(corpus_dir, glob = "*.txt", type = "file")
lens  <- vapply(files, function(f) nchar(readr::read_file(f)), integer(1))
cat(sprintf("File length (chars) — min=%d, median=%d, mean=%.1f, max=%d, n=%d\n",
            min(lens), stats::median(lens), mean(lens), max(lens), length(lens)))

# ----- Params -----
ngram_size <- P$stylo$ngram_size %||% 1
mfw_min    <- P$stylo$mfw_min    %||% 100
mfw_max    <- P$stylo$mfw_max    %||% 500

# ----- stylo (Delta) directly in working dir -----
res <- stylo(
  gui = FALSE,
  corpus.dir        = corpus_dir,
  corpus.lang       = "Other",
  analyzed.features = "c",          # characters (not words)
  ngram.size        = ngram_size,
  mfw.min           = mfw_min,
  mfw.max           = mfw_max,
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

# Debug: confirm tokenization
cat("\nTop 20 features actually used (should be single Han chars if ngram=1):\n")
print(head(res$features.actually.used, 20))

D <- res$distance.table
if (is.null(D) || nrow(D) < 2) stop("stylo did not return a valid distance.table")

# ----- Labels & sanity -----
labels_true <- author_from_fname(rownames(D))
k <- length(unique(labels_true))
n_docs <- nrow(D)
if (k < 2 || k > n_docs) {
  stop(sprintf("Bad labels/docs: need 2<=k<=n_docs, got k=%d, n_docs=%d. Check label parser.", k, n_docs))
}

# ----- Hierarchical clustering + ARI -----
hc   <- hclust(as.dist(D), method = "ward.D2")
cl_h <- cutree(hc, k = k)
ari_h <- mclust::adjustedRandIndex(labels_true, cl_h)

# ----- K-means via MDS + ARI -----
nd <- max(2, min(n_docs - 1, 50))
X  <- cmdscale(as.dist(D), k = nd)
set.seed(42)
km <- kmeans(X, centers = k, nstart = 20)
ari_k <- mclust::adjustedRandIndex(labels_true, km$cluster)

# ----- Save results (timestamp as character) -----
fs::dir_create(P$outputs$results_dir)
out_csv <- file.path(P$outputs$results_dir, "stylo_ari.csv")

now_chr <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
row <- dplyr::tibble(
  timestamp  = now_chr,
  corpus     = corpus_dir,
  n_docs     = n_docs,
  k          = k,
  ngram      = ngram_size,
  mfw_min    = mfw_min,
  mfw_max    = mfw_max,
  ari_hclust = ari_h,
  ari_kmeans = ari_k
)

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
  row <- dplyr::bind_rows(old, row)
}
readr::write_csv(row, out_csv)

cat("\nLatest results:\n")
print(tail(readr::read_csv(out_csv, show_col_types = FALSE), 1))
