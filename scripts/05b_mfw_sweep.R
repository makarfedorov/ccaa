# scripts/05b_mfw_sweep.R
pkgs <- c("yaml","stylo","mclust","dplyr","readr","fs")
invisible(lapply(pkgs, require, character.only = TRUE))
`%||%` <- function(a,b) if (is.null(a)) b else a
source("R/labels_from_filenames.R")

P <- yaml::read_yaml("config/params.yaml")

# pick corpus (Enter = newest balanced set)
corpus_dir <- fs::path_expand(readline("Path to chunks folder (Enter = newest balanced set): "))
if (corpus_dir == "") {
  cand <- fs::dir_ls("data/interim", type="directory", recurse=FALSE)
  cand <- cand[grepl("fixed_\\d+_balanced_\\d+_", basename(cand))]
  stopifnot(length(cand) > 0)
  corpus_dir <- file.path(cand[which.max(file.info(cand)$mtime)], "chunks")
  message("Using: ", corpus_dir)
}
stopifnot(dir.exists(corpus_dir))

# choose a grid of MFW values to try
mfw_grid <- c(100, 200, 300, 400, 500, 600, 800)  # edit as you like

ngram_size <- P$stylo$ngram_size %||% 1
mfw_min    <- P$stylo$mfw_min    %||% 100

fs::dir_create(P$outputs$results_dir)
out_csv <- file.path(P$outputs$results_dir, "stylo_ari_mfw_sweep.csv")


for (mfw_max in mfw_grid) {
  message("MFW = ", mfw_max)
  res <- stylo(
    gui = FALSE,
    corpus.dir        = corpus_dir,
    corpus.lang       = "Other",
    analyzed.features = "c",
    ngram.size        = ngram_size,
    mfw.min           = mfw_min,
    mfw.max           = mfw_max,
    culling.min       = 0, culling.max = 0,
    delete.pronouns   = FALSE,
    distance.measure  = "wurzburg",
    analysis.type     = "CA",
    display.on.screen = FALSE,
    write.png.file    = FALSE, write.pdf.file = FALSE,
    write.svg.file    = FALSE, write.jpg.file = FALSE,
    write.data.file   = FALSE
  )
  
  D <- res$distance.table
  labels_true <- author_from_fname(rownames(D))
  k <- length(unique(labels_true))
  n_docs <- nrow(D)
  
  # hclust ARI
  hc   <- hclust(as.dist(D), method = "ward.D2")
  cl_h <- cutree(hc, k = k)
  ari_h <- mclust::adjustedRandIndex(labels_true, cl_h)
  
  # k-means ARI (use richer MDS dims so it’s comparable)
  nd <- max(2, min(n_docs - 1, 50))
  X  <- cmdscale(as.dist(D), k = nd)
  set.seed(42)
  km <- kmeans(X, centers = k, nstart = 20)
  ari_k <- mclust::adjustedRandIndex(labels_true, km$cluster)
  
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
}
cat("Appended MFW sweep to:", out_csv, "\n")
