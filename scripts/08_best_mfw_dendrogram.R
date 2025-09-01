# scripts/08_best_mfw_dendrogram.R
# Save a colored dendrogram at the best MFW
# + produce robust misassignment reports

pkgs <- c("yaml","stylo","fs","readr","dplyr","mclust")
invisible(lapply(pkgs, require, character.only = TRUE))

`%||%` <- function(a,b) if (is.null(a)) b else a

# helper: parse authors from filenames like "作者名_2.txt"
source("R/labels_from_filenames.R")  # defines author_from_fname()

P <- yaml::read_yaml("config/params.yaml")

# ---------- Load sweep results ----------
results_csv <- "reports/results/stylo_ari_mfw_sweep.csv"
if (!file.exists(results_csv)) {
  stop("Missing ", results_csv, " — run scripts/05b_mfw_sweep.R first.")
}
res <- readr::read_csv(results_csv, show_col_types = FALSE)

best <- res %>%
  arrange(desc(ari_hclust), desc(mfw_max), desc(timestamp)) %>%
  slice(1)

cat("Best sweep row:\n"); print(best)

corpus_dir <- best$corpus[1]
if (!dir.exists(corpus_dir)) stop("Corpus not found: ", corpus_dir)

mfw_best <- suppressWarnings(as.integer(best$mfw_max[1]))
if (!is.finite(mfw_best) || is.na(mfw_best) || mfw_best < 2) {
  stop("Bad mfw_best: ", best$mfw_max[1])
}

ngram_size <- P$stylo$ngram_size %||% 1

# ---------- Run stylo ----------
res2 <- stylo(
  gui = FALSE,
  corpus.dir        = corpus_dir,
  corpus.lang       = "Other",
  analyzed.features = "c",
  ngram.size        = ngram_size,
  mfw.min           = mfw_best,
  mfw.max           = mfw_best,
  culling.min       = 0,
  culling.max       = 0,
  delete.pronouns   = FALSE,
  distance          = "delta",
  analysis.type     = "CA",
  display.on.screen = FALSE,
  write.png.file    = FALSE,
  write.pdf.file    = FALSE
)

D <- res2$distance.table
hc <- res2$hc
if (is.null(D) || !is.matrix(D) || any(!is.finite(D))) {
  stop("Distance table unusable (NULL or non-finite).")
}
if (is.null(hc)) {
  hc <- hclust(as.dist(D), method = "ward.D2")
  cat("Rebuilt hclust from distance.table.\n")
}

# ---------- Colored dendrogram ----------
if (!requireNamespace("dendextend", quietly = TRUE)) {
  stop("Package 'dendextend' is required. install.packages('dendextend')")
}
library(dendextend)

labs     <- rownames(D)
authors  <- author_from_fname(labs)
grp      <- factor(authors)

if (!requireNamespace("scales", quietly = TRUE)) {
  stop("Package 'scales' is required. install.packages('scales')")
}
pal <- scales::hue_pal(l = 70, c = 90)(nlevels(grp))
col_by_author <- setNames(pal, levels(grp))

dend <- as.dendrogram(hc)
ord <- order.dendrogram(dend)
labels_colors(dend) <- col_by_author[ grp[ord] ]
labels(dend) <- sub("\\.txt$", "", labs)[ord]
dend <- set(dend, "labels_cex", 0.55)
dend <- set(dend, "hang", -0.01)
dend <- set(dend, "branches_lwd", 1.1)

fs::dir_create("reports/figures")
out_png <- sprintf("reports/figures/dendrogram_best_mfw_%d.png", mfw_best)
png(out_png, width = 1600, height = 1200, res = 140)
op <- par(mar = c(4, 14, 5, 2))
plot(dend, horiz = TRUE,
     main = sprintf("ccaa — Cluster Analysis (best MFW=%d)", mfw_best),
     xlab = "Classic Delta distance", sub = "")
par(op)
dev.off()
cat("Saved colored dendrogram:", out_png, "\n")

key <- data.frame(author = levels(grp),
                  color = unname(col_by_author),
                  stringsAsFactors = FALSE)
key_csv <- "reports/figures/dendrogram_best_mfw_colors.csv"
readr::write_csv(key, key_csv)

# ---------- Misassignment check ----------
labels_true <- authors
k <- length(unique(labels_true))
pred <- cutree(hc, k = k)

# per-doc assignments
assignments <- tibble(
  doc     = names(pred),
  author  = labels_true,
  cluster = pred
)

# majority author per cluster
majority <- assignments %>%
  count(cluster, author, name = "n") %>%
  group_by(cluster) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(maj_author = author)

# mark correct vs wrong (force no NA)
assessed <- assignments %>%
  left_join(majority, by = "cluster") %>%
  mutate(correct = (author == maj_author)) %>%
  mutate(correct = ifelse(is.na(correct), FALSE, correct))

stopifnot(!any(is.na(assessed$correct)))  # sanity check

# robust per-author summary
summary <- assessed %>%
  group_by(author) %>%
  summarise(
    total   = n(),
    correct = sum(correct),
    wrong   = n() - sum(correct),
    .groups = "drop"
  ) %>%
  arrange(desc(wrong))

n_wrong <- sum(summary$wrong)
cat(sprintf("Misassigned texts: %d / %d (%.1f%% correct)\n",
            n_wrong, sum(summary$total),
            100 * (1 - n_wrong / sum(summary$total))))

fs::dir_create("reports/results")
out_docs <- "reports/results/hclust_attribution_check.csv"
out_sum  <- "reports/results/hclust_attribution_summary.csv"

readr::write_csv(assessed, out_docs)
cat("Saved per-document attribution report:", out_docs, "\n")

readr::write_csv(summary, out_sum)
cat("Saved per-author summary:", out_sum, "\n")

# ARI
ari_h <- mclust::adjustedRandIndex(labels_true, pred)
cat(sprintf("ARI(hclust) at best MFW=%d: %.3f\n", mfw_best, ari_h))
