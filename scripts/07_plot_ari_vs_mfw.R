pkgs <- c("readr","dplyr","ggplot2")
invisible(lapply(pkgs, require, character.only = TRUE))

results_csv <- "reports/results/stylo_ari_mfw_sweep.csv"
stopifnot(file.exists(results_csv))
res <- readr::read_csv(results_csv, show_col_types = FALSE)

p <- res %>%
  tidyr::pivot_longer(c(ari_hclust, ari_kmeans), names_to="method", values_to="ari") %>%
  ggplot(aes(x = mfw_max, y = ari, color = method)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, method = "loess") +
  labs(
    x = "Most Frequent Words (MFW)",
    y = "Adjusted Rand Index (ARI)",
    color = "Method",
    title = "ARI vs. MFW"
  ) +
  theme_minimal(base_size = 12)

out_png <- "reports/results/ari_vs_mfw.png"
ggsave(out_png, p, width = 7, height = 5, dpi = 150)
cat("Saved plot:", out_png, "\n")
