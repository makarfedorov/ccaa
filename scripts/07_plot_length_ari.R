# scripts/07_plot_length_ari.R
# Plot ARI vs length with error bars (mean ± 1 SD) for both methods.

pkgs <- c("readr","dplyr","ggplot2")
invisible(lapply(pkgs, require, character.only = TRUE))

summary_csv <- "reports/results/delta_ari_length_sweep_summary.csv"
if (!file.exists(summary_csv)) stop("Missing: ", summary_csv)

df <- readr::read_csv(summary_csv, show_col_types = FALSE)

# Friendlier method labels
df$method <- factor(df$method,
                    levels = c("ari_hclust","ari_kmeans"),
                    labels = c("Hierarchical (Ward.D2)","K-means (MDS)"))

p <- ggplot(df, aes(x = length, y = mean_ari, group = method)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = pmax(0, mean_ari - sd_ari),
                    ymax = pmin(1, mean_ari + sd_ari)),
                width = 0) +
  facet_wrap(~ method) +
  labs(title = "Adjusted Rand Index vs. Text Length (Delta distance)",
       x = "Characters per document",
       y = "Mean ARI (±1 SD)") +
  theme_minimal(base_size = 13)

print(p)

# Save PNG
ggsave("reports/results/delta_ari_length_sweep.png", p, width = 9, height = 5, dpi = 150)
cat("Saved plot to reports/results/delta_ari_length_sweep.png\n")
