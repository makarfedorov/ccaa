# scripts/01_combine_by_author.R
# Combine all texts per author, count characters, and sort

pkgs <- c("yaml","dplyr","readr","fs","stringi")
invisible(lapply(pkgs, require, character.only = TRUE))

# Use your existing helpers
source("R/clean_cjk.R")
source("R/load_poems.R")

# 1) Load config
P <- yaml::read_yaml("config/params.yaml")
tang_path <- P$paths$tang_traditional

# 2) Read poems -> clean -> keep non-empty
poems <- read_poetry_dir(tang_path) %>%
  mutate(
    title = trimws(title),
    author = trimws(author),
    text_clean = clean_cjk(text)
  ) %>%
  filter(!is.na(author), nchar(text_clean) > 0)

# 3) Combine all poems per author into one big text
authors_combined <- poems %>%
  group_by(author) %>%
  summarise(
    n_poems = n(),
    combined_text = paste(text_clean, collapse = "\n"),
    .groups = "drop"
  ) %>%
  mutate(char_count = nchar(combined_text)) %>%
  arrange(desc(char_count))

# 4) Write a CSV summary
fs::dir_create("data/interim")
out_csv <- "data/interim/authors_combined.csv"
readr::write_csv(authors_combined %>% select(author, n_poems, char_count), out_csv)

# 5) (Optional) also write one TXT per author (handy later)
out_txt_dir <- "data/interim/authors_combined_txt"
fs::dir_create(out_txt_dir)

sanitize <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "\\s+", "_")
  stringi::stri_replace_all_regex(x, "[^\\p{Han}_A-Za-z0-9]+", "")
}

invisible(purrr::pwalk(
  list(authors_combined$author, authors_combined$combined_text),
  function(a, txt) {
    fname <- file.path(out_txt_dir, paste0(sanitize(a), ".txt"))
    readr::write_file(txt, fname)
  }
))

# 6) Quick console preview
print(authors_combined %>% select(author, n_poems, char_count) %>% slice_head(n = 15))
cat("\nWrote summary:", out_csv, "\n")
cat("Wrote per-author TXT files to:", out_txt_dir, "\n")
