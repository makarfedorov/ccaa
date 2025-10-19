# scripts/03_make_balanced_exact_chunks.R
# 1. Take author-level full-text files
# 2. Clean out all whitespace (so tokens = characters, no spaces/newlines)
# 3. Split into exactly 2 balanced ~8000-char chunks per author
# 4. Save to data/interim/fixed_8000_balanced_exact_TIMESTAMP/chunks

pkgs <- c("fs","readr","stringr","dplyr")
invisible(lapply(pkgs, require, character.only = TRUE))

# ---- input: one file per author ----
input_dir <- "data/interim/authors_combined_txt"
stopifnot(dir.exists(input_dir))

# ---- output base ----
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_base <- file.path("data","interim", paste0("fixed_8000_balanced_exact_", stamp))
out_dir  <- file.path(out_base, "chunks")
fs::dir_create(out_dir)

# ---- helper: clean and split into 2 balanced chunks ----
make_chunks <- function(author, path, chunk_size = 8000) {
  raw <- read_file(path)
  # remove *all* whitespace (spaces, tabs, newlines)
  txt <- str_remove_all(raw, "\\s+")
  n <- nchar(txt)
  
  if (n < chunk_size * 2) {
    warning("Author ", author, " has too few characters (", n, ") for 2x", chunk_size, " chunks")
    return(NULL)
  }
  
  # split into 2 balanced chunks around 8000 chars each
  half <- floor(n/2)
  part1 <- substr(txt, 1, half)
  part2 <- substr(txt, half+1, n)
  
  # re-balance if > 8000
  if (nchar(part1) > chunk_size) {
    part1 <- substr(part1, 1, chunk_size)
  }
  if (nchar(part2) > chunk_size) {
    part2 <- substr(part2, 1, chunk_size)
  }
  
  tibble(
    author = author,
    chunk  = c(1,2),
    text   = c(part1, part2),
    nchar  = c(nchar(part1), nchar(part2))
  )
}

# ---- process all authors ----
files <- fs::dir_ls(input_dir, glob = "*.txt")
res <- lapply(files, function(f) {
  author <- tools::file_path_sans_ext(basename(f))
  make_chunks(author, f)
})
res <- bind_rows(res)

# ---- save chunks ----
for (i in seq_len(nrow(res))) {
  row <- res[i, ]
  fn <- sprintf("%s_%d.txt", row$author, row$chunk)
  write_file(row$text, file.path(out_dir, fn))
}

# ---- save metadata ----
meta <- res %>% select(author, chunk, nchar)
write_csv(meta, file.path(out_base, "meta.csv"))

# ---- reporting ----
n_authors <- n_distinct(meta$author)
total_tokens <- sum(meta$nchar)

message("\n--- Balanced exact corpus created ---",
        "\nAuthors: ", n_authors,
        "\nChunks: ", nrow(meta),
        "\nTotal tokens (no whitespace): ", total_tokens,
        "\nOutput written to: ", out_dir)
