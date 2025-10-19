# scripts/03_make_fixed_chunks_exact.R
# Create exactly two 8000-character chunks per author (if possible).
# Cleaning removes whitespace/control chars first.
# Reports: number of authors and total tokens (characters w/o spaces).

pkgs <- c("fs","readr","dplyr","stringr")
invisible(lapply(pkgs, require, character.only = TRUE))

# ---- helpers ----
strip_ws <- function(x) gsub("[\\p{Z}\\p{Cc}]+","",x, perl=TRUE)

# ---- config ----
chunk_len <- 8000L
seed      <- 42L
in_dir <- readline("Path to your Tang texts (per-author subdirs or flat files)? ")
in_dir <- fs::path_expand(in_dir)
stopifnot(dir.exists(in_dir))

# ---- collect texts ----
files <- fs::dir_ls(in_dir, recurse = TRUE, type = "file", glob = "*.txt")

author_from_path <- function(p) {
  b <- basename(p)
  a <- sub("\\.txt$", "", b)
  a <- sub("_(\\d+)$", "", a)
  parent <- basename(dirname(p))
  if (parent != "" && parent != basename(in_dir)) return(parent)
  a
}

df <- tibble::tibble(file = files, author = vapply(files, author_from_path, ""))

by_auth <- df %>%
  group_by(author) %>%
  summarise(text = paste0(vapply(file, readr::read_file, ""), collapse = ""),
            .groups = "drop") %>%
  mutate(text = strip_ws(text),
         nchar = nchar(text))

eligible <- by_auth %>% filter(nchar >= 2L * chunk_len)

# ---- make chunks ----
mk_two_chunks <- function(txt, L) {
  n <- nchar(txt)
  if (n < 2L*L) return(NULL)
  start1 <- max(1L, floor((n/3) - L/2))
  start1 <- min(start1, n - 2L*L + 1L)
  start2 <- start1 + L
  c(substr(txt, start1, start1 + L - 1L),
    substr(txt, start2, start2 + L - 1L))
}

chunks <- lapply(seq_len(nrow(eligible)), function(i){
  a  <- eligible$author[i]
  tx <- eligible$text[i]
  ck <- mk_two_chunks(tx, chunk_len)
  if (is.null(ck)) return(NULL)
  tibble::tibble(author = a,
                 chunk_id = c(1L,2L),
                 text = ck)
})
chunks <- dplyr::bind_rows(chunks)

# ---- write output ----
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_base <- file.path("data","interim", sprintf("fixed_%d_balanced_exact_%s", chunk_len, stamp))
out_dir  <- file.path(out_base, "chunks")
fs::dir_create(out_dir)

apply(chunks, 1, function(r){
  a <- r[["author"]]; i <- r[["chunk_id"]]; t <- r[["text"]]
  fn <- file.path(out_dir, sprintf("%s_%d.txt", a, as.integer(i)))
  readr::write_file(t, fn)
})

meta <- chunks %>%
  mutate(nchar = nchar(text),
         file  = sprintf("%s_%d.txt", author, chunk_id)) %>%
  select(author, chunk_id, nchar, file)
readr::write_csv(meta, file.path(out_base, "meta.csv"))

# ---- report ----
n_authors <- length(unique(chunks$author))
total_tokens <- sum(chunks$nchar)
message("\n--- Balanced corpus summary ---",
        "\nAuthors: ", n_authors,
        "\nChunks: ", nrow(chunks),
        "\nTotal tokens (chars, no whitespace): ", total_tokens,
        "\nOutput written to: ", out_dir)
