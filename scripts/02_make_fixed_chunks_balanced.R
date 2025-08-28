# scripts/02_make_fixed_chunks_balanced.R
# Build fixed-length (8,000-char) chunks per author, then sample exactly 2 per author.
# Writes files as: authorname_1.txt, authorname_2.txt

pkgs <- c("yaml","dplyr","readr","fs","stringi")
invisible(lapply(pkgs, require, character.only = TRUE))

source("R/chunk_from_combined.R")

# ---- settings ----
P <- yaml::read_yaml("config/params.yaml")

# Inputs produced by your combine-by-author step:
combined_csv     <- "data/interim/authors_combined.csv"
combined_txt_dir <- "data/interim/authors_combined_txt"

if (!file.exists(combined_csv)) stop("Missing: ", combined_csv)
if (!dir.exists(combined_txt_dir)) stop("Missing folder: ", combined_txt_dir)

# Target length & per-author quota
chunk_len  <- 8000
per_author <- 2
seed       <- 42

# ---- read author table & texts ----
comb <- readr::read_csv(combined_csv, show_col_types = FALSE)

sanitize <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "\\s+", "_")
  stringi::stri_replace_all_regex(x, "[^\\p{Han}_A-Za-z0-9]+", "")
}

comb$text_path <- file.path(combined_txt_dir, paste0(sanitize(comb$author), ".txt"))
missing <- comb$author[!file.exists(comb$text_path)]
if (length(missing) > 0) {
  warning("Missing TXT for: ", paste(missing, collapse = ", "))
}
comb$combined_text <- vapply(
  comb$text_path,
  function(p) if (file.exists(p)) readr::read_file(p) else NA_character_,
  FUN.VALUE = character(1)
)

comb <- comb %>% dplyr::filter(!is.na(combined_text), nchar(combined_text) > 0)

# ---- make fixed-length chunks per author ----
chunks <- build_author_chunks(
  df   = comb %>% dplyr::select(author, combined_text),
  size = chunk_len
)

if (nrow(chunks) == 0) stop("No chunks produced. Check chunk_len and inputs.")

# Count chunks per author
counts <- chunks %>% dplyr::count(author, name = "n_chunks") %>% dplyr::arrange(dplyr::desc(n_chunks))
message("Top authors by available chunks:\n"); print(utils::head(counts, 10))

# ---- keep only authors with at least 'per_author' chunks ----
eligible_authors <- counts %>% dplyr::filter(n_chunks >= per_author) %>% dplyr::pull(author)
if (length(eligible_authors) == 0) stop("No authors have >= ", per_author, " chunks at length ", chunk_len)

chunks_eligible <- chunks %>% dplyr::filter(author %in% eligible_authors)

# ---- balanced sampling: exactly 'per_author' chunks per author ----
set.seed(seed)
sampled <- chunks_eligible %>%
  dplyr::group_by(author) %>%
  dplyr::slice_sample(n = per_author) %>%
  dplyr::ungroup()

# ---- write sampled chunks with names: authorname_1.txt, authorname_2.txt ----
stamp         <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_parent    <- file.path("data", "interim", paste0("fixed_", chunk_len, "_balanced_", per_author, "_", stamp))
out_chunksdir <- file.path(out_parent, "chunks")
fs::dir_create(out_chunksdir)

# order files per author to assign suffixes 1..per_author
sampled <- sampled %>%
  dplyr::group_by(author) %>%
  dplyr::arrange(author) %>%
  dplyr::mutate(sample_index = dplyr::row_number()) %>%
  dplyr::ungroup()

# build filenames: authorname_1.txt, authorname_2.txt
file_names <- sprintf("%s_%d.txt", sanitize(sampled$author), sampled$sample_index)
dest_paths <- file.path(out_chunksdir, file_names)

# write files (base loop; no purrr needed)
for (i in seq_len(nrow(sampled))) {
  readr::write_file(sampled$text[i], dest_paths[i])
}

# manifest
manifest <- dplyr::tibble(
  author       = sampled$author,
  sample_index = sampled$sample_index,
  filename     = file_names,
  path         = dest_paths,
  chars        = nchar(sampled$text)
)

readr::write_csv(manifest, file.path(out_parent, "manifest.csv"))

cat("\nBalanced fixed-length set written to:\n  ", out_parent, "\n")
cat("Chunks folder:\n  ", out_chunksdir, "\n\n")
print(table(manifest$author))
