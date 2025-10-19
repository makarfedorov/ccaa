# Strip whitespace (including ideographic space U+3000) from all text files
# and save cleaned copies in a new directory tree.

pkgs <- c("fs","readr")
invisible(lapply(pkgs, require, character.only = TRUE))

# Helper to strip all Unicode whitespace + control chars
strip_ws <- function(x) {
  gsub("[\\p{Z}\\p{Cc}]+", "", x, perl = TRUE)
}

# Ask for input dir (your current samples)
in_dir <- readline("Path to your samples folder (e.g. data/interim/fixed_8000_balanced_2_xxxxx/chunks): ")
in_dir <- fs::path_expand(in_dir)
stopifnot(dir.exists(in_dir))

# Create parallel cleaned dir
parent <- dirname(in_dir)
clean_dir <- file.path(parent, paste0(basename(in_dir), "_clean"))
fs::dir_create(clean_dir)

files <- fs::dir_ls(in_dir, glob = "*.txt", type = "file")
cat("Cleaning", length(files), "files...\n")

for (f in files) {
  txt <- readr::read_file(f)
  txt_clean <- strip_ws(txt)
  out <- file.path(clean_dir, basename(f))
  readr::write_file(txt_clean, out)
}

cat("Done. Cleaned corpus in:\n", clean_dir, "\n")
