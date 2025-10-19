# R/clean_text.R
strip_ws <- function(x) {
  # remove ALL Unicode whitespace (incl. ideographic U+3000) and control chars
  gsub("[\\p{Z}\\p{Cc}]+", "", x, perl = TRUE)
}
