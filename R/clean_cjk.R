clean_cjk <- function(s) {
  s <- stringi::stri_trans_nfkc(s)
  s <- stringi::stri_replace_all_regex(s, "[^\\p{Han}\\p{Z}\\n]", " ")
  s <- stringi::stri_replace_all_regex(s, "\\s+", " ")
  trimws(s)
}
