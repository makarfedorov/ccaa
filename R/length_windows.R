# R/length_windows.R
# Given text and a target length, pick one contiguous window at a random start.

random_window <- function(text, target_len, seed = NULL) {
  n <- nchar(text)
  if (n < target_len) return(NA_character_)
  if (!is.null(seed)) set.seed(seed)
  if (n == target_len) return(text)
  start <- sample.int(n - target_len + 1L, 1L)
  substr(text, start, start + target_len - 1L)
}
