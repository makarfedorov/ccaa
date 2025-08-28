# R/chunk_from_combined.R
# Split a long string into non-overlapping fixed-length character chunks

chunk_fixed <- function(text, size = 8000) {
  n <- nchar(text)
  if (n < size) return(character(0))
  starts <- seq(1, n - size + 1L, by = size)
  ends   <- starts + size - 1L
  mapply(substr, text, starts, ends, USE.NAMES = FALSE)
}

# Build per-author fixed-size chunks from a data.frame with columns: author, combined_text
build_author_chunks <- function(df, size = 8000) {
  stopifnot(all(c("author","combined_text") %in% names(df)))
  rows <- vector("list", nrow(df))
  idx  <- 0L
  for (i in seq_len(nrow(df))) {
    a   <- df$author[i]
    chs <- chunk_fixed(df$combined_text[i], size = size)
    if (length(chs) == 0) next
    tmp <- data.frame(
      author  = rep(a, length(chs)),
      chunk_id = seq_along(chs),
      text    = chs,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
    rows[[idx]] <- tmp
  }
  rows <- rows[seq_len(idx)]
  if (length(rows) == 0) return(data.frame(author=character(), chunk_id=integer(), text=character()))
  do.call(rbind, rows)
}
