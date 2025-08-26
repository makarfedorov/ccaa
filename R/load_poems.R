`%||%` <- function(a,b) if (is.null(a)) b else a
normalize_record <- function(rec, def_dynasty = "唐") {
  if (!is.list(rec)) return(NULL)
  title   <- rec$title   %||% rec$篇名   %||% rec$题名   %||% NA_character_
  author  <- rec$author  %||% rec$作者  %||% NA_character_
  dynasty <- rec$dynasty %||% rec$朝代  %||% def_dynasty
  text    <- rec$content %||% rec$正文  %||% rec$paragraphs %||% rec$段落
  if (is.list(text) || is.vector(text)) text <- paste(unlist(text), collapse = "\n")
  if (is.null(text)) text <- NA_character_
  tibble::tibble(title, author, dynasty, text)
}
read_poetry_dir <- function(path_vec) {
  files <- fs::dir_ls(path_vec, recurse = TRUE, regexp = "\\.json$")
  purrr::map_dfr(files, function(f) {
    x <- try(jsonlite::fromJSON(f, simplifyVector = FALSE), silent = TRUE)
    if (inherits(x, "try-error") || is.null(x)) return(tibble::tibble())
    if (is.list(x) && length(x) > 0 && is.list(x[[1]])) {
      purrr::map_dfr(x, normalize_record)
    } else {
      dplyr::bind_rows(normalize_record(x))
    }
  }) |>
    dplyr::distinct(title, author, text, .keep_all = TRUE) |>
    dplyr::filter(!is.na(text), !is.na(author), nchar(text) > 0)
}
