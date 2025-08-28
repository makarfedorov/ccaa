author_from_fname <- function(x) {
  b <- basename(x)
  b <- sub("\\.txt$", "", b, ignore.case = TRUE)  # drop extension if present
  sub("_[0-9]+$", "", b)                          # strip trailing _<digits>
}