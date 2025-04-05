`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

map_chr <- function(.x, .f, ..., .nms = FALSE) {
  vapply(.x, .f, character(1L), ..., USE.NAMES = .nms)
}
