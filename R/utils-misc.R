`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

map_chr <- function(.x, .f, ..., .nms = FALSE) {
  vapply(.x, .f, character(1L), ..., USE.NAMES = .nms)
}

map_lgl <- function(.x, .f, ..., .nms = FALSE) {
  vapply(.x, .f, logical(1L), ..., USE.NAMES = .nms)
}

is_assertion_fail <- function(x) {
  anyNA(x) || !all(x)
}

is_assertion_pass <- function(x) {
  !anyNA(x) && all(x)
}
