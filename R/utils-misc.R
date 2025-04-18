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

is_unnamed <- function(x) {
  all(rlang::names2(x) == "")
}

# `dplyr:::is_rowwise_df()` is internal, use the exported version if it is added
is_rowwise_df <- function(x) {
  inherits(x, "rowwise_df")
}
