#' @export
rule <- function(test, fail = NULL, pass = NULL) {
  rlang::check_required(test)
  check_is_class(test, is.logical, "logical")
  check_is_type(fail, is.character, "a character vector", null_ok = TRUE)
  check_is_type(pass, is.character, "a character vector", null_ok = TRUE)

  if (!is.null(fail) && is_unnamed(fail)) fail <- fail_bullet(fail)
  if (!is.null(pass) && is_unnamed(pass)) pass <- pass_bullet(pass)

  structure(
    if (is_rule(test)) unclass(test) else test,
    fail = fail,
    pass = pass,
    class = c("dassert_rule", "logical")
  )
}

#' @export
is_rule <- function(x) {
  inherits(x, "expect_rule")
}

#' @export
print.dassert_rule <- function(x, ...) {
  cli::cat_line("<rule>")
  print(as.logical(x))
  cli::cat_line()
  cli::cli_bullets_raw(
    c(
      "pass:", pass_message(x) %||% cli::col_grey("No Message"),
      "fail:", fail_message(x) %||% cli::col_grey("No Message")
    )
  )
}

#' @export
`[.dassert_rule` <- function(x, i) {
  rule(NextMethod(), fail = fail_message(x), pass = pass_message(x))
}

pass_message <- function(x) {
  attr(x, "pass")
}

fail_message <- function(x) {
  attr(x, "fail")
}
