#' @export
rule <- function(test, fail = NULL, pass = NULL) {
  rlang::check_required(test)
  check_is_class(test, is.logical, "logical")
  check_is_type(fail, is.character, "a character vector", null_ok = TRUE)
  check_is_type(pass, is.character, "a character vector", null_ok = TRUE)

  structure(
    test,
    fail = fail,
    pass = pass,
    class = c("dassert_rule", "logical")
  )
}

#' @export
print.dassert_rule <- function(x, ...) {
  cli::cat_line("<rule>")
  print(unclass(x))
  cli::cat_line()
  cli::cli_bullets(c(v = "pass", pass_message(x) %||% cli::col_grey("No Message")))
  cli::cli_bullets(c(x = "fail", fail_message(x) %||% cli::col_grey("No Message")))
}

#' @export
is_rule <- function(x) {
  inherits(x, "expect_rule")
}

pass_message <- function(x) {
  attr(x, "pass")
}

fail_message <- function(x) {
  attr(x, "fail")
}
