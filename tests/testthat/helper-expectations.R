expect_input_type_error <- function(object) {
  expect_error(object, class = "dassert_error_input_type")
}

expect_input_incompatible_error <- function(object) {
  expect_error(object, class = "dassert_error_input_incompatible")
}

expect_assertion_error <- function(object, regexp = NULL, ...) {
  expect_error(
    object,
    regexp = regexp,
    class = "dassert_error_assertion_fail",
    ...,
    inherit = TRUE,
    info = NULL,
    label = NULL
  )
}

expect_grouped_assertion_error <- function(object, regexp = NULL, ...) {
  expect_error(
    object,
    regexp = regexp,
    class = "dassert_error_grouped_assertion_fail",
    ...,
    inherit = TRUE,
    info = NULL,
    label = NULL
  )
}
