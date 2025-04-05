check_is_type <- function(
    x,
    is_type,
    expected_type,
    null_ok = FALSE,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
  ) {
  if ((is.null(x) && null_ok) || is_type(x)) {
    return(x)
  }
  cli::cli_abort(
    "{.arg {x_name}} must be {expected_type}, not {.obj_type_friendly(x)}.",
    class = "dassert_error_input_type",
    call = call
  )
}

check_is_class <- function(
    x,
    is_class,
    expected_class,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
  ) {
  if (is_class(x)) {
    return(x)
  }
  cli::cli_abort(
    "{.arg {x_name}} must be of class {.cls {expected_class}}, not {.cls {class(x)}}.",
    class = "dassert_error_input_type",
    call = call
  )
}
