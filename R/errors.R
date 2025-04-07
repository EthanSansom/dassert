# check ------------------------------------------------------------------------

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
  stop_input_type(
    paste0(
      or_null("{.arg {x_name}} must be {expected_type}", null_ok),
      ", not {.obj_type_friendly(x)}."
    ),
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
  stop_input_type(
    "{.arg {x_name}} must be of class {.cls {expected_class}}, not {.cls {class(x)}}.",
    call = call
  )
}

check_unsupplied_null <- function(
  x,
  x_name = rlang::caller_arg(x),
  call = rlang::caller_env()
  ) {
  if (is.null(x)) {
    return(x)
  }
  stop_input_type(
    "{.arg {x_name}} must not be supplied.",
    call = call
  )
}

# stop -------------------------------------------------------------------------

stop_input_type <- function(message, call = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = "dassert_error_input_type",
    call = call,
    .envir = rlang::caller_env()
  )
}

stop_input_incompatible <- function(message, call = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = "dassert_error_input_incompatible",
    call = call,
    .envir = rlang::caller_env()
  )
}

stop_internal <- function(message, call = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = "dassert_error_internal",
    call = call,
    .envir = rlang::caller_env(),
    .internal = TRUE
  )
}

# helpers ----------------------------------------------------------------------


