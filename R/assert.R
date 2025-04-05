assert <- function(
    .data, ..., .by = NULL, .error_header = NULL, .error_call = rlang::caller_env(),
    # These just absorb other arguments which could be supplied to `mutate`
    .keep = NULL, .before = NULL, .after = NULL
) {

  stopifnot(is.data.frame(.data))
  stopifnot(is.null(.keep), is.null(.before), is.null(.after))

  # The `.keep = "none"` is key here
  # - capture outgoing errors from `dplyr::mutate()` and make a chained error
  results <- dplyr::mutate(.data, ..., .by = .by, .keep = "none")

  stopifnot(vapply(results, is.logical, logical(1L)))

  messages <- character()
  colnames <- names(results)
  env <- rlang::env_clone(rlang::caller_env())

  for (i in seq_along(results)) {
    col <- results[[i]]

    if (!anyNA(col) && all(col)) {
      next
    }

    env$vals <- function() { col }

    if (is_rule(col)) {
      message <- map_chr(
        .x = fail_message(col),
        .f = format_inline_tidy,
        data = .data,
        env = env,
        .nms = TRUE
      )
    } else {
      message <- c(x = format_inline_tidy(colnames[[i]], data = .data, env = env))
    }
    messages <- c(messages, message)
  }

  if (length(messages)) {
    header <- .error_header %||% "Dataframe assertions failed."
    cli::cli_abort(
      c(header, messages),
      class = "expect_assertion_error",
      call = .error_call
    )
  }

  # TODO:
  # We'll also want to handle message revisions from `.by`. My vote is to show
  # the message in the first failing group, report that group (e.g. `x, y, z`),
  # and report the number of other groups which failed.

  # Return the original data
  .data
}

format_inline_tidy <- function(x, env, data) {
  rlang::eval_tidy(
    expr = rlang::expr(cli::format_inline(!!x)),
    data = data,
    env = env
  )
}
