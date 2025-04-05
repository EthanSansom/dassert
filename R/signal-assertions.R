# assert and alert -------------------------------------------------------------

# TODO: Introduce some kind of error message length limiting behavior
signal_assertion_failures <- function(
    .data,
    ...,
    .by = NULL,
    .header = NULL,
    .call = rlang::caller_env(),
    .type = c("assert", "alert")
) {

  ungrouped <- is.null(rlang::quo_get_expr(.by))
  if (ungrouped) {
    results <- handle_mutate_errors(dplyr::mutate(.data, ..., .keep = "none"))
  } else {
    results <- handle_mutate_errors(dplyr::mutate(.data, ..., .by = {{ .by }}, .keep = "none"))
    results <- dplyr::select(results, -{{ .by }}) # `mutate()` keeps columns in `.by`
  }

  assertions_passed_at <- map_lgl(results, is_assertion_pass)
  if (all(assertions_passed_at)) {
    return(.data)
  }

  n_fails <- sum(!assertions_passed_at)
  n_results <- ncol(results)
  results <- results[!assertions_passed_at]
  env <- rlang::env_clone(rlang::caller_env())

  if (ungrouped) {
    header <- .header %||% ungrouped_failure_header(n_fails, n_results)
    bullets <- ungrouped_fail_bullets(results, data = .data, env = env)
  } else {
    header <- .header %||% grouped_failure_header(n_fails, n_results)
    bullets <- grouped_fail_bullets(
      results,
      group_ids = dplyr::mutate(.data, id = dplyr::cur_group_id(), .by = {{ .by }})[["id"]],
      data = .data,
      env = env
    )
  }

  signal <- switch(.type, assert = cli::cli_abort, alert = cli::cli_warn)
  class <- switch(
    .type,
    assert = c("dassert_assertion_error", if (!ungrouped) "dassert_grouped_assertion_error"),
    alert = c("dassert_assertion_warning", if (!ungrouped) "dassert_grouped_assertion_warning")
  )
  signal(
    c(header, bullets),
    class = class,
    call = .call
  )
}

# inspect ----------------------------------------------------------------------

# TODO: Introduce some kind of error message length limiting behaviour
signal_assertion_results <- function(
    .data,
    ...,
    .by = NULL,
    .header = NULL,
    .call = rlang::caller_env()
) {

  ungrouped <- is.null(rlang::quo_get_expr(.by))
  if (ungrouped) {
    results <- handle_mutate_errors(dplyr::mutate(.data, ..., .keep = "none"))
  } else {
    results <- handle_mutate_errors(dplyr::mutate(.data, ..., .by = {{ .by }}, .keep = "none"))
    results <- dplyr::select(results, -{{ .by }}) # `mutate()` keeps columns in `.by`
  }

  assertions_passed_at <- map_lgl(results, is_assertion_pass)
  n_fails <- sum(!assertions_passed_at)
  n_results <- ncol(results)
  env <- rlang::env_clone(rlang::caller_env())

  if (ungrouped) {
    header <- .header %||% ungrouped_results_header(n_fails, n_results)
    fail_bullets <- ungrouped_fail_bullets(
      results = results[!assertions_passed_at],
      data = .data,
      env = env
    )
    pass_bullets <- ungrouped_pass_bullets(
      results = results[assertions_passed_at],
      data = .data,
      env = env
    )
  } else {
    group_ids <- dplyr::mutate(.data, id = dplyr::cur_group_id(), .by = {{ .by }})[["id"]]
    header <- .header %||% grouped_results_header(n_fails, n_results)
    fail_bullets <- grouped_fail_bullets(
      results = results[!assertions_passed_at],
      group_ids = group_ids,
      data = .data,
      env = env
    )
    pass_bullets <- grouped_pass_bullets(
      results = results[assertions_passed_at],
      group_ids = group_ids,
      data = .data,
      env = env
    )
  }

  cli::cli_inform(
    c(header, fail_bullets, pass_bullets),
    class = c("dassert_assertion_message", if (!ungrouped) "dassert_grouped_assertion_message"),
    call = .call
  )
  .data
}

# helpers grouped --------------------------------------------------------------

# TODO: We'll want to implement a maximum message length and stop generating if we reach it!
grouped_pass_bullets <- function(results, group_ids, data, env) {
  bullets <- character()
  results_names <- names(results)
  for (i in seq_along(results)) {
    bullet <- grouped_pass_bullet(
      result = results[[i]],
      result_name = results_names[[i]],
      group_ids = group_ids,
      data = data,
      env = env
    )
    bullets <- c(bullets, bullet)
  }
  bullets
}

grouped_pass_bullet <- function(result, result_name, group_ids, data, env) {
  first_group_id <- group_ids[[1]]
  first_group_at <- group_ids == first_group_id
  first_result <- result[first_group_at, ]
  first_data <- data[first_group_at, ]

  bullet_header <- grouped_bullet_header(
    group_id = first_group_id,
    n_groups = dplyr::n_distinct(group_ids)
  )
  bullet <- result_bullet(
    result = first_result,
    result_name = result_name,
    data = first_data,
    env = env,
    type = "pass"
  )
  c(i = bullet_header, bullet)
}

# TODO: We'll want to implement a maximum message length and stop generating if we reach it!
grouped_fail_bullets <- function(results, group_ids, data, env) {
  bullets <- character()
  results_names <- names(results)
  for (i in seq_along(results)) {
    bullet <- grouped_fail_bullet(
      result = results[[i]],
      result_name = results_names[[i]],
      group_ids = group_ids,
      data = data,
      env = env
    )
    bullets <- c(bullets, bullet)
  }
  bullets
}

grouped_fail_bullet <- function(result, result_name, group_ids, data, env) {

  result_split <- vctrs::vec_split(result, group_ids)
  fail_groups_at <- map_lgl(result_split$val, is_assertion_fail)

  # We're only going to generate a nice error message for the first group
  # which caused a failure.
  first_fail_group_at <- which.max(fail_groups_at)
  fail_group_id <- result_split$key[[first_fail_group_at]]
  fail_result <- result_split$val[[first_fail_group_at]]
  fail_data <- data[group_ids == fail_group_id, ]

  bullet_header <- grouped_bullet_header(
    group_id = fail_group_id,
    n_groups = sum(fail_groups_at)
  )
  bullet <- result_bullet(
    result = fail_result,
    result_name = result_name,
    data = fail_data,
    env = env,
    type = "fail"
  )
  c(i = bullet_header, bullet)
}

# helpers ungrouped ------------------------------------------------------------

# TODO: We'll want to implement a maximum message length and stop generating if we reach it!
ungrouped_fail_bullets <- function(results, data, env) {
  ungrouped_bullets(results, data = data, env = env, type = "fail")
}

ungrouped_pass_bullets <- function(results, data, env) {
  ungrouped_bullets(results, data = data, env = env, type = "pass")
}

ungrouped_bullets <- function(results, data, env, type = c("pass", "fail")) {
  bullets <- character()
  results_names <- names(results)
  for (i in seq_along(results)) {
    bullet <- result_bullet(
      result = results[[i]],
      result_name = results_names[[i]],
      data = data,
      env = env,
      type = type
    )
    bullets <- c(bullets, bullet)
  }
  bullets
}

# helpers ----------------------------------------------------------------------

# TODO: Actually handle the errors
handle_mutate_errors <- function(expr) {
  expr
}

grouped_bullet_header <- function(group_id, n_groups) {
  n_other_groups <- n_groups - 1
  paste0(
    "In group: `dplyr::cur_group_id() == ", group_id, "`",
    if (n_other_groups) {
      paste0(" (and ", n_other_groups, " other group", "s"[n_other_groups > 1], ")")
    }
  )
}

grouped_results_header <- function(n_fails, n_results) {
  if (n_fails == 0) {
    c(v = paste0("Passed ", n_results, "/", n_results, " grouped dataframe assertions."))
  } else if (n_fails == n_results) {
    c(x = paste0("Failed ", n_results, "/", n_results, " grouped dataframe assertions."))
  } else {
    c(i = paste0("Passed ", n_results - n_fails, " and failed ", n_fails, " grouped dataframe assertions."))
  }
}

ungrouped_results_header <- function(n_fails, n_results) {
  if (n_fails == 0) {
    c(v = paste0("Passed ", n_results, "/", n_results, " dataframe assertions."))
  } else if (n_fails == n_results) {
    c(x = paste0("Failed ", n_results, "/", n_results, " dataframe assertions."))
  } else {
    c(
      i = paste0(
        "Passed ", n_results - n_fails, "/", n_results,
        " and failed ", n_fails, "/", n_results, " dataframe assertions."
      )
    )
  }
}

grouped_failure_header <- function(n_fails, n_results) {
  paste0("Failed ", n_fails, "/", n_results, " grouped dataframe assertions.")
}

ungrouped_failure_header <- function(n_fails, n_results) {
  paste0("Failed ", n_fails, "/", n_results, " dataframe assertions.")
}

# TODO: In internal documentation, `env` MUST be an environment that you own,
#       since we're modifying it.
result_bullet <- function(result, result_name, data, env, type = c("fail", "pass")) {
  rlang::env_bind(
    env,
    result = function() { result },
    true_at = function() { which(!is.na(result) & result) },
    false_at = function() { which(!is.na(result) & !result) },
    na_at = function() { which(is.na(result)) },
    pass_at = function() { which(!is.na(result) & result) },
    fail_at = function() { which(is.na(result) | !result) }
  )

  if (is_rule(result)) {
    contents <- switch(
      type,
      fail = fail_message(result),
      pass = pass_message(result)
    )
    map_chr(
      .x = contents,
      .f = format_inline_tidy,
      data = data,
      env = env,
      .nms = TRUE
    )
  } else {
    switch (
      type,
      fail = fail_bullet(format_inline_tidy(result_name, data = data, env = env)),
      pass = pass_bullet(format_inline_tidy(result_name, data = data, env = env))
    )
  }
}

format_inline_tidy <- function(x, env, data) {
  rlang::eval_tidy(
    expr = rlang::expr(cli::format_inline(!!x)),
    data = data,
    env = env
  )
}

fail_bullet <- function(x) {
  rlang::set_names(x, "x")
}

pass_bullet <- function(x) {
  rlang::set_names(x, "v")
}
