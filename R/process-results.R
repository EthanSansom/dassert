# assert and alert -------------------------------------------------------------

signal_assertion_failures <- function(
    .data,
    ...,
    .by = NULL,
    .header = NULL,
    .call = rlang::caller_env(),
    .type = c("assert", "alert")
) {

  ungrouped <- is.null(substitute(.by))
  if (ungrouped) {
    results <- handle_mutate_errors(dplyr::mutate(.data, ..., .keep = "none"))
  } else {
    results <- handle_mutate_errors(dplyr::mutate(.data, ..., .by = {{ .by }}, .keep = "none"))
    results <- dplyr::select(results, -{{ .by }}) # `mutate()` keeps columns in `.by`
  }

  if (all(map_lgl(results, is_assertion_pass))) {
    return(.data)
  }

  env <- rlang::env_clone(rlang::caller_env())
  if (ungrouped) {
    message <- ungrouped_failure_message(results, data = .data, env = env)
  } else {
    group_ids <- dplyr::mutate(.data, id = dplyr::cur_group_id(), .by = {{ .by }})[["id"]]
    message <- grouped_failure_message(results, group_ids, data = .data, env = env)
  }

  signal <- switch(
    .type,
    assert = cli::cli_abort,
    alert = cli::cli_warn
  )
  class <- switch(
    .type,
    assert = c("dassert_assertion_error", if (!ungrouped) "dassert_grouped_assertion_error"),
    alert = c("dassert_assertion_warning", if (!ungrouped) "dassert_grouped_assertion_warning")
  )
  signal(
    c(.header %||% message$header, message$bullets),
    class = class,
    call = .call
  )
}

# TODO: We'll want to implement a maximum message length and stop generating if
# we reach it!
grouped_failure_message <- function(results, group_ids, data, env) {
  # TODO: This should be `bullets`
  messages <- character()
  n_failures <- 0L
  n_results <- ncol(results)
  for (i in seq_along(results)) {
    result <- results[[i]]
    if (is_assertion_pass(result)) {
      next
    }
    n_failures <- n_failures + 1L

    result_split <- vctrs::vec_split(result, group_ids)
    fail_at <- map_lgl(result_split$val, is_assertion_fail)
    n_fails <- sum(fail_at)

    # We're only going to generate a nice error message for the first group
    # which caused a failure.
    first_fail <- which.max(fail_at)
    fail_group_id <- result_split$key[[first_fail]]
    fail_result <- result_split$val[[first_fail]]
    fail_data <- data[group_ids == fail_group_id, ]

    # Splitting a <rule> `result` un-classes it to logical. So, we re-class the
    # `fail_result` here, which is faster than using a split-method for <rule>.
    if (is_rule(result)) {
      fail_result <- rule(
        fail_result,
        fail = fail_message(result),
        pass = pass_message(result)
      )
    }

    message <- result_bullet(
      result = fail_result,
      result_name = names(results)[[i]],
      data = fail_data,
      env = env,
      type = "fail"
    )
    group_header <- paste0(
      "In group: `dplyr::cur_group_id() == ", fail_group_id, "`",
      if (n_fails - 1) paste0(" (and ", n_fails - 1, " other group", "s"[n_fails > 2], ")")
    )
    messages <- c(messages, c(i = group_header, message))
  }

  list(
    header = paste0("Failed ", n_failures, "/", n_results, " grouped dataframe assertions."),
    bullets = messages
  )
}

# TODO: Implement this within `grouped_failure_message()`
# TODO: Need to pass in the result name!
group_failure_bullet <- function(result, result_name, group_ids, data, env) {

  result_split <- vctrs::vec_split(result, group_ids)
  fail_groups_at <- map_lgl(result_split$val, is_assertion_fail)

  # We're only going to generate a nice error message for the first group
  # which caused a failure.
  first_fail_group_at <- which.max(fail_groups_at)
  fail_group_id <- result_split$key[[first_fail_group_at]]
  fail_result <- result_split$val[[first_fail_group_at]]
  fail_data <- data[group_ids == fail_group_id, ]

  # Splitting a <rule> `result` un-classes it to logical. So, we re-class the
  # `fail_result` here, which is faster than using a split-method for <rule>.
  if (is_rule(result)) {
    fail_result <- rule(
      fail_result,
      fail = fail_message(result),
      pass = pass_message(result)
    )
  }

  bullet_header <- grouped_bullet_header(
    group_id = fail_group_id,
    n_groups = sum(fail_at)
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

# TODO: We'll want to implement a maximum message length and stop generating if
# we reach it!
ungrouped_failure_message <- function(results, data, env) {
  messages <- character()
  n_failures <- 0L
  n_results <- ncol(results)
  for (i in seq_along(results)) {
    result <- results[[i]]
    if (is_assertion_pass(result)) {
      next
    }
    n_failures <- n_failures + 1L
    message <- result_bullet(
      result = result,
      result_name = names(results)[[i]],
      data = data,
      env = env,
      type = "fail"
    )
    messages <- c(messages, message)
  }
  list(
    header = paste0("Failed ", n_failures, "/", n_results, " dataframe assertions."),
    bullets = messages
  )
}

# inspect ----------------------------------------------------------------------

signal_assertion_results <- function(
    .data,
    ...,
    .by = NULL,
    .header = NULL,
    .call = rlang::caller_env(),
    .type = c("assert", "alert")
) {

  ungrouped <- is.null(substitute(.by))
  if (ungrouped) {
    results <- handle_mutate_errors(dplyr::mutate(.data, ..., .keep = "none"))
  } else {
    results <- handle_mutate_errors(dplyr::mutate(.data, ..., .by = {{ .by }}, .keep = "none"))
    results <- dplyr::select(results, -{{ .by }}) # `mutate()` keeps columns in `.by`
  }

  env <- rlang::env_clone(rlang::caller_env())
  if (ungrouped) {
    message <- ungrouped_results_message(results, data = .data, env = env)
  } else {
    group_ids <- dplyr::mutate(.data, id = dplyr::cur_group_id(), .by = {{ .by }})[["id"]]
    message <- grouped_results_message(results, group_ids, data = .data, env = env)
  }

  cli::cli_inform(
    c(.header %||% message$header, message$bullets),
    class = c("dassert_assertion_message", if (!ungrouped) "dassert_grouped_assertion_message"),
    call = .call
  )
}

grouped_results_message <- function(results, group_ids, data, env) {
  messages <- character()
  n_failures <- 0L
  n_results <- ncol(results)
  for (i in seq_along(results)) {
    result <- results[[i]]
    if (is_assertion_pass(result)) {
      next
    }
    n_failures <- n_failures + 1L

    result_split <- vctrs::vec_split(result, group_ids)
    fail_at <- map_lgl(result_split$val, is_assertion_fail)
    n_fails <- sum(fail_at)

    # We're only going to generate a nice error message for the first group
    # which caused a failure.
    first_fail <- which.max(fail_at)
    fail_group_id <- result_split$key[[first_fail]]
    fail_result <- result_split$val[[first_fail]]
    fail_data <- data[group_ids == fail_group_id, ]

    # Splitting a <rule> `result` un-classes it to logical. So, we re-class the
    # `fail_result` here, which is faster than using a split-method for <rule>.
    if (is_rule(result)) {
      fail_result <- rule(
        fail_result,
        fail = fail_message(result),
        pass = pass_message(result)
      )
    }

    message <- result_bullet(
      result = fail_result,
      result_name = names(results)[[i]],
      data = fail_data,
      env = env,
      type = "fail"
    )
    group_header <- paste0(
      "In group: `dplyr::cur_group_id() == ", fail_group_id, "`",
      if (n_fails - 1) paste0(" (and ", n_fails - 1, " other group", "s"[n_fails > 2], ")")
    )
    messages <- c(messages, c(i = group_header, message))
  }

  if (n_failures == 0) {
    header <- paste0("Passed ", n_results, "/", n_results, " dataframe assertions.")
  } else if (n_failures == n_results) {
    header <- paste0("Failed ", n_results, "/", n_results, " dataframe assertions.")
  } else {
    header <- paste(
      "Passed", n_results - n_failures, "and failed", n_failures,
      "grouped dataframe assertions."
    )
  }

  list(
    header = header,
    bullets = messages
  )
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

