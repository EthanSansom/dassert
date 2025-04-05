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
