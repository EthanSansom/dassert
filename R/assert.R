# TODO: Review how {dplyr} does grouping with `.by`. You'll want to use {tidyselect}
# and {vctrs} to run the underlying `.by` operation:
# https://github.com/tidyverse/dplyr/blob/2d87e4f491691384183f88c9991dd3396f777352/R/by.R#L28

assert <- function(
    .data,
    ...,
    .by = NULL,
    .header = NULL,
    .call = rlang::caller_env(),
    .keep = NULL,
    .before = NULL,
    .after = NULL
) {
  # TODO: Error checking here
  signal_assertion_failures(
    .data = .data,
    ...,
    .by = .by,
    .header = .header,
    .call = .call,
    .type = "assert"
  )
}

alert <- function(
    .data,
    ...,
    .by = NULL,
    .header = NULL,
    .call = rlang::caller_env(),
    .keep = NULL,
    .before = NULL,
    .after = NULL
  ) {
  signal_assertion_failures(
    .data = .data,
    ...,
    .by = .by,
    .header = .header,
    .call = .call,
    .type = "alert"
  )
}

