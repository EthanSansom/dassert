#' @export
inspect <- function(
    .data,
    ...,
    .by = NULL,
    .header = NULL,
    .call = rlang::caller_env(),
    .keep = NULL,
    .before = NULL,
    .after = NULL
) {
  # TODO: Validate inputs
  signal_assertion_results(
    .data = .data,
    ...,
    .by = rlang::enquo(.by),
    .header = .header,
    .call = .call
  )
}
