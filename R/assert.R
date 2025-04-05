# TODO: Maybe add a once per-session hint at the end of a grouped `assert()` message like:
# i = Use `.data %>% dplyr::filter(dplyr::cur_group_id() == `id`, .by = `.by`)`
#     to filter to an assertion failure group.

#' @export
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
  # TODO: Validate inputs
  signal_assertion_failures(
    .data = .data,
    ...,
    .by = rlang::enquo(.by),
    .header = .header,
    .call = .call,
    .type = "assert"
  )
}

# How does {dplyr} handle really long names?
if (FALSE) {
  # Okay, so the super long name here ends up as `+ ...`, it seems like
  # {dplyr} will try to put the name of the outer call
  thisisanincredicablylongvariablenameforatestiamdoingonmutate <- 10
  dplyr::mutate(
    data.frame(x = 10),
    thisisanincredicablylongvariablenameforatestiamdoingonmutate +
      thisisanincredicablylongvariablenameforatestiamdoingonmutate
  )

  # Yay! So this works totally fine, it's `I(...)` which means we can still
  # protect the contents of the `I()` message
  dplyr::mutate(
    data.frame(x = 10),
    I(thisisanincredicablylongvariablenameforatestiamdoingonmutate +
      thisisanincredicablylongvariablenameforatestiamdoingonmutate),
  )

  # {dplyr} doesn't know what to do with two expressions with a really long
  # name. It names both of these `I(...)` and the last one wins out.
  dplyr::mutate(
    data.frame(x = 10),
    # This is `20`
    I(thisisanincredicablylongvariablenameforatestiamdoingonmutate + thisisanincredicablylongvariablenameforatestiamdoingonmutate),
    # This is `30`
    I(thisisanincredicablylongvariablenameforatestiamdoingonmutate + thisisanincredicablylongvariablenameforatestiamdoingonmutate + 10)
  )

  # {dplyr} does support really long manual names
  dplyr::mutate(
    data.frame(x = 10),
    "I(thisisanincredicablylongvariablenameforatestiamdoingonmutate + thisisanincredicablylongvariablenameforatestiamdoingonmutate + 10)" = 20
  )

  # We can also insert this in manual column name
  dplyr::mutate(
    data.frame(x = 10),
    "I({x} protect this column.)" = 20
  )

}
