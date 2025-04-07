# todos ------------------------------------------------------------------------

## More consistent naming conventions, makes the package easier to think about
# - Top of message is "header", rest is "bullets"
# - Each `...` argument is an "assertion"
#   - A passed (all TRUE) assertion is a `pass` (call function `is_pass()`)
#   - A failed (all NA or FALSE) assertion is a `fail` (function `is_fail()`)

## Hacks for working within `across()`.
# To make rules work in `across()`, specifically for naming, we want to
# use the `.names()` and `.names_glue()` functions in the testing section,
# which are hacks to exploit the `.col` object in the `.names` argument of
# `dplyr::across()`.

## Re-exports.
# Re-export functions from other packages that are useful for working with {dassert}
# - `rlang::caller_arg()` for capturing arguments in custom rules (although `substitute()`
#   might be fine)
#   - We actually need to be careful about this, since sometimes we want the *actual*
#     argument name (e.g. it's exact symbol via `rlang::as_name()`) and other times
#     we just want a nice label (via `rlang::caller_arg()` which uses `rlang::as_label()`
#     internally)
# - `cli::format_inline()` and possibly a vectorized variant if {cli} doesn't
#   provide one. This is useful for creating a function which returns a `<rule>`,
#   since we want to mention the objects in the error message.
#   - implement a `format_inline_vec` which just vectorizes cli::format_inline

## Vectorized `cli::format_inline()`
#
# This is `cli::format_inline()`, `cli_fmt()` is an exported function. So, we
# just need to use a vectorized {cli} output function.
#
# function (..., .envir = parent.frame(), collapse = TRUE, keep_whitespace = TRUE)
# {
#   opts <- options(cli.width = Inf)
#   on.exit(options(opts), add = TRUE)
#   fun <- if (keep_whitespace)
#     cli_inline
#   else cli_text
#   cli_fmt(fun(..., .envir = .envir), collapse = collapse, strip_newline = TRUE)
# }
# <bytecode: 0x128c8a5f8>
# <environment: namespace:cli>
#
# I think we can use `cli::cli_bullets()`, by un-naming beforehand. It allows the
# same string-interpolation as `cli::cli_text()` but accepts a vector as input.
# If we un-name, this ensures that we don't attach a bullet to the front.
if (FALSE) {
  # This does what we want
  out <- cli::cli_fmt(cli::cli_bullets(c("{.arg 10}", "{.val {TRUE}}")))
  out
  cat(out)

  format_inline_vec <- function(text, .envir = parent.frame(), bullets = TRUE) {
    if (!bullets) text <- unname(text)
    cli::cli_fmt(
      expr = cli::cli_bullets(text, .envir = .envir),
      collapse = FALSE,
      strip_newline = TRUE
    )
  }
}

# assert -----------------------------------------------------------------------

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
  # Intersect arguments that would otherwise affect `dplyr::mutate()`
  check_unsupplied_null(.keep)
  check_unsupplied_null(.before)
  check_unsupplied_null(.after)

  check_is_class(.call, is.environment, "environment")
  check_is_type(.header, rlang::is_string, "a string", null_ok = TRUE)

  signal_assertion_failures(
    .data = .data,
    # Deferring validation of `.by` and `...` to `dplyr::mutate()`
    ...,
    .by = rlang::enquo(.by),
    .header = .header,
    .call = .call,
    .type = "assert"
  )
}

# testing ----------------------------------------------------------------------

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

# Experiments with across
if (FALSE) {
  load_all()
  library(dplyr)

  df <- tibble(
    x = 1:10,
    y = letters[1:10],
    g = rep(c("A", "B"), each = 5)
  )

  df %>%
    assert(
      across(
        everything(),
        ~rule(
          test = is.logical(.x),
          fail = paste0(
            "`{'", cur_column(), "'}` must be a logical vector, not ",
            "{.obj_type_friendly {(.data$", cur_column(), ")}}"
          )
        )
      )
    )

  is_logical <- function(x) {
    # x_name <- rlang::caller_arg(x)
    x_name <- substitute(x)
    rule(
      test = is.logical(x),
      fail = cli::format_inline(
        "{.arg {x_name}} must be a logical vector, not ",
        "{.obj_type_friendly {x}}."
      )
    )
  }

  df %>% assert(across(everything(), is_logical))
  df %>% assert(across(everything(), is.logical, .names = "is.logical({.col})"))
  df %>% assert(across(everything(), list(is.logical = is.logical), .names = "{.fn}({.col})"))


  # TODO: Implement `.names` and `.names_glue` as experimental functions under
  # "`dplyr::across()` helpers". These are meant to be used as arguments to
  # the `.names` argument of `dplyr::across()`.
  .names <- function(expr) {
    deparsed <- rlang::expr_text(rlang::enexpr(expr))
    paste0("{", deparsed, "}")
  }

  .names_glue <- function(..., .open = "<<", .close = ">>") {
    # TODO: `text`, `.open`, and `.close` must be strings here,
    #       and .open doesn't contain '{' or '}' (same for .close).
    text <- encodeString(paste(..., collapse = ""), quote = '"')
    open <- encodeString(.open, quote = '"')
    close <- encodeString(.close, quote = '"')
    paste0("{glue::glue(", text, ", .open = ", open, ", .close = ", close, ")}")
  }

  df %>%
    assert(
      across(
        everything(),
        is.logical,
        # .names = "{paste0('`', .col, '` must be a logical vector, not {.obj_type_friendly {', .col, '}}')}"
        # .names = .names(paste0("`", .col, "` must be a logical vector, not {.obj_type_friendly {", .col, "}}.")),
        # .names = .names(glue::glue(
        #   "{.arg <<.col>>} must be a logical vector, not {.obj_type_friendly {<<.col>>}}.",
        #   .open = "<<",
        #   .close = ">>"
        # ))
        .names = .names_glue(
          "{.arg <<.col>>} must be a logical vector, not {.obj_type_friendly {<<.col>>}}."
        )
      )
    )

  is_logical_2 <- function(x) {
    result <- is.logical(x)
    if (result) {
      result
    } else {
      x_name <- rlang::caller_arg(x)
      rule(
        test = result,
        fail = cli::format_inline(
          "{.arg {x_name}} must be a logical vector, not ",
          "{.obj_type_friendly {x}}."
        )
      )
    }
  }

  above <- function(x, min) {
    result <- x > min
    if (is_assertion_pass(result)) {
      return(result)
    }
    rule(
      test = result,
      fail = cli::format_inline(
        "{.arg {rlang::caller_arg(x)}} must be greater ",
        "than {rlang::caller_arg(min)}."
      )
    )
  }

  bench::mark(
    is_logical(10),
    is_logical_2(10),
    is.logical(10),
    check = FALSE
  )

  bench::mark(
    is_logical(TRUE),
    is_logical_2(TRUE),
    is.logical(TRUE),
    check = FALSE
  )

}

# Test `assert()` with `dplyr::group_by()`
if (FALSE) {
  load_all()
  library(dplyr)

  df <- tibble(
    x = 1:10,
    y = letters[1:10],
    g = rep(c("A", "B"), each = 5)
  )

  # What's faster, dropping grouping columns with {dplyr} or base? (Answer, base)
  df_grouped <- df %>% group_by(g)
  bench::mark(
    dplyr::select(dplyr::ungroup(df_grouped), -dplyr::all_of(group_vars(df_grouped))),
    df_grouped[!names(df_grouped) %in% group_vars(df_grouped)]
  )

  dplyr::mutate(df_grouped, dplyr::cur_group_id())

  # Confirm: group_by plays well with `assert()`
  df %>% group_by(g) %>% assert(x < 5)

  # Does group_by + `.by` cause an error? (Answer, yes)
  df %>% group_by(g) %>% mutate(mean(x), .by = y)

  df %>% mutate(x = 10, .by = x)
}

# Test `assert()` with `dplyr::rowwise()`
if (FALSE) {
  load_all()
  library(dplyr)

  df <- tibble(
    x = 1:10,
    y = 1:10,
    g = rep(c("A", "B"), each = 5)
  )

  df_rowwise <- df %>% rowwise()
  class(df_rowwise)

  # Maintains <rowwise> on pass
  df_rowwise %>% assert("{fail_at()}" = mean(c(x, y)) < 11)
  # Rowwise works correctly for errors
  df_rowwise %>% assert("{fail_at()}" = mean(c(x, y)) < 11)

}
