test_that("`assert()` works", {
  df <- data.frame(
    x = 1:10,
    y = letters[1:10]
  )

  # No condition emitted on pass
  expect_no_condition(df %>% assert(x < 11))
  expect_no_condition(df %>% assert(x < 11, y != "Z"))
  expect_no_condition(df %>% assert())

  # Returns the input dataset on success
  expect_identical(df %>% assert(x < 11), df)
  expect_identical(df %>% assert(x < 11, y != "Z"), df)
  expect_identical(df %>% assert(), df)

  # Errors on failure
  expect_assertion_error(
    df %>% assert(x > 11),
    regexp = "x > 11",
    fixed = TRUE
  )
  expect_assertion_error(
    df %>% assert(x < 11, y == "A"),
    regexp = 'y == "A"',
    fixed = TRUE
  )
  expect_assertion_error(
    df %>% assert(x > 11, y == "A"),
    regexp = '(x > 11).*(y == "A")'
  )
})

test_that("`assert()` errors on invalid named input arguments", {
  df <- data.frame(
    x = 1:10,
    y = letters[1:10],
    g = rep(c("A", "B"), each = 5)
  )

  # `.by` cannot be used with grouped or row-wise `.data`
  expect_input_incompatible_error(
    df %>% dplyr::group_by(g) %>% assert(x < 4, .by = y)
  )
  expect_input_incompatible_error(
    df %>% dplyr::rowwise() %>% assert(x < 4, .by = y)
  )

  # Vestigial `mutate()` arguments `.keep`, `.before`, `.after` aren't allowed
  expect_input_type_error(df %>% assert(x < 4, .keep = "all"))
  expect_input_type_error(df %>% assert(x < 4, .before = "y"))
  expect_input_type_error(df %>% assert(x < 4, .after = "y"))
})

# TODO: This is more complicated, since we need to catch an re-throw errors
#       chained error raised by `mutate()`.
test_that("`assert()` error on invalid `...` arguments", {

})

test_that("`assert()` works with the `.by` group argument", {
  df <- data.frame(
    x = 1:10,
    y = letters[1:10],
    g1 = c("A", "A", "A", "B", "B", "B", "C", "C", "C", "C")
  )

  # No condition emitted on pass
  expect_no_condition(df %>% assert(x < 11, .by = g1))
  expect_no_condition(df %>% assert(x < 11, y != "Z", .by = g1))
  expect_no_condition(df %>% assert(.by = g1))

  # Returns the input dataset on success
  expect_identical(df %>% assert(x < 11, .by = g1), df)
  expect_identical(df %>% assert(x < 11, y != "Z", .by = g1), df)
  expect_identical(df %>% assert(.by = g1), df)

  # Errors on failure
  expect_snapshot_error(
    df %>% assert(x < 4, .by = g1),
    class = "dassert_error_assertion_fail"
  )
  expect_snapshot_error(
    df %>% assert(x < 11, y == "Z", .by = g1),
    class = "dassert_error_assertion_fail"
  )
  expect_snapshot_error(
    df %>% assert(x < 4, y == "A", .by = g1),
    class = "dassert_error_assertion_fail"
  )
})

test_that("`assert()` works on grouped inputs", {
  df <- data.frame(
    x = 1:10,
    y = letters[1:10],
    g = rep(c("A", "B"), each = 5)
  )

  # No error emitted on pass
  expect_no_error(df %>% dplyr::group_by(g) %>% assert(x < 11))
  expect_no_error(df %>% dplyr::group_by(g) %>% assert())

  # Returns grouped the input dataset on success
  expect_true(dplyr::is_grouped_df(df %>% dplyr::group_by(g) %>% assert()))
  expect_identical(
    df %>% dplyr::group_by(g) %>% assert(x < 11),
    df %>% dplyr::group_by(g)
  )

  # Errors on failure
  expect_snapshot_error(
    df %>% dplyr::group_by(g) %>% assert(x < 4),
    class = "dassert_error_grouped_assertion_fail"
  )
  expect_snapshot_error(
    df %>% dplyr::group_by(g) %>% assert(x < 11, y %in% letters[1:5]),
    class = "dassert_error_grouped_assertion_fail"
  )
})

# TODO!
test_that("`assert()` works on rowwise inputs", {
  df <- data.frame(
    x = 1:10,
    y = letters[1:10],
    g = rep(c("A", "B"), each = 5)
  )
})
