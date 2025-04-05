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
  expect_error(
    df %>% assert(x > 11),
    class = "dassert_assertion_error",
    regexp = "x > 11",
    fixed = TRUE
  )
  expect_error(
    df %>% assert(x < 11, y == "A"),
    class = "dassert_assertion_error",
    regexp = 'y == "A"',
    fixed = TRUE
  )
  expect_error(
    df %>% assert(x > 11, y == "A"),
    class = "dassert_assertion_error",
    regexp = '(x > 11).*(y == "A")'
  )
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
    class = "dassert_assertion_error"
  )
  expect_snapshot_error(
    df %>% assert(x < 11, y == "Z", .by = g1),
    class = "dassert_assertion_error"
  )
  expect_snapshot_error(
    df %>% assert(x < 4, y == "A", .by = g1),
    class = "dassert_assertion_error"
  )
})
