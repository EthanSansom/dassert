# Setup
df <- data.frame(
  x = 1:10,
  y = letters[1:10],
  g = rep(c("A", "B"), each = 5)
)
load_all()

# `mutate()` overhead on success
bench::mark(
  df %>% dplyr::mutate(x < 11, y != "Z"),
  df %>% assert(x < 11, y != "Z"),
  check = FALSE
)

# `mutate()` overhead on success (with `.by`)
bench::mark(
  df %>% dplyr::mutate(x < 11, y != "Z", .by = g),
  df %>% assert(x < 11, y != "Z", .by = g),
  check = FALSE
)

# `mutate()` overhead on failure (failure takes a decent amount of time)
bench::mark(
  try(df %>% dplyr::mutate(x < 11, y != "Z") %>% cli::cli_abort(message = "", data = .)),
  try(df %>% assert(x > 11, y != "Z")),
  check = FALSE
)
