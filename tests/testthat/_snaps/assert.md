# `assert()` works with the `.by` group argument

    Failed 1/1 grouped dataframe assertions.
    i In group: `dplyr::cur_group_id() == 2` (and 1 other group)
    x x < 4

---

    Failed 1/2 grouped dataframe assertions.
    i In group: `dplyr::cur_group_id() == 1` (and 2 other groups)
    x y == "Z"

---

    Failed 2/2 grouped dataframe assertions.
    i In group: `dplyr::cur_group_id() == 2` (and 1 other group)
    x x < 4
    i In group: `dplyr::cur_group_id() == 1` (and 2 other groups)
    x y == "A"

# `assert()` works on grouped inputs

    Failed 1/1 grouped dataframe assertions.
    i In group: `dplyr::cur_group_id() == 1` (and 1 other group)
    x x < 4

---

    Failed 1/2 grouped dataframe assertions.
    i In group: `dplyr::cur_group_id() == 2`
    x y %in% letters[1:5]

# `assert()` works on rowwise inputs

    Failed 1/1 grouped dataframe assertions.
    i In group: `dplyr::cur_group_id() == 1` (and 1 other group)
    x x < 4

---

    Failed 1/2 grouped dataframe assertions.
    i In group: `dplyr::cur_group_id() == 2`
    x y %in% letters[1:5]

