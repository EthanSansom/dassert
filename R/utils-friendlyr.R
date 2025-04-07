# comments ---------------------------------------------------------------------

# Once {friendlyr} is finished and on CRAN, replace these helpers with the
# {friendlyr} equivalents.

# utils ------------------------------------------------------------------------

or_null <- function(text, null = TRUE) {
  if (null) {
    paste(text, "or `NULL`")
  } else {
    text
  }
}
