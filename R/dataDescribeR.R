# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# Import necessary functions from other packages
usethis::use_package("dplyr",  "imports")
usethis::use_package("tibble", "imports")
usethis::use_package("tidyr",  "imports")
usethis::use_package("skimr",  "imports")
usethis::use_package("stringr",  "imports")


# bespoke function to make a sort of data dictionary
data_describer <- function(data) {

  # order the column names, so to reflect column order in source files
  ordered_cols <- names(data) |>
    tibble::enframe() |>
    dplyr::mutate(value = factor(value, ordered = T)) |>
    dplyr::pull(value)

  # make a pivoted set of non-null values, and add skimr::skim variables
  data |>
    dplyr::mutate_all(as.character) |>
    tidyr::pivot_longer(cols = 1:ncol(data)) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(name) |>
    dplyr::sample_n(15) |>
    dplyr::summarise(
      value = stringr::str_c(value, collapse = "; ")
    ) |>
    dplyr::mutate(
      name = factor(name, levels = ordered_cols)
    ) |>
    dplyr::arrange(name) |>
    dplyr::left_join(skimr::skim(data), by = c("name" = "skim_variable"))

}
