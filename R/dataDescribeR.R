# A package to leverage a usefull bespoke function I use to describe data sets
# with in greater detail.
#' data_describer function
#'
#' @param data A dataframe you want to describe
#' @param example_number The amount of example values you want to include in the output, must be a numeric input
#'
#' @return A dataframe
#'
#' @export
#'
#' @examples
#' data_describer(data = tidyr::billboard, example_number = 3)
data_describer <- function(data, example_number) {

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
    dplyr::sample_n(example_number, replace = T) |>
    dplyr::summarise(
      value = stringr::str_c(value, collapse = "; ")
    ) |>
    dplyr::mutate(
      name = factor(name, levels = ordered_cols)
    ) |>
    dplyr::arrange(name) |>
    dplyr::left_join(skimr::skim(data), by = c("name" = "skim_variable")) |>
    dplyr::rename(
      "column_name" = 1,
      "example_values" = 2,
      "data_type_guess" = 3
    )

}
