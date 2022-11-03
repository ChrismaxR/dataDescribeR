# A package to leverage a usefull bespoke function I use to describe data sets
# with in greater detail.
#' data_describer function
#'
#' @param data A dataframe you want to describe
#'
#' @return A dataframe
#'
#' @export
#'
#' @examples
#' data_describer(mtdata)
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
