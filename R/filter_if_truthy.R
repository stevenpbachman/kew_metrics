#' Filter data if a condition `isTruthy`
#'
#' Checks if a condition meets the [shiny::isTruthy()] condition, and if it is then filter a column
#' of the data on matches to the `ifTruthy` value.
#'
#' @details
#' When the condition of `ifTruthy` is found to be `TRUE` according to [shiny::isTruthy()], then the
#' data is filtered where `col %in% ifTruthy`.
#'
#' @param .data A dataset to be filtered.
#' @param col <[`data-masking`][rlang::args_data_masking]> A column for that should be filtered by
#'   for matches against the `isTruthy` variable.
#' @param ifTruthy A value that is check for Truthyness using [shiny::isTruthy()]
#' @return The filtered dataset if the `ifTruthy` variable if Truthy, or the original dataset if
#'   not.
filter_if_truthy <- function(.data, col, ifTruthy) {
  if (shiny::isTruthy(ifTruthy)) {
    dplyr::filter(.data, {{ col }} %in% .env$ifTruthy)
  } else {
    .data
  }
}
