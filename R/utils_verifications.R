verify_completeness <- function(
    to_verify_data,
    variable,
    user_na_is_data = TRUE
  ) {

  to_verify_data |>
    odytools::ody_rc_format(keep_user_na = user_na_is_data) |>
    dplyr::mutate(.ok = !is.na(.data[[variable]]))

}


verify_flipped_dates <- function(
    to_verify_data,
    start_date,
    end_date
  ) {

  to_verify_data |>
  odytools::ody_rc_format() |>
    dplyr::mutate(
      # If there is no start or end date, it is considered correct. This check
      # only focuses on ensuring that start <= end_date when both dates exist.
      .ok = .data[[start_date]] <= .data[[end_date]] |
        is.na(.data[[start_date]]) |
        is.na(.data[[end_date]])
    )

}
