# Specific code for each pausability verification


verif_1_1 <- function(rc_data, start_date, end_date) {

  odytools::ody_rc_select(rc_data, !!start_date, !!end_date) |>
    odytools::ody_rc_format() |>
    dplyr::mutate(
      # If there is no start_date or end_date date, it is considered correct. This check
      # only focuses on ensuring that start_date <= end_date when both dates exist.
      .ok = .data[[start_date]] <= .data[[end_date]] |
        is.na(.data[[start_date]]) |
        is.na(.data[[end_date]])
    ) |>
    filter_issues(
      issue_text = glue::glue(
        "{start_date} (<<{start_date}>>) is after {end_date} (<<{end_date}>>)."
      )
    )

}
