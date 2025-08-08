# Specific code for each pausability verification


verif_1_1 <- function(rc_data, date1, date2) {

  odytools::ody_rc_select(rc_data, !!date1, !!date2) |>
    odytools::ody_rc_format() |>
    dplyr::mutate(
      # If there is no date1 or date2 date, it is considered correct (this is a
      # completeness matter). This check only focuses on ensuring that
      # date1 <= date2 when both dates exist.
      .ok = .data[[date1]] <= .data[[date2]] |
        is.na(.data[[date1]]) |
        is.na(.data[[date2]])
    ) |>
    filter_issues(
      issue_text = glue::glue(
        "{date1} (<<{date1}>>) is after {date2} (<<{date2}>>)."
      )
    )

}
