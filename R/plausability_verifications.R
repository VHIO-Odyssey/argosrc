# Specific code for each pausability verification


# adverse_events ---------------------------------------------------------------


adverse_events_1_1 <- function(
    rc_data,
    ae_stat,
    ae_startdate,
    ae_enddate) {

  issue_text <- glue::glue(
    "{ae_startdate} (<<{ae_startdate}>>) is after {ae_enddate} (<<{ae_enddate}>>)."
  )

  odytools::ody_rc_select(
    rc_data,
    !!ae_stat,
    !!ae_startdate,
    !!ae_enddate
  ) |>
    dplyr::filter(.data[[ae_stat]] == "1") |>
    verify_flipped_dates(ae_startdate, ae_enddate) |>
    filter_issues(issue_text)

}
