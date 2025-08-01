# Adverse Events Verifications

adverse_events_1_1 <- function(rc_data, ae_stat) {

  issue_text <- glue::glue("{ae_stat} is empty.")

  odytools::ody_rc_select(
    rc_data,
    !!ae_stat
  ) |>
    verify_completeness(ae_stat) |>
    filter_issues(issue_text)

}


adverse_events_2_1 <- function(
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
