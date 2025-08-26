# Specific code for each pausability verification


verif_1_1 <- function(rc_data, date1, date2) {
# Intraform

  odytools::ody_rc_select(rc_data, !!date1, !!date2, .accept_form_name = FALSE) |>
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

verif_2_1 <- function(rc_data, date1, date2, min_period, max_period, unit) {
# Interform

  # The reference variable is date1.
  date1_tbl <- odytools::ody_rc_select(rc_data, !!date1, .accept_form_name = FALSE)
  date2_tbl <- odytools::ody_rc_select(rc_data, !!date2, .accept_form_name = FALSE) |>
    dplyr::select(1, !!date2)

  dplyr::inner_join(date1_tbl, date2_tbl, by = attr(rc_data, "id_var")) |>
    odytools::ody_rc_format() |>
    dplyr::mutate(
      period_years = lubridate::time_length(
        .data[[date2]] - .data[[date1]], unit = unit
      ),
      .ok = dplyr::between(
        period_years,
        as.numeric(min_period),
        as.numeric(max_period)
      )
    ) |>
    filter_issues(
      issue_text = glue::glue(
        "Time from {date1} (<<{date1}>>) to {date2} (<<{date2}>>) is <<round(period_years, 1)>> {unit} (expected {min_period}-{max_period})."
      )
    )

}

verif_3_1 <- function(rc_data, var_name, expected) {

  odytools::ody_rc_select(rc_data, !!var_name, .accept_form_name = FALSE) |>
    odytools::ody_rc_format() |>
    tidyr::pivot_longer(cols = tidyselect::all_of(var_name)) |>
    dplyr::mutate(
      .ok = is.na(value) | value == expected
    ) |>
    filter_issues(
      issue_text = glue::glue(
        "<<name>> has value '<<value>>', expected '{expected}'."
      )
    )

}


verif_4_1 <- function(rc_data, var_name) {

  odytools::ody_rc_select(rc_data, !!var_name, .accept_form_name = FALSE) |>
    odytools::ody_rc_format() |>
    tidyr::pivot_longer(cols = var_name) |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::group_by(
      .data[[attr(rc_data, "id_var")]],
      .data$redcap_event_name,
      .data$redcap_form_name,
      .data$redcap_instance_type,
      .data$name,
      .data$value
    ) |>
    dplyr::summarise(
      instance_value = stringr::str_c(redcap_instance_number, collapse = ",") |>
        stringr::str_c("=", unique(value))
    ) |>
    dplyr::summarise(
      grouped_values = stringr::str_c(instance_value, collapse = " & "),
      redcap_instance_number = NA_character_,
      n = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      .ok = n == 1
    ) |>
    filter_issues(
      issue_text = "<<name>> is not equal across instances (<<grouped_values>>)."
    )

}

