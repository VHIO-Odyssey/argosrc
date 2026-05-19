# Specific code for each pausability verification

verif_1_1 <- function(rc_data, date1, date2) {
  # Intraform

  odytools::ody_rc_select(
    rc_data,
    !!date1,
    !!date2,
    .accept_form_name = FALSE
  ) |>
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

verif_1_2 <- function(rc_data, date1, date2) {
  # date1 is expected to be from a unique form.
  # In case date1 comes from a repating form, the check is performed on th
  # minimun date1
  date1_tbl <-
    odytools::ody_rc_select(
      rc_data,
      !!date1,
      .accept_form_name = FALSE
    ) |>
    # only non missing cases (missingness is a completeness issue, not a
    # plausibility issue)
    dplyr::filter_out(is.na(.data[[date1]])) |>
    dplyr::group_by(.data[[attr(rc_data, "id_var")]]) |>
    dplyr::filter(.data[[date1]] == min(.data[[date1]])) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename_with(
      ~ stringr::str_c(., "_", date1),
      dplyr::starts_with("redcap")
    )

  # date2 is expected to be from a multi-instance form (e.g. Adverse Events)
  date2_tbl <-
    odytools::ody_rc_select(
      rc_data,
      !!date2,
      .accept_form_name = FALSE
    ) |>
    # only non missing cases (missingness is a completeness issue, not a
    # plausibility issue)
    dplyr::filter_out(is.na(.data[[date2]])) |>
    dplyr::rename_with(
      ~ stringr::str_c(., "_", date2),
      dplyr::starts_with("redcap")
    )

  dplyr::inner_join(
    date1_tbl,
    date2_tbl,
    by = attr(rc_data, "id_var")
  ) |>
    odytools::ody_rc_format() |>
    dplyr::mutate(
      .ok = .data[[date1]] <= .data[[date2]]
    ) |>
    filter_issues(
      issue_text = glue::glue(
        "{date1} (<<{date1}>>) is after {date2} (<<{date2}>>)."
      )
    )
}

verif_1_3 <- function(rc_data, date1a, date1b, date2) {
  date1a_tbl <-
    odytools::ody_rc_select(
      rc_data,
      !!date1a,
      .accept_form_name = FALSE
    ) |>
    # only non missing cases (missingness is a completeness issue, not a
    # plausibility issue)
    dplyr::filter_out(is.na(.data[[date1a]])) |>
    dplyr::group_by(.data[[attr(rc_data, "id_var")]]) |>
    dplyr::filter(.data[[date1a]] == min(.data[[date1a]])) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename_with(
      ~ stringr::str_c(., "_", date1a),
      dplyr::starts_with("redcap")
    )

  date1b_tbl <-
    odytools::ody_rc_select(
      rc_data,
      !!date1b,
      .accept_form_name = FALSE
    ) |>
    # only non missing cases (missingness is a completeness issue, not a
    # plausibility issue)
    dplyr::filter_out(is.na(.data[[date1b]])) |>
    dplyr::group_by(.data[[attr(rc_data, "id_var")]]) |>
    dplyr::filter(.data[[date1b]] == min(.data[[date1b]])) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename_with(
      ~ stringr::str_c(., "_", date1b),
      dplyr::starts_with("redcap")
    )

  date1_tbl <-
    dplyr::full_join(date1a_tbl, date1b_tbl, by = attr(rc_data, "id_var")) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      date1_name = dplyr::case_when(
        is.na(.data[[date1a]]) & !is.na(.data[[date1b]]) ~ date1b,
        !is.na(.data[[date1a]]) & is.na(.data[[date1b]]) ~ date1a,
        .data[[date1a]] < .data[[date1b]] ~ date1a,
        .data[[date1a]] > .data[[date1b]] ~ date1b,
        .default = date1a
      ),
      date1_value = min(
        dplyr::c_across(tidyselect::all_of(c(date1a, date1b))),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()

  # date2 is expected to be from a multi-instance form (e.g. Adverse Events)
  date2_tbl <-
    odytools::ody_rc_select(
      rc_data,
      !!date2,
      .accept_form_name = FALSE
    ) |>
    # only non missing cases (missingness is a completeness issue, not a
    # plausibility issue)
    dplyr::filter_out(is.na(.data[[date2]])) |>
    dplyr::rename_with(
      ~ stringr::str_c(., "_", date2),
      dplyr::starts_with("redcap")
    )

  dplyr::inner_join(
    date1_tbl,
    date2_tbl,
    by = attr(rc_data, "id_var")
  ) |>
    odytools::ody_rc_format() |>
    dplyr::mutate(
      .ok = .data$date1_value <= .data[[date2]]
    ) |>
    filter_issues(
      issue_text = glue::glue(
        "<<date1_name>> (<<date1_value>>) is after {date2} (<<{date2}>>)."
      )
    )
}

verif_2_1 <- function(rc_data, date1, date2, min_period, max_period, unit) {
  # Interform

  # The reference variable is date1.
  date1_tbl <- odytools::ody_rc_select(
    rc_data,
    !!date1,
    .accept_form_name = FALSE
  )
  date2_tbl <- odytools::ody_rc_select(
    rc_data,
    !!date2,
    .accept_form_name = FALSE
  ) |>
    dplyr::select(1, !!date2)

  dplyr::inner_join(date1_tbl, date2_tbl, by = attr(rc_data, "id_var")) |>
    odytools::ody_rc_format() |>
    dplyr::mutate(
      period_years = lubridate::time_length(
        .data[[date2]] - .data[[date1]],
        unit = unit
      ),
      .ok = dplyr::between(
        .data$period_years,
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
      .ok = is.na(.data$value) | .data$value == expected
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
      instance_value = stringr::str_c(
        .data$redcap_instance_number,
        collapse = ","
      ) |>
        stringr::str_c("=", unique(.data$value))
    ) |>
    dplyr::summarise(
      grouped_values = stringr::str_c(.data$instance_value, collapse = " & "),
      redcap_instance_number = NA_character_,
      n = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      .ok = .data$n == 1
    ) |>
    filter_issues(
      issue_text = "<<name>> is not equal across instances (<<grouped_values>>)."
    )
}


verif_5_1 <- function(
  rc_data,
  target_response,
  no_target_response,
  new_lesions,
  overall_response
) {
  odytools::ody_rc_select(
    rc_data,
    !!target_response,
    !!no_target_response,
    !!new_lesions,
    !!overall_response
  ) |>
    odytools::ody_rc_format() |>
    dplyr::mutate(
      .ok = dplyr::case_when(
        # If the overall_response is NA, it is considered correct (this is a
        # completeness matter).
        is.na(.data[[overall_response]]) ~ TRUE,
        .data[[target_response]] == "Complete Response (CR)" &
          .data[[no_target_response]] == "Complete Response (CR)" &
          .data[[new_lesions]] == "No" &
          .data[[overall_response]] == "Complete Response (CR)" ~ TRUE,
        .data[[target_response]] == "Complete Response (CR)" &
          .data[[no_target_response]] == "Non-CR/Non-PD" &
          .data[[new_lesions]] == "No" &
          .data[[overall_response]] == "Partial Response (PR)" ~ TRUE,
        .data[[target_response]] == "Complete Response (CR)" &
          is.na(.data[[no_target_response]]) &
          .data[[new_lesions]] == "No" &
          .data[[overall_response]] == "Partial Response (PR)" ~ TRUE,
        .data[[target_response]] == "Partial response (PR)" &
          (.data[[no_target_response]] != "Progressive Disease (PD)" |
            is.na(.data[[no_target_response]])) &
          .data[[new_lesions]] == "No" &
          .data[[overall_response]] == "Partial Response (PR)" ~ TRUE,
        .data[[target_response]] == "Stable disease (SD)" &
          (.data[[no_target_response]] != "Progressive Disease (PD)" |
            is.na(.data[[no_target_response]])) &
          .data[[new_lesions]] == "No" &
          .data[[overall_response]] == "Stable Disease (SD)" ~ TRUE,
        (.data[[target_response]] == "Not evaluable" |
          is.na(.data[[target_response]])) &
          (.data[[no_target_response]] == "Non-CR/Non-PD" |
            is.na(.data[[no_target_response]])) &
          .data[[new_lesions]] == "No" &
          is.na(.data[[overall_response]]) ~ TRUE,
        .data[[target_response]] == "Progressive disease (PD)" &
          .data[[overall_response]] == "Progressive Disease (PD)" ~ TRUE,
        .data[[no_target_response]] == "Progressive Disease (PD)" &
          .data[[overall_response]] == "Progressive Disease (PD)" ~ TRUE,
        .data[[new_lesions]] == "Yes" &
          .data[[overall_response]] == "Progressive Disease (PD)" ~ TRUE,
        .default = FALSE
      )
    ) |>
    filter_issues(
      issue_text = glue::glue(
        "Unexpected {overall_response} = <<{overall_response}>> according to {target_response} = <<{target_response}>> , {no_target_response} = <<{no_target_response}>> and {new_lesions} = <<{new_lesions}>>."
      )
    )
}


verif_6_1 <- function(rc_data, last_fu_date, last_fu_status, time_limit, unit) {
  import_date <- attr(rc_data, "import_date") |> as.Date()

  odytools::ody_rc_select(
    rc_data,
    !!last_fu_date,
    !!last_fu_status,
    .accept_form_name = FALSE
  ) |>
    odytools::ody_rc_format() |>
    # filter to ensure we only check the latest follow-up in a repetating form.
    dplyr::filter(.data$redcap_instance_type != "unique") |>
    dplyr::filter_out(is.na(.data[[last_fu_date]])) |>
    dplyr::group_by(.data[[attr(rc_data, "id_var")]]) |>
    dplyr::slice_max(order_by = as.numeric(.data$redcap_instance_number)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      time_since_last_fu = lubridate::time_length(
        import_date - .data[[last_fu_date]],
        unit = unit
      ),
      .ok = dplyr::case_when(
        last_fu_status != "Alive" ~ TRUE,
        time_since_last_fu <= as.numeric(time_limit) ~ TRUE,
        .default = FALSE
      )
    ) |>
    filter_issues(
      issue_text = glue::glue(
        "Last follow-up date (<<{last_fu_date}>>) is <<round(time_since_last_fu, 1)>> {unit} ago and status is 'Alive' (expected <= {time_limit} {unit})."
      )
    )
}

verif_7_1 <- function(rc_data, visit_date, end_date, time_limit, unit) {
  import_date <- attr(rc_data, "import_date") |> as.Date()

  last_date <-
    odytools::ody_rc_select(rc_data, !!visit_date) |>
    odytools::ody_rc_format() |>
    dplyr::filter_out(is.na(.data[[visit_date]])) |>
    dplyr::group_by(.data[[attr(rc_data, "id_var")]]) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()

  stop_date <-
    odytools::ody_rc_select(rc_data, !!end_date) |>
    odytools::ody_rc_format() |>
    dplyr::select(
      tidyselect::all_of(attr(rc_data, "id_var")),
      !!end_date
    )

  dplyr::full_join(last_date, stop_date, by = attr(rc_data, "id_var")) |>
    dplyr::mutate(
      time_since_vist = lubridate::time_length(
        import_date - .data[[visit_date]],
        unit = unit
      )
    ) |>
    dplyr::mutate(
      .ok = !is.na(.data[[end_date]]) | .data$time_since_vist <= time_limit
    ) |>
    filter_issues(
      issue_text = glue::glue(
        "Last {visit_date} (<<{visit_date}>>) is more than {time_limit} {unit} ago and {end_date} is missing."
      )
    )
}
