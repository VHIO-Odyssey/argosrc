# Completeness related functions

# Helper function to check inside get_conditions_from_metadata whether the
#  data_frame can be actually filtered by the elements of the conditions_list.
filter_condition <- function(data_frame, condition) {

  data_frame |>
    dplyr::filter(eval(str2lang(condition)))

}

# Helper functions to create a conditions_list from redcap metadata.
get_conditions_from_metadata <- function(metadata, missing_codes) {

  needed_meta <- metadata |>
    dplyr::filter(!is.na(.data$branching_logic))

  missing_value <- stringr::str_c(
    missing_codes$raw_value, collapse = "|")

  if (nrow(needed_meta) > 0) {

    external_branching <- needed_meta |>
      dplyr::filter(
        stringr::str_detect(
          .data$branching_logic,
          "\\[.+\\]\\[.+\\]|current-instance|user-role-name"
        )
      ) |> dplyr::pull("field_name")

    if (length(external_branching) > 0) {
      warning(
        "External branching detected for variables\n",
        stringr::str_c(external_branching, collapse = "\n"),
        "\nExternal branching is still not implemented"
      )
    }

    pre_list <- needed_meta |>
      dplyr::filter(!(.data$field_name %in% external_branching)) |>
      dplyr::select("field_name", "branching_logic") |>
      dplyr::mutate(
        # RedCap logic is translated into R languaje
        r_branch =
          stringr::str_replace_all(
            .data$branching_logic, "event-name", "redcap_event_name"
          ) |>
          # Any upper case AND / OR is lower cased to avoid motential confusion with
          # the missing data codes.
          stringr::str_replace_all(" AND ", " and ") |>
          stringr::str_replace_all( " OR ", " or ") |>
          stringr::str_replace_all("\\n", " ") |>
          stringr::str_replace_all(missing_value, "user_na") |>
          # Checkbox variables to especific check box column
          stringr::str_replace_all( "\\((\\d+)\\)", "___\\1") |>
          stringr::str_replace_all(
            # RedCap empty to regular R na
            "\\[([^\\[]+)\\] *<> *['\"]{2}",
            "!labelled::is_regular_na\\(\\1\\)"
          ) |>
          stringr::str_replace_all(
            # RedCap declared missing to user defined R na
            "\\[([^\\[]+)\\] *<> *['\"]user_na['\"]",
            "!labelled::is_user_na\\(\\1\\)"
          ) |>
          #Some easy symbol translations
          stringr::str_remove_all("\\[|\\]") |>
          stringr::str_replace_all("=", "==") |>
          stringr::str_replace_all("<>", "!=") |>
          stringr::str_replace_all(" or ", " | ") |>
          stringr::str_replace_all(" and ", " & ") |>
          # Delete possible duplicates of is_user_na
          stringr::str_replace_all("(.*labelled::is_user_na.+)\\1+", "\\1"),
        cond = stringr::str_c(
          .data$field_name, " = ", "\"", .data$r_branch, "\""
        )
      ) |>
      dplyr::pull("cond")

    conditions_list <- stringr::str_c(
      "list(",
      stringr::str_c(pre_list, collapse = ", "),
      ")"
    ) |> str2lang() |> eval()

  } else {

    conditions_list <- NULL

  }

  conditions_list

  # Legado de odytools. Desactivado mientras se piensa sobre una gestíon más global.
  # We need to check if the conditions are actually filterable since variables
  # outside the data_frame can be used in the conditions. This is not suported
  # by the current implementation because ody_verify_completeness (the function
  # in charge of checking the conditions) only checks for the conditional presence
  # of variables in the current data_frame.

  # secure_filter_conditions <- purrr::possibly(filter_condition)
  #
  # ok_index <- purrr::map_lgl(
  #   conditions_list,
  #   ~ secure_filter_conditions(data_frame, .) |>
  #     is.data.frame()
  # )
  #
  # if (sum(ok_index) < length(conditions_list)) {
  #   warning(
  #     "Some conditions are not filterable because they use variables not present in the data frame. These conditions will be ignored."
  #   )
  # }
  #
  # conditions_list[ok_index]

}

verify_completeness_form <- function(
    rc_data,
    current_form_name,
    conditions_list,
    user_na_is_data,
    missing_data_codes
) {

  id_var <- attr(rc_data, "id_var")

  current_form <- odytools::ody_rc_select_form(
    rc_data, !!current_form_name
  )

  if (nrow(current_form) == 0) {

    empty_result <-
      tibble::tibble(
      "{id_var}" := NA_character_,
      redcap_event_name = NA_character_,
      redcap_form_name = NA_character_,
      redcap_instance_type = NA_character_,
      redcap_instance_number = NA_character_,
      variable = NA_character_,
      missing_type = NA_character_
    ) |>
      dplyr::filter(!is.na(variable))

    return(empty_result)

  }

  current_variables_name <-
  current_form |>
    dplyr::select(
      -all_of(id_var),
      -"redcap_event_name",
      -"redcap_form_name",
      -"redcap_instance_type",
      -"redcap_instance_number",
      -stringr::str_c(current_form_name, "_complete")
    ) |>
    names()


  purrr::map(
    current_variables_name,
    function(x) {

      current_variable <- str2lang(x)
      current_condition_raw <- conditions_list[[current_variable]]

      if (is.null(current_condition_raw)) {
        current_condition <- TRUE
        current_condition_label <- "Allways"
      } else {
        current_condition <- str2lang(current_condition_raw)
        current_condition_label <- current_condition_raw
      }

      if (user_na_is_data) {
        na_fn <- labelled::is_regular_na
      } else {
        na_fn <- is.na
      }


      current_form |>
        # Safe version. If the filter fails (mainly because the variables
        # declared in current_condition do not belong to the current form) it
        # returns the original unfiltered form.
        safe_filter(!!current_condition) |>
        dplyr::filter(na_fn(!!current_variable)) |>
        dplyr::mutate(
          variable = x,
          condition = current_condition_label,
          missing_type = dplyr::case_when(
            labelled::is_regular_na(!!current_variable) ~ "Regular",
            labelled::is_user_na(!!current_variable) ~ "User defined",
          ),
          missing_value = dplyr::case_when(
            labelled::is_regular_na(!!current_variable) ~ NA_character_,
            labelled::is_user_na(!!current_variable) ~ as.character(!!current_variable)
          )
        ) |>
        dplyr::select(
          tidyselect::any_of(c(
            id_var,
            "redcap_event_name",
            "redcap_form_name",
            "redcap_instance_type",
            "redcap_instance_number"
          )),
          "variable",
          "condition",
          "missing_type",
          "missing_value"
        )


    }
  ) |>
    purrr::list_rbind() |>
    dplyr::left_join(
      missing_data_codes,
      by = c("missing_value" = "raw_value")
    ) |>
    dplyr::mutate(
      missing_value = label
    ) |>
    dplyr::select(-"label")

}


#' Check completeness of REDCap data forms
#'
#' This function verifies completeness of specified forms in REDCap data,
#' considering user-defined missing values and branching logic conditions.
#'
#' @param rc_data A REDCap data frame with associated attributes
#'   `"metadata"`, `"missing"`, and `"forms"`.
#' @param forms Character vector of form names to check or `"All"` (the default) to check all forms.
#' @param user_na_is_data Logical, if TRUE treats user-defined missing values as non-missing values.
#' @param extra_conditions_list Optional list of additional conditions to consider.
#'
#' @return A tibble summarizing missing data per variable and form.
#' @export
argos_check_completeness <- function(
    rc_data,
    forms = "All",
    user_na_is_data = TRUE,
    extra_conditions_list = NULL) {

  metadata <- attr(rc_data, "metadata")
  missing_data_codes <- attr(rc_data, "missing")
  conditions_list <- get_conditions_from_metadata(
    metadata, missing_data_codes
  )

  if (!is.null(extra_conditions_list)) {

    conditions_list <-
      # If any variable in extra_conditions_list is already defined by
      # the default branching logic, it is removed so the new will conditions
      #  apply
      conditions_list[
        !names(conditions_list) %in% names(extra_conditions_list)
      ] |>
      c(extra_conditions_list)

  }

  if (any(forms == "All")) forms <- attr(rc_data, "forms")$instrument_name

  purrr::map(
    forms,
    ~ verify_completeness_form(
      rc_data,
      .,
      conditions_list,
      user_na_is_data,
      missing_data_codes
    ),
    .progress = "Argos is searching \U1F415"
  ) |>
    purrr::list_rbind() |>
    dplyr::relocate(.data[["condition"]], .after = "variable") |>
    dplyr::arrange(.data[[attr(rc_data, "id_var")]])

}


#' Count form completions in REDCap data
#'
#' This function counts the number of records per form and event in REDCap data.
#' It returns a list of data frames, one per event, with counts of forms completed by record.
#' Optionally, the result can be saved as an Excel file with one sheet per event.
#'
#' @param rc_data A REDCap data frame imported with  `odytools::ody_rc_import()`
#' @param save_path Optional path to save the output Excel file; if NULL, no file is saved.
#'
#' @return A list of tibbles, each containing counts of completed forms per record for a specific event.
#' @export
argos_count_forms <- function(rc_data, save_path = NULL) {

  id_var <- attr(rc_data, "id_var")
  subjects <- attr(rc_data, "subjects")
  forms <- attr(rc_data, "forms")$instrument_name
  events <- attr(rc_data, "events")$unique_event_name
  forms_events_mapping <- attr(rc_data, "forms_events_mapping")

  form_count_raw <- purrr::map(
    forms,
    function(form) {
      form_data <- odytools::ody_rc_select_form(rc_data, !!form)

      if (nrow(form_data) == 0) {

        forms_count <- tibble::tibble(
          "{id_var}" := NA_character_,
          redcap_event_name = NA_character_,
          redcap_form_name = NA_character_,
          n = NA_integer_
        ) |>
          dplyr::filter(!is.na(.data[[id_var]]))

      } else {

      forms_count <-
        form_data |>
        dplyr::count(.data[[id_var]], .data[["redcap_event_name"]]) |>
        dplyr::mutate(
          redcap_form_name = form,
          .before = n
        )

      }

      expected_events <-
        forms_events_mapping |>
        dplyr::filter(.data[["form"]] == .env$form) |>
        dplyr::pull("unique_event_name")

      expected_structure <-
        tidyr::expand_grid(
          "{id_var}" := subjects,
          redcap_event_name = expected_events,
          redcap_form_name = form
        )

      dplyr::full_join(
        forms_count, expected_structure,
        by = c(id_var, "redcap_event_name", "redcap_form_name")
      ) |>
        dplyr::mutate(
          n = tidyr::replace_na(n, 0)
        ) |>
        dplyr::arrange(.data[[id_var]])

    }
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      redcap_event_name = factor(
        redcap_event_name, levels = events
      ),
      redcap_form_name = factor(
        redcap_form_name, levels = forms
      )
    ) |>
    dplyr::arrange(redcap_event_name)

  if (is.null(save_path)) return(form_count_raw)

  form_count_list <-
    purrr::map(
      events,
      ~ form_count_raw |>
        dplyr::filter(
          redcap_event_name == .env$.
        ) |>
        tidyr::pivot_wider(
          names_from = "redcap_form_name",
          values_from = "n",
          values_fill = 0
        ) |>
        dplyr::select(-"redcap_event_name")
    ) |>
    purrr::set_names(events)

  wb <- openxlsx::createWorkbook()
  purrr::walk2(
    form_count_list,
    events,
    function(.x, .y) {
      openxlsx::addWorksheet(wb, .y)
      openxlsx::writeData(wb, sheet = .y, .x)
    }
  )
  openxlsx::saveWorkbook(
    wb, here::here(save_path, "form_count_list.xlsx"),
    overwrite = TRUE
  )


}
