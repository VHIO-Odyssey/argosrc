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
    user_na_is_data
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
      } else {
        current_condition <- str2lang(current_condition_raw)
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
          missing_type = dplyr::case_when(
            labelled::is_regular_na(!!current_variable) ~ "Regular",
            labelled::is_user_na(!!current_variable) ~ "User defined",
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
          missing_type
        )


    }
  ) |>
    purrr::list_rbind()

}


argos_completeness <- function(rc_data) {

  metadata <- attr(rc_data, "metadata")
  missing_data_codes <- attr(rc_data, "missing")
  conditions_list <- get_conditions_from_metadata(
      metadata, missing_data_codes
    )

  forms <- attr(rc_data, "forms")$instrument_name
  purrr::map(
    forms,
    ~ verify_completeness_form(rc_data, ., conditions_list, TRUE),
    .progress = TRUE
  ) |>
    purrr::list_rbind()

}
