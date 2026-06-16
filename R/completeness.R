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
    missing_codes$raw_value,
    collapse = "|"
  )

  if (nrow(needed_meta) > 0) {
    external_branching <- needed_meta |>
      dplyr::filter(
        stringr::str_detect(
          .data$branching_logic,
          "current-instance|user-role-name"
        )
      ) |>
      dplyr::pull("field_name")

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
        r_branch = stringr::str_replace_all(
          .data$branching_logic,
          "event-name",
          "redcap_event_name"
        ) |>
          # For external variables with structure [form_name][variable] the
          # [form_name] is removed
          stringr::str_replace("\\[[^\\[\\]]+\\](\\[[^\\[\\]]+\\])", "\\1") |>
          # Ensure all values are beteen ' and not "
          stringr::str_replace_all("\"", "'") |>
          stringr::str_replace_all("\\n", " ") |>
          # Any upper case AND / OR is lower cased to avoid potential confusion
          # with the missing data codes.
          stringr::str_replace_all(" AND ", " and ") |>
          stringr::str_replace_all(" OR ", " or ") |>
          stringr::str_replace_all(missing_value, "user_na") |>
          # Checkbox variables to especific check box column
          stringr::str_replace_all(
            "(\\[[^\\]\\[\\(\\)]+)\\(([^\\]\\[\\(\\)]+)\\)\\]",
            "\\1___\\2]"
          ) |>
          # Lowercase suffix after ___ to ensure consistency
          stringr::str_replace_all("(?<=___)\\w+", tolower) |>
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
          stringr::str_replace_all("(.*labelled::is_user_na.+)\\1+", "\\1") |>
          # RedCap checkbox checked to R TRUE
          stringr::str_replace_all(
            "(___[^=]+== *)'1'",
            "\\1TRUE"
          ) |>
          # RedCap checkbox unchecked to R FALSE
          stringr::str_replace_all(
            "(___[^=]+== *)''",
            "\\1FALSE"
          ),
        cond = stringr::str_c(
          .data$field_name,
          " = ",
          "\"",
          .data$r_branch,
          "\""
        )
      ) |>
      dplyr::pull("cond")

    conditions_list <- stringr::str_c(
      "list(",
      stringr::str_c(pre_list, collapse = ", "),
      ")"
    ) |>
      str2lang() |>
      eval()
  } else {
    conditions_list <- NULL
  }

  conditions_list
}

verify_completeness_form <- function(
  rc_data,
  current_form_name,
  conditions_list,
  user_na_is_data,
  missing_data_codes,
  check_for
) {
  id_var <- attr(rc_data, "id_var")

  current_form <- odytools::ody_rc_select_form(
    rc_data,
    !!current_form_name
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
        completeness_issue = NA_character_
      ) |>
      dplyr::filter(!is.na(.data$variable))

    return(empty_result)
  }

  if (!any(names(current_form) == "redcap_event_name")) {
    current_form <-
      current_form |>
      dplyr::mutate(
        redcap_event_name = NA_character_,
        .before = "redcap_form_name"
      )
  }

  current_variables_name <-
    current_form |>
    dplyr::select(
      -tidyselect::all_of(id_var),
      -"redcap_event_name",
      -"redcap_form_name",
      -"redcap_instance_type",
      -"redcap_instance_number",
      -stringr::str_c(current_form_name, "_complete")
    ) |>
    names()

  if (length(current_variables_name) == 0) {
    empty_result <-
      tibble::tibble(
        "{id_var}" := NA_character_,
        redcap_event_name = NA_character_,
        redcap_form_name = NA_character_,
        redcap_instance_type = NA_character_,
        redcap_instance_number = NA_character_,
        variable = NA_character_,
        completeness_issue = NA_character_
      ) |>
      dplyr::filter(!is.na(.data$variable))

    return(empty_result)
  }

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

      # Hay que añadir variables externas al formulario si así lo requiere el
      # branching logic
      cond_variables <- stringr::str_extract_all(
        as.character(current_condition),
        stringr::str_c(attr(rc_data, "metadata")$field_name, collapse = "|")
      ) |>
        unlist() |>
        unique()

      extra_form_variables <- cond_variables[
        !cond_variables %in% names(current_form)
      ]

      if (length(extra_form_variables) > 0) {
        extra_form_list <- odytools::ody_rc_select(
          rc_data,
          !!extra_form_variables
        )

        if (is.data.frame(extra_form_list)) {
          extra_form_list <- list(extra_form_list)
        }

        all_extra_is_unique <-
          purrr::map_lgl(
            extra_form_list,
            ~ all(is.na(.$redcap_instance_number))
          ) |>
          all()

        if (all_extra_is_unique) {
          extra_variables_info <-
            purrr::map(
              extra_form_list,
              ~ dplyr::select(
                .,
                -tidyselect::any_of(c(
                  "redcap_event_name",
                  "redcap_form_name",
                  "redcap_instance_type",
                  "redcap_instance_number"
                ))
              )
            ) |>
            purrr::reduce(
              dplyr::full_join,
              by = attr(rc_data, "id_var")
            )

          expanded_form <-
            dplyr::left_join(
              current_form,
              extra_variables_info,
              by = attr(rc_data, "id_var")
            )
        } else {
          expanded_form <- current_form
        }
      } else {
        expanded_form <- current_form
      }

      if (check_for %in% c("missing", "both")) {
        missing_values <-
          expanded_form |>
          # Safe way to define if the branching logic condition is met. If the
          # logic fails  it assumes the condition is met for all cases.
          # It returns NA for those cases where the variables involved in the
          # branching logic are missing. So, if a branching logic can not be
          # resolved it is assumed the condition is met and the completeness
          # check must be performed.
          safe_condition_definition(current_condition) |>
          dplyr::filter_out(!.data$meets_condition) |>
          dplyr::filter(na_fn(!!current_variable)) |>
          dplyr::mutate(
            variable = x,
            condition = current_condition_label,
            evaluable_condition = dplyr::if_else(
              is.na(.data$meets_condition),
              "No",
              "Yes"
            ),
            completeness_issue = dplyr::case_when(
              labelled::is_regular_na(!!current_variable) ~ "Regular missing",
              labelled::is_user_na(!!current_variable) ~ "User missing",
            ),
            missing_value = dplyr::case_when(
              labelled::is_regular_na(!!current_variable) ~ NA_character_,
              labelled::is_user_na(!!current_variable) ~ as.character(
                !!current_variable
              )
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
            "evaluable_condition",
            "completeness_issue",
            "missing_value"
          )
      } else {
        missing_values <- NULL
      }

      if (check_for %in% c("unexpected", "both")) {
        unexpected_values <-
          expanded_form |>
          # Safe way to define if the branching logic condition is met. If the
          # logic fails  it assumes the condition is met for all cases.
          # It returns NA for those cases where the variables involved in the
          # branching logic are missing. So, if a  branching logic can not
          # be resolved it is assumed the condition is met and the completeness
          # check must be performed.
          safe_condition_definition(current_condition) |>
          dplyr::filter_out(.data$meets_condition) |>
          dplyr::filter(!is.na(!!current_variable)) |>
          dplyr::mutate(
            variable = x,
            condition = current_condition_label,
            evaluable_condition = dplyr::if_else(
              is.na(.data$meets_condition),
              "No",
              "Yes"
            ),
            completeness_issue = "Unexpected",
            missing_value = NA_character_
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
            "evaluable_condition",
            "completeness_issue",
            "missing_value"
          )
      } else {
        unexpected_values <- NULL
      }

      dplyr::bind_rows(missing_values, unexpected_values)
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::left_join(
      missing_data_codes,
      by = c("missing_value" = "raw_value")
    ) |>
    dplyr::mutate(
      missing_value = .data$label
    ) |>
    dplyr::select(-"label")
}


#' @title Check Completeness of REDCap Data Forms
#' @description
#'   Verifies completeness of specified forms in REDCap data, considering
#'   user-defined missing values and branching logic conditions. For each
#'   variable, the function evaluates whether the corresponding REDCap
#'   branching logic condition is met and flags values that are absent when
#'   expected (`"missing"`) or present when not expected (`"unexpected"`).
#'
#' @param rc_data A REDCap data object with attributes `"metadata"`,
#'   `"missing"`, `"forms"`, and `"id_var"`, as produced by
#'   `odytools::ody_rc_import()`.
#' @param forms A character vector of form names to check, or `"All"` (the
#'   default) to check every form in `rc_data`.
#' @param user_na_is_data A logical scalar. If `TRUE` (the default),
#'   user-defined missing values (declared missing codes) are treated as
#'   non-missing data, so only regular `NA`s are flagged. If `FALSE`, any
#'   `NA`-like value — including user-defined missing codes — is considered
#'   missing.
#' @param check_for A character string controlling which completeness issues
#'   to flag. One of:
#'   \describe{
#'     \item{`"missing"`}{Variables whose branching logic condition is met but
#'       whose value is absent.}
#'     \item{`"unexpected"`}{Variables whose branching logic condition is not
#'       met but whose value is present.}
#'     \item{`"both"`}{Both missing and unexpected values.}
#'   }
#' @param extra_conditions_list An optional named list of additional branching
#'   logic conditions expressed as R character strings. Names must match
#'   variable names in `rc_data`. If a variable is already covered by the
#'   metadata branching logic, the entry in `extra_conditions_list` takes
#'   precedence and replaces it.
#' @param format A character string specifying the output format. One of:
#'   \describe{
#'     \item{`"raw"`}{Returns internal variable names, the branching logic
#'       condition string, and the `evaluable_condition` flag.}
#'     \item{`"friendly"`}{Replaces internal names with human-readable labels
#'       (field labels, instrument labels, event names) and drops internal
#'       columns not useful for reporting.}
#'   }
#' @param include_non_evaluable_conditions A logical scalar. If `TRUE` (the
#'   default), retains rows where the branching logic condition could not be
#'   evaluated because the variables it depends on are themselves missing;
#'   these rows are marked `evaluable_condition = "No"`. If `FALSE`, such
#'   rows are silently dropped and the `evaluable_condition` column is removed
#'   from the output.
#'
#' @return A tibble where each row corresponds to a completeness issue
#'   detected for a specific subject, variable, and (if applicable) event and
#'   form instance. Columns depend on the `format` and
#'   `include_non_evaluable_conditions` arguments:
#'   \describe{
#'     \item{`id_var`}{Subject identifier (column name matches the project's
#'       record ID field).}
#'     \item{`redcap_event_name` / `event`}{Event identifier (`raw`) or label
#'       (`friendly`). `NA` for non-longitudinal projects.}
#'     \item{`redcap_form_name` / `form`}{Form identifier (`raw`) or label
#'       (`friendly`).}
#'     \item{`redcap_instance_type`, `redcap_instance_number` / `form_instance`}{
#'       Repeat instrument metadata.}
#'     \item{`variable` / `field`}{Variable name (`raw`) or field label
#'       (`friendly`).}
#'     \item{`condition`}{The branching logic condition as an R expression
#'       string. `"Allways"` when no branching logic applies. Only present in
#'       `"raw"` format.}
#'     \item{`evaluable_condition`}{`"Yes"` if the condition could be resolved,
#'       `"No"` if it could not (dependent variables were missing). Only
#'       present when `include_non_evaluable_conditions = TRUE` and
#'       `format = "raw"`.}
#'     \item{`completeness_issue`}{One of `"Regular missing"`, `"User missing"`,
#'       or `"Unexpected"`.}
#'     \item{`missing_value`}{Label of the user-defined missing code when
#'       `completeness_issue` is `"User missing"`. Column is omitted entirely
#'       if no user-defined missing values are detected.}
#'   }
#'   When `format = "raw"`, the returned tibble carries a `"reviewed_forms"`
#'   attribute listing the forms that were checked.
#'
#' @details
#'   ## Branching logic resolution
#'
#'   REDCap branching logic is translated into R expressions and evaluated
#'   row-by-row to determine whether each variable is expected to be present
#'   for each case. The evaluation follows a conservative approach with two
#'   distinct failure modes:
#'
#'   - **Expression error** (e.g. branching logic contains invalid or
#'     unsupported syntax): the condition is assumed to be met for all cases,
#'     so completeness is checked for everyone.
#'
#'   - **Unresolvable branching** (the variables referenced in the branching
#'     logic are themselves missing for a given case): the condition evaluates
#'     to `NA` for that case. By default (`include_non_evaluable_conditions =
#'     TRUE`), these rows are retained and flagged with `evaluable_condition =
#'     "No"`. Set
#'     `include_non_evaluable_conditions = FALSE` to exclude them.
#'
#' @seealso [argos_count_forms()], [argos_write_forms_matrix()]
#' @export
argos_check_completeness <- function(
  rc_data,
  forms = "All",
  user_na_is_data = TRUE,
  check_for = c("missing", "unexpected", "both"),
  extra_conditions_list = NULL,
  format = c("raw", "friendly"),
  include_non_evaluable_conditions = TRUE
) {
  check_for <- rlang::arg_match(check_for)
  format <- rlang::arg_match(format)

  metadata <- attr(rc_data, "metadata")
  missing_data_codes <- attr(rc_data, "missing")
  conditions_list <- get_conditions_from_metadata(
    metadata,
    missing_data_codes
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

  if (any(forms == "All")) {
    forms <- attr(rc_data, "forms")$instrument_name
  }

  completeness_result <-
    purrr::map(
      forms,
      ~ verify_completeness_form(
        rc_data,
        .,
        conditions_list,
        user_na_is_data,
        missing_data_codes,
        check_for
      ),
      .progress = "Argos is searching \U1F415"
    ) |>
    purrr::list_rbind() |>
    dplyr::relocate(.data[["condition"]], .after = "variable") |>
    dplyr::arrange(.data[[attr(rc_data, "id_var")]])

  if (all(is.na(completeness_result$missing_value))) {
    completeness_result <-
      completeness_result |>
      dplyr::select(-"missing_value")
  }

  if (!include_non_evaluable_conditions) {
    completeness_result <-
      completeness_result |>
      dplyr::filter(.data$evaluable_condition == "Yes") |>
      dplyr::select(-"evaluable_condition")
  }

  if (format == "raw") {
    attr(completeness_result, "reviewed_forms") <- forms
    return(completeness_result)
  }

  field_label <- metadata |>
    dplyr::select("field_name", "field_label")
  form_names <- attr(rc_data, "forms")
  if (!is.null(attr(rc_data, "events"))) {
    event_names <- attr(rc_data, "events") |>
      dplyr::select("event_name", "unique_event_name")
  } else {
    event_names <- tibble::tibble(
      event_name = NA_character_,
      unique_event_name = NA_character_
    )
  }

  completeness_result |>
    dplyr::left_join(field_label, by = c("variable" = "field_name")) |>
    dplyr::relocate(.data$field_label, .after = "variable") |>
    dplyr::rename(field = .data$field_label) |>
    dplyr::left_join(
      form_names,
      by = c("redcap_form_name" = "instrument_name")
    ) |>
    dplyr::relocate(.data$instrument_label, .after = "redcap_form_name") |>
    dplyr::select(-"redcap_form_name") |>
    dplyr::rename(form = .data$instrument_label) |>
    dplyr::left_join(
      event_names,
      by = c("redcap_event_name" = "unique_event_name")
    ) |>
    dplyr::relocate(.data$event_name, .after = "redcap_event_name") |>
    dplyr::select(-"redcap_event_name") |>
    dplyr::rename(
      event = .data$event_name,
      form_instance = .data$redcap_instance_number
    ) |>
    dplyr::select(
      tidyselect::all_of(c(attr(rc_data, "id_var"))),
      "event",
      "form",
      "form_instance",
      "field",
      "completeness_issue",
      tidyselect::any_of("missing_value")
    ) |>
    dplyr::rename_with(~ stringr::str_to_title(gsub("_", " ", .)))
}


#' @title Compute Per-Subject Form Counts Across Events
#' @description
#'   Builds a completion matrix from REDCap data by counting how many times each
#'   form appears for each subject within each event.
#'
#'   The function ensures the expected subject-event-form structure is complete
#'   by joining observed counts with the form-event mapping stored in
#'   `rc_data` attributes, filling missing combinations with `0`.
#'
#' @param rc_data A data frame returned by `odytools::ody_rc_import()`, with
#'   REDCap metadata stored as attributes. It must include `id_var`, `subjects`,
#'   `forms`, `events` (for longitudinal projects), and `forms_events_mapping`.
#'
#' @return A tibble with one row per subject-event-form combination and columns:
#'   `id_var` (project record identifier column), `redcap_event_name`,
#'   `redcap_form_name`, and `n` (integer count of observed form instances).
#'   For non-longitudinal projects, `redcap_event_name` is `NA`.
#'
#'   The returned tibble includes attributes copied from `rc_data`:
#'   `events`, `forms`, `redcap_import_date`, and `project_info`.
#'
#' @details
#'   For each form, the function extracts form-level records with
#'   [odytools::ody_rc_select_form()], counts rows by subject and event, and
#'   expands to all expected subject-event combinations defined by
#'   `forms_events_mapping`. Unobserved combinations are assigned `n = 0`.
#'
#' @seealso [argos_write_forms_matrix()]
#' @export
argos_count_forms <- function(rc_data) {
  id_var <- attr(rc_data, "id_var")
  subjects <- attr(rc_data, "subjects")
  forms <- attr(rc_data, "forms")$instrument_name
  events <- attr(rc_data, "events")$unique_event_name
  forms_events_mapping <- attr(rc_data, "forms_events_mapping")

  if (is.null(events)) {
    events <- NA_character_

    forms_events_mapping <-
      tibble::tibble(
        unique_event_name = NA_character_,
        form = forms
      )
  }

  forms_count <- purrr::map(
    forms,
    function(form) {
      form_data <- odytools::ody_rc_select_form(rc_data, !!form)

      if (!any(names(form_data) == "redcap_event_name")) {
        form_data$redcap_event_name <- NA_character_
      }

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
          dplyr::count(.data[[id_var]], .data$redcap_event_name) |>
          dplyr::mutate(
            redcap_form_name = form,
            .before = "n"
          )
      }

      expected_events <-
        forms_events_mapping |>
        dplyr::filter(.data$form == .env$form) |>
        dplyr::pull("unique_event_name")

      expected_structure <-
        tidyr::expand_grid(
          "{id_var}" := subjects,
          redcap_event_name = expected_events,
          redcap_form_name = form
        )

      dplyr::full_join(
        forms_count,
        expected_structure,
        by = c(id_var, "redcap_event_name", "redcap_form_name")
      ) |>
        dplyr::mutate(
          n = tidyr::replace_na(.data$n, 0)
        ) |>
        dplyr::arrange(.data[[id_var]])
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      redcap_event_name = factor(
        .data$redcap_event_name,
        levels = events
      ),
      redcap_form_name = factor(
        .data$redcap_form_name,
        levels = forms
      )
    ) |>
    dplyr::arrange(.data$redcap_event_name)

  attr(forms_count, "events") <- attr(rc_data, "events")
  attr(forms_count, "forms") <- attr(rc_data, "forms")
  attr(forms_count, "redcap_import_date") <- attr(rc_data, "import_date")
  attr(forms_count, "project_info") <- attr(rc_data, "project_info")

  forms_count
}

#' @title Write Form Completion Matrices to an Excel Workbook
#' @description
#'   Creates an `.xlsx` workbook with one worksheet per REDCap event, where each
#'   row is a subject and each column is a form label. Cell values are the number
#'   of observed instances for each subject-form combination.
#'
#'   The workbook is styled to highlight completion counts (`0` in gray,
#'   `> 0` in light blue), and the output filename is suffixed with the REDCap
#'   import timestamp extracted from `forms_count` attributes.
#'
#' @param forms_count A tibble generated by [argos_count_forms()] containing
#'   `redcap_event_name`, `redcap_form_name`, and `n`, plus attributes `forms`,
#'   `events` (or `NULL` for non-longitudinal projects), `project_info`, and
#'   `redcap_import_date`.
#' @param file_path A character string specifying the output file path. If it
#'   ends with `.xlsx`, the timestamp is inserted before the extension;
#'   otherwise, `.xlsx` is appended.
#
#'
#' @details
#'   Event names are taken from the `events` attribute and mapped to display
#'   labels; if multiple arms are present, worksheet names include the arm
#'   number. For non-longitudinal projects (`events` is `NULL`), a single event
#'   is derived from `project_info$project_title`.
#'
#' @seealso [argos_count_forms()]
#' @export
argos_write_forms_matrix <- function(forms_count, file_path) {
  forms <- attr(forms_count, "forms")$instrument_name
  forms_dict <- attr(forms_count, "forms")$instrument_label |>
    purrr::set_names(forms)
  events_attr <- attr(forms_count, "events")
  events <- events_attr$unique_event_name

  if (is.null(events_attr)) {
    events_attr <- tibble::tibble(
      arm_num = 1,
      event_name = attr(forms_count, "project_info")$project_title,
      unique_event_name = attr(forms_count, "project_info")$project_title
    )
  }
  # If there is more than one arm, append the arm number to the event name
  if (length(unique(events_attr$arm_num)) > 1) {
    events_attr <-
      events_attr |>
      dplyr::mutate(
        event_name = stringr::str_c(
          .data$event_name,
          " (Arm ",
          .data$arm_num,
          ")"
        )
      )
  }

  events_dict <- events_attr$event_name |>
    purrr::set_names(events)

  form_count_list <-
    purrr::map(
      events,
      ~ forms_count |>
        dplyr::filter(
          redcap_event_name == .env$.
        ) |>
        tidyr::pivot_wider(
          names_from = "redcap_form_name",
          values_from = "n",
          values_fill = 0
        ) |>
        dplyr::select(-"redcap_event_name") |>
        dplyr::rename_with(~ forms_dict[.], -1) |>
        dplyr::rename("Patient ID" = 1)
    ) |>
    purrr::set_names(events_dict[events])

  wb <- openxlsx::createWorkbook()
  bg_style_0 <- openxlsx::createStyle(fgFill = "#D3D3D3")
  bg_style <- openxlsx::createStyle(fgFill = "#ADD8E6")

  purrr::walk2(
    form_count_list,
    names(form_count_list),
    function(form_tbl, form_name) {
      openxlsx::addWorksheet(wb, form_name)
      openxlsx::writeDataTable(
        wb,
        sheet = form_name,
        form_tbl,
        tableStyle = "TableStyleLight9"
      )
      openxlsx::freezePane(
        wb,
        sheet = form_name,
        firstActiveRow = NULL,
        firstActiveCol = 2
      )

      zero_cells <- which(form_tbl[-1] == 0, arr.ind = TRUE)
      if (nrow(zero_cells) > 0) {
        rows <- zero_cells[, 1] + 1
        cols <- zero_cells[, 2] + 1

        openxlsx::addStyle(
          wb,
          sheet = form_name,
          style = bg_style_0,
          rows = rows,
          cols = cols,
          gridExpand = FALSE,
          stack = TRUE
        )
      }
      non_zero_cells <- which(form_tbl[-1] > 0, arr.ind = TRUE)
      if (nrow(non_zero_cells) > 0) {
        rows <- non_zero_cells[, 1] + 1
        cols <- non_zero_cells[, 2] + 1

        openxlsx::addStyle(
          wb,
          sheet = form_name,
          style = bg_style,
          rows = rows,
          cols = cols,
          gridExpand = FALSE,
          stack = TRUE
        )
      }
    }
  )

  import_date <- attr(forms_count, "redcap_import_date") |>
    stringr::str_extract("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}") |>
    stringr::str_replace_all(" ", "_") |>
    stringr::str_remove_all("[-:]")

  if (stringr::str_detect(file_path, "\\.xlsx$")) {
    final_file_path <- stringr::str_replace(
      file_path,
      "\\.xlsx$",
      paste0("_", import_date, ".xlsx")
    )
  } else {
    final_file_path <- stringr::str_c(
      file_path,
      "_",
      import_date,
      ".xlsx"
    )
  }

  openxlsx::saveWorkbook(wb, here::here(final_file_path), overwrite = TRUE)
}
