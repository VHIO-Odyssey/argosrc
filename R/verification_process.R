#' @title Build Argos Quality Folders and Templates
#' @description
#'   Create the `quality/argosrc` folder structure in the current project and
#'   copy default verification templates into it.
#'
#'   Use this function when initializing Argos review scaffolding in a project
#'   that already contains a top-level `quality` directory.
#'
#' @return A logical scalar indicating whether the last template file copy
#'   operation succeeded. The function is primarily used for its side effects:
#'   creating directories and copying template scripts.
#'
#'   The function stops with an error if a top-level `quality` folder is not
#'   present in the current project.
#'
#' @details
#'   The function creates the following directories under the current project
#'   root: `quality/argosrc` and `quality/argosrc/verification_results`.
#'
#'   It then copies bundled templates from the package `inst/argosrc_templates`
#'   directory (`verifications_master.R` and `verifications_adhoc.R`) and
#'   renames them using the project name obtained from
#'   `odytools:::get_project_name()`.
#'
#' @export
argos_add_folders <- function() {
  folders <- list.dirs(here::here(), recursive = FALSE) |>
    basename()
  if (!any(folders == "quality")) {
    stop("No 'quality' folder found in this project")
  }
  dir.create(here::here("quality", "argosrc"))
  dir.create(here::here("quality", "argosrc", "verification_results"))
  file.copy(
    system.file(
      "argosrc_templates",
      "verifications_master.R",
      package = "argosrc"
    ),
    here::here(
      "quality",
      "argosrc",
      stringr::str_c(odytools:::get_project_name(), "_verifications_master.R")
    )
  )
  file.copy(
    system.file(
      "argosrc_templates",
      "verifications_adhoc.R",
      package = "argosrc"
    ),
    here::here(
      "quality",
      "argosrc",
      stringr::str_c(odytools:::get_project_name(), "_verifications_adhoc.R")
    )
  )
}

# Plausability related general functions

# Internal helper to drop columns that are entirely NA. Used to keep issue
# tables (which end up in the Excel report produced by
# argos_write_verification_report()) free of columns that carry no
# information, regardless of which function generated the issues (automatic
# verifications, ad-hoc verifications, or completeness checks).
drop_all_na_cols <- function(df) {
  dplyr::select(df, dplyr::where(~ !all(is.na(.))))
}

# Internal helper to filter issues from the verified data and glue the issue text
filter_issues <- function(verified_data, issue_text) {
  verified_data |>
    dplyr::filter(!.data$.ok | is.na(.data$.ok)) |>
    dplyr::mutate(
      issue = glue::glue(issue_text, .open = "<<", .close = ">>")
    ) |>
    dplyr::select(
      1,
      # Selecciona por patrón por si s incluyen campos de formularios distintos
      tidyselect::matches(c(
        "^redcap_event_name",
        "^redcap_form_name",
        "^redcap_instance_number"
      )),
      .data$issue
    ) |>
    drop_all_na_cols()
}

# Internal helper to find valid set of arguments for each verification.
find_valid_candidates <- function(
  arguments_metadata,
  candidates_mapping,
  complexity,
  metadata,
  rc_data
) {
  # Para detectar si los argumentos corresponden a variables de redcap hay que
  # quedarse con los asi definidos y extraer las constantes.
  redcap_fields <- arguments_metadata |>
    dplyr::filter(.data$argument_type == "redcap_field") |>
    dplyr::pull("argument") |>
    unique()

  candidates_list <-
    purrr::map(
      1:nrow(candidates_mapping),
      ~ candidates_mapping[., ] |>
        dplyr::select(tidyselect::all_of(redcap_fields))
    )

  # Se mira si todas las variables candidatas están en el metadata.
  # Se mira por patrón.
  candidates_index <-
    purrr::map_lgl(
      candidates_list,
      function(candidates) {
        purrr::map_lgl(
          candidates,
          ~ stringr::str_detect(
            metadata$field_name,
            stringr::str_c("^", ., "$")
          ) |>
            any()
        ) |>
          all()
      }
    )

  present_candidates <- candidates_list[candidates_index]

  if (length(present_candidates) == 0) {
    return(NA)
  }

  # Forms where the present candidates belong to
  distinct_forms <-
    purrr::map(
      present_candidates,
      ~ dplyr::filter(
        metadata,
        stringr::str_detect(
          .data$field_name,
          stringr::str_c("^", ., "$") |> stringr::str_c(collapse = "|")
        )
      ) |>
        dplyr::select(form_name) |>
        unique() |>
        unlist()
    )
  n_distinct_forms <- purrr::map_int(distinct_forms, length)

  # If intraform complexity, we make sure all variables from the same set belong to
  # the same form
  if (stringr::str_detect(complexity, "intraform")) {
    present_candidates <- present_candidates[n_distinct_forms == 1]

    if (length(present_candidates) == 0) return(NA)
  }

  # If interform complexity, we make sure at least two variables from the same set
  # belong to different forms.
  if (stringr::str_detect(complexity, "interform")) {
    present_candidates <- present_candidates[n_distinct_forms > 1]

    if (length(present_candidates) == 0) return(NA)
  }

  # If multiinstance complexity, we make sure at least one variable from the set
  # belongs to a repeating form.
  if (stringr::str_detect(complexity, "multiinstance")) {
    forms_events_mapping <- attr(rc_data, "forms_events_mapping")
    repeating <- attr(rc_data, "repeating")

    # Si el proyecto ni siquiera tiene formularios repetidos, no hay candidatos
    # válidos para verificaciones multiinstance.
    if (is.null(repeating)) {
      return(NA)
    }

    # Formularios repetidos
    repeating_forms <- na.omit(repeating$form_name) |> unique()

    # Posibles repeticiones de formulario debido a los eventos.
    if (!is.null(forms_events_mapping)) {
      # Formularios repetidos por pertecer a evento repertido
      repeating_events <- repeating |>
        dplyr::filter(is.na(.data$form_name)) |>
        dplyr::pull("event_name") |>
        unique()
      repeating_forms_event <- forms_events_mapping |>
        dplyr::filter(.data$unique_event_name %in% repeating_events) |>
        dplyr::pull("form") |>
        unique()
      # Formularios repetidos por aparecer en más de un evento (aunque ni el
      # evento ni el formulario sean repertidos per se)
      multievent_forms <-
        dplyr::count(forms_events_mapping, .data$form) |>
        dplyr::filter(.data$n > 1) |>
        dplyr::pull("form") |>
        unique()

      all_repeating_forms <- union(repeating_forms, repeating_forms_event) |>
        union(multievent_forms)
    } else {
      all_repeating_forms <- repeating_forms
    }

    is_repeating_form <- purrr::map_lgl(
      distinct_forms,
      ~ any(. %in% all_repeating_forms)
    )

    present_candidates <- present_candidates[is_repeating_form]

    if (length(present_candidates) == 0) return(NA)
  }

  present_candidates_match <-
    purrr::map(
      present_candidates,
      function(candidates_set) {
        purrr::map2(
          candidates_set,
          names(candidates_set),
          ~ dplyr::filter(
            metadata,
            stringr::str_detect(
              .data$field_name,
              stringr::str_c("^", ., "$")
            )
          ) |>
            dplyr::mutate(
              argument = .y
            ) |>
            dplyr::select(
              "argument",
              field_name = "field_name",
              field_type_cand = "field_type",
              field_choices_cand = "select_choices_or_calculations",
              field_validation_cand = "text_validation_type_or_show_slider_number"
            )
        ) |>
          purrr::list_rbind()
      }
    ) |>
    purrr::map(
      ~ dplyr::left_join(., arguments_metadata, by = "argument") |>
        dplyr::mutate(
          #All NA's are set to character so they return TRUE when comparing
          # arguments with candidates
          dplyr::across(
            tidyselect::everything(),
            ~ tidyr::replace_na(as.character(.), "NA")
          ),
          no_need_match = field_type == "NA",
          type_ok = field_type_cand == field_type,
          choices_ok = stringr::str_remove_all(field_choices_cand, " ") ==
            stringr::str_remove_all(field_choices, " "),
          validation_ok = field_validation_cand == field_validation,
          field_match = no_need_match | (type_ok & choices_ok & validation_ok)
        ) |>
        dplyr::select("argument", "field_name", "field_match")
    )

  valid_candidates_index <-
    purrr::map_lgl(
      present_candidates_match,
      ~ . |>
        dplyr::group_by(.data$argument) |>
        dplyr::summarise(arg_ok = any(.data$field_match)) |>
        dplyr::summarise(args_ok = all(.data$arg_ok)) |>
        dplyr::pull(args_ok)
    )

  valid_candidates <- present_candidates[valid_candidates_index]

  if (length(valid_candidates) == 0) {
    return(NA)
  }

  # Se añaden las constantes si las hubiere.
  if (any(arguments_metadata$argument_type == "constant")) {
    constants <-
      arguments_metadata |>
      dplyr::filter(.data[["argument_type"]] == "constant") |>
      dplyr::pull("argument") |>
      unique() |>
      purrr::map(~ tibble::tibble("{.}" := NA_character_)) |>
      purrr::list_cbind()

    # If the constants are defined in the mapping, they are added to the valid
    # candidates. Otherwise NA constants are added.
    if (all(names(constants) %in% names(candidates_mapping))) {
      purrr::map(
        valid_candidates,
        ~ dplyr::left_join(., candidates_mapping, by = redcap_fields)
      )
    } else {
      purrr::map(
        valid_candidates,
        ~ dplyr::bind_cols(., constants)
      )
    }
  } else {
    valid_candidates
  }
}

# Helper function to create human-readable verification descriptions based on
# the verification function and its arguments.
create_verification_description <- function(verif_fn, verif_arg, description) {
  if (verif_fn == "verif_1_1") {
    glue::glue(
      "{verif_arg$date1} is before (or equal to) {verif_arg$date2}"
    )
  } else if (verif_fn == "verif_1_2") {
    glue::glue(
      "{verif_arg$date1} is before (or equal to) {verif_arg$date2} for all instances"
    )
  } else if (verif_fn == "verif_1_3") {
    glue::glue(
      "{verif_arg$date1a} or {verif_arg$date1b} (the first one) is before (or equal to) {verif_arg$date2} for all instances"
    )
  } else if (verif_fn == "verif_1_4") {
    glue::glue(
      "{verif_arg$date1} is before (or equal to) {verif_arg$date2}"
    )
  } else if (verif_fn == "verif_2_1") {
    glue::glue(
      "The time between {verif_arg$date1} and {verif_arg$date2} is between {verif_arg$min_period} and {verif_arg$max_period} {verif_arg$unit}."
    )
  } else if (verif_fn == "verif_3_1") {
    glue::glue(
      "Variable(s) {stringr::str_c(verif_arg$var_name, collapse = ', ')} are equal to '{verif_arg$expected}'."
    )
  } else if (verif_fn == "verif_4_1") {
    glue::glue(
      "Variable(s) {stringr::str_c(verif_arg$var_name, collapse = ', ')} have the same value across all instances."
    )
  } else if (verif_fn == "verif_5_1") {
    glue::glue(
      "The value of {verif_arg$overall_response} must be consistent with the values provided in {verif_arg$target_response}, {verif_arg$no_target_response}, and {verif_arg$new_lesions}."
    )
  } else if (verif_fn == "verif_6_1") {
    glue::glue(
      "The last value of {verif_arg$last_fu_date} for a patient with {verif_arg$last_fu_status} = 'Alive' is within the last {verif_arg$time_limit} {verif_arg$unit}."
    )
  } else if (verif_fn == "verif_7_1") {
    glue::glue(
      "The last value of {verif_arg$visit_date} for a patient with missing {verif_arg$end_date} is within the last {verif_arg$time_limit} {verif_arg$unit}."
    )
  } else {
    description
  }
}

# Internal helper to attach rc_data-derived attributes to a verification result
# tibble. Used by both argos_check_verifications() and
# argos_run_ad_hoc_verifications() so that their outputs are compatible with
# argos_write_verification_report().
attach_rc_attributes <- function(result, rc_data) {
  project_info <- attr(rc_data, "project_info") |>
    dplyr::select("project_id", "project_title")
  attr(result, "redcap_project") <- project_info

  attr(result, "redcap_import_date") <- attr(rc_data, "import_date")

  reviewed_subjects <- attr(rc_data, "subjects")
  dags <- attr(rc_data, "dag")

  if (is.null(reviewed_subjects)) {
    attr(result, "reviewed_subjects") <- tibble::tibble(
      reviewed_subjects
    ) |>
      dplyr::arrange(.data$reviewed_subjects)
  } else {
    attr(result, "reviewed_subjects") <-
      tibble::tibble(reviewed_subjects) |>
      dplyr::left_join(
        attr(rc_data, "subjects_dag"),
        by = c("reviewed_subjects" = attr(rc_data, "id_var"))
      ) |>
      dplyr::left_join(
        dags,
        by = c("redcap_data_access_group" = "unique_group_name")
      ) |>
      dplyr::select(
        reviewed_subjects,
        site = "data_access_group_name"
      ) |>
      dplyr::arrange(.data$site, .data$reviewed_subjects)
  }
  result
}

#' @title Check REDCap Data for Verification Issues
#' @description
#'   Runs the verification catalogue on a REDCap data export and
#'   returns issue tables for each detected verification.
#'
#'   The function identifies valid argument candidates from project metadata,
#'   optionally augments constants with user-supplied values, executes
#'   compatible verification functions, and appends ad-hoc verification outputs
#'   when requested.
#'
#' @param rc_data A REDCap export object returned by `odytools::ody_rc_import()`.
#'   It must be a data frame-like object with, at minimum, the attributes
#'   `metadata`, `project_info`, and `import_date`. For verifications involving
#'   repeating instruments or events, the attributes `forms_events_mapping` and
#'   `repeating` must also be present.
#' @param constants_list A list of tibbles, or `NULL`. Each tibble must contain
#'   a `verif_fn` column naming the target verification in the form
#'   `"<id>_<version>"`, plus one column per verification argument. When
#'   provided, these values replace constant arguments derived from the master
#'   verification table for matching verifications. All arguments must be
#'   supplied in each tibble row to avoid ambiguity when matching candidate
#'   fields.
#' @param ad_hoc_verifications_path A character vector of paths to ad-hoc
#'   verification scripts, or `NULL`. When provided, scripts are sourced with
#'   [argos_run_ad_hoc_verifications()] and their results are appended to the
#'   automatic verification checks.
#' @param data_sets A named list of auxiliary objects, or `NULL`. These objects
#'   are passed to ad-hoc scripts via [argos_run_ad_hoc_verifications()] as
#'   `datasets` and are ignored when `ad_hoc_verifications_path` is `NULL`.
#'
#' @return A tibble with one row per automatic verification (and additional rows
#'   from ad-hoc scripts when provided).
#'   \describe{
#'     \item{verif_fn}{A character string identifying the verification function,
#'     usually formatted as `<id>_<version>`.}
#'     \item{verif_origin}{A character string indicating the origin of the
#'     verification: `"auto"` for verifications detected from
#'     `argos_verifications_master`, or `"adhoc"` for ad-hoc verifications.}
#'     \item{verif_type}{A character string with the verification type, taken
#'     from `argos_verifications_master`. `NA` for ad-hoc verifications.}
#'     \item{verif_arg}{A list-column of arguments used for execution. For
#'     unresolved checks this may contain missing values; for ad-hoc rows it can
#'     be `NA`.}
#'     \item{verification}{A character string with a human-readable
#'     verification description.}
#'     \item{execution}{A character string indicating execution status:
#'     `"ok"`, `"fail"`, or `"missing constants"`.}
#'     \item{n_issues}{An integer with the number of detected issues. It is
#'     `NA` when execution fails or constants are missing.}
#'     \item{issues}{A list-column of tibbles with issue-level records, or
#'     `NULL` for failed verifications.}
#'   }
#'
#'   The returned tibble carries attributes
#'   \describe{
#'     \item{redcap_project}{A tibble with `project_id` and `project_title`.
#'     Extracted from `attr(rc_data, "project_info")`.}
#'     \item{redcap_import_date}{A scalar copied from
#'     `attr(rc_data, "import_date")`.}
#'     \item{reviewed_subjects}{A tibble of reviewed subject identifiers and,
#'     when available, their site (`site`).}
#'   }
#'
#' @details
#'   Candidate arguments are first filtered by checking whether the referenced
#'   REDCap fields exist in the metadata and whether their field type, choices,
#'   and validation rules match the verification specification. The `complexity`
#'   field in the verification catalogue is then used to restrict candidate sets
#'   to intraform, interform, or multi-instance configurations.
#'
#'   Verifications with unresolved constants are not executed. Instead, they are
#'   returned with `execution = "missing constants"`. Executed verifications are
#'   called with `do.call()`. Runtime errors in verification functions are caught
#'   and represented as `execution = "fail"`, `n_issues = NA`, and
#'   `issues = NULL`. When `ad_hoc_verifications_path` is provided, ad-hoc
#'   results are appended after automatic checks.
#'
#' @export
argos_check_verifications <- function(
  rc_data,
  constants_list = NULL,
  ad_hoc_verifications_path = NULL,
  data_sets = NULL
) {
  rc_data_expr <- rlang::enexpr(rc_data)

  metadata <- attr(rc_data, "metadata")

  detected_verifications_v0 <-
    argos_verifications_master |>
    dplyr::mutate(
      valid_candidates = purrr::pmap(
        tibble::tibble(
          arguments_metadata = argos_verifications_master$arguments_metadata,
          candidates_mapping = argos_verifications_master$candidates_mapping,
          complexity = argos_verifications_master$complexity
        ),
        find_valid_candidates,
        metadata,
        rc_data
      )
    ) |>
    dplyr::filter(!is.na(.data$valid_candidates)) |>
    dplyr::mutate(
      verif_fn = stringr::str_c(.data$id, "_", .data$version)
    ) |>
    dplyr::select(
      "verif_fn",
      verif_type = "type",
      verif_arg = "valid_candidates",
      "description"
    ) |>
    tidyr::unnest("verif_arg")

  if (!is.null(constants_list)) {
    verif_extra_names <-
      purrr::map_chr(constants_list, ~ dplyr::pull(., "verif_fn")) |>
      unique()

    verif_extra_values <-
      purrr::map(
        verif_extra_names,
        function(x) {
          verif_index <- purrr::map_lgl(constants_list, ~ .$verif_fn == x)

          constants_list[verif_index] |>
            purrr::list_rbind() |>
            dplyr::select(-"verif_fn")
        }
      )

    verif_extra_args <-
      purrr::map(
        verif_extra_names,
        ~ argos_verifications_master |>
          dplyr::filter(
            stringr::str_c(.data$id, "_", .data$version) == .
          ) |>
          dplyr::select("arguments_metadata") |>
          tidyr::unnest("arguments_metadata") |>
          dplyr::filter(.data$argument_type == "redcap_field") |>
          dplyr::pull("argument") |>
          unique()
      )

    detected_verifications_modif <-
      purrr::pmap(
        tibble::tibble(
          verif_extra_names,
          verif_extra_args,
          verif_extra_values
        ),
        function(verif_extra_names, verif_extra_args, verif_extra_values) {
          verifs_to_complete <-
            detected_verifications_v0 |>
            dplyr::filter(.data[["verif_fn"]] == verif_extra_names)

          verifs_to_complete |>
            dplyr::mutate(
              verif_arg = purrr::map(
                .data$verif_arg,
                function(x) {
                  args_join <-
                    dplyr::left_join(
                      x,
                      verif_extra_values,
                      by = verif_extra_args
                    )

                  no_added_constants <-
                    args_join |>
                    dplyr::select(tidyselect::ends_with(".y")) |>
                    is.na() |>
                    all()

                  if (no_added_constants) {
                    args_join |>
                      dplyr::select(!tidyselect::ends_with(".y")) |>
                      dplyr::rename_with(~ stringr::str_remove(., ".x$"))
                  } else {
                    args_join |>
                      dplyr::select(!tidyselect::ends_with(".x")) |>
                      dplyr::rename_with(~ stringr::str_remove(., ".y$"))
                  }
                }
              )
            )
        }
      ) |>
      purrr::list_rbind() |>
      dplyr::arrange(.data[["verif_fn"]])

    detected_verifications_v0 <-
      dplyr::bind_rows(
        detected_verifications_v0 |>
          dplyr::filter(!.data[["verif_fn"]] %in% verif_extra_names),
        detected_verifications_modif
      ) |>
      dplyr::arrange(.data[["verif_fn"]])
  }

  detected_verifications <-
    detected_verifications_v0 |>
    dplyr::mutate(
      needs_constants = purrr::map_lgl(
        .data[["verif_arg"]],
        ~ any(purrr::map_lgl(., ~ any(is.na(.))))
      )
    )

  detected_verifications_ready <-
    detected_verifications |>
    dplyr::filter(!.data[["needs_constants"]]) |>
    dplyr::select(-"needs_constants")

  detected_verifications_undefined <-
    detected_verifications |>
    dplyr::filter(.data[["needs_constants"]]) |>
    dplyr::select(-"needs_constants") |>
    dplyr::mutate(
      execution = "missing constants",
      n_issues = NA
    )

  # Se comprueba si los argumentos hacen referncia a patrones de nombre. Si así
  # es, el argumento pasa a ser un vector con todos los nombres que cumplen el
  # patrón
  detected_verifications_expanded_args <-
    detected_verifications_ready |>
    dplyr::mutate(
      verif_arg = purrr::map(
        .data$verif_arg,
        function(args) {
          full_args <-
            purrr::map(
              args,
              ~ metadata |>
                dplyr::filter(
                  stringr::str_detect(
                    field_name,
                    stringr::str_c(
                      "^",
                      .,
                      "$"
                    )
                  )
                ) |>
                dplyr::pull(field_name)
            )
          purrr::map2(
            args,
            full_args,
            function(args, full_args) {
              if (length(full_args) > 1) full_args else args
            }
          )
        }
      )
    )

  detected_verifications_executed <-
    detected_verifications_expanded_args |>
    dplyr::mutate(
      issues = purrr::map2(
        .data$verif_fn,
        .data$verif_arg,
        ~ tryCatch(
          do.call(.x, c(.y, rc_data = rc_data_expr)),
          error = function(e) NULL
        )
      ),
      n_issues = purrr::map_int(
        .data$issues,
        ~ ifelse(is.null(.), NA_integer_, nrow(.))
      )
    ) |>
    dplyr::relocate(.data$n_issues, .before = "issues") |>
    dplyr::mutate(
      execution = ifelse(is.na(.data$n_issues), "fail", "ok"),
      .before = "n_issues"
    )

  if (nrow(detected_verifications_undefined) > 0) {
    argos_result <-
      dplyr::bind_rows(
        detected_verifications_executed,
        detected_verifications_undefined |>
          # Arguments table is transformed to list of lists for consistency
          dplyr::mutate(verif_arg = purrr::map(.data$verif_arg, as.list))
      ) |>
      dplyr::arrange(.data[["verif_fn"]])
  } else {
    argos_result <- detected_verifications_executed
  }

  argos_automatic <- argos_result |>
    # Se crea una descripción de la verificación más concreta basada en los
    # argumentos utilizados.
    dplyr::mutate(
      verification = purrr::pmap_chr(
        tibble::tibble(
          verif_fn = argos_result$verif_fn,
          verif_arg = argos_result$verif_arg,
          description = argos_result$description
        ),
        create_verification_description
      )
    ) |>
    dplyr::select(
      "verif_fn",
      "verif_type",
      "verif_arg",
      "verification",
      "execution",
      "n_issues",
      "issues"
    ) |>
    dplyr::mutate(verif_origin = "auto", .after = "verif_fn")

  if (!is.null(ad_hoc_verifications_path)) {
    ad_hoc_verifications <- argos_run_ad_hoc_verifications(
      rc_data,
      data_sets,
      ad_hoc_verifications_path
    )

    if (nrow(argos_automatic) > 0) {
      argos_final_result <- dplyr::bind_rows(
        argos_automatic,
        ad_hoc_verifications
      )
    } else {
      argos_final_result <- ad_hoc_verifications
    }
  } else {
    argos_final_result <- argos_automatic
  }

  attach_rc_attributes(argos_final_result, rc_data)
}


#' @title Append Completeness Results to a Verification Table
#' @description
#'   Converts the output of a completeness check into the standard verification
#'   row format and appends it to an existing verification results tibble.
#'
#'   For each REDCap form listed in the `reviewed_forms` attribute of
#'   `completeness_results`, the function creates one row summarising how many
#'   completeness issues were detected in that form and stores the individual
#'   issue records in a list-column. The resulting rows are bound to
#'   `previous_results` and the combined tibble is returned.
#'
#' @param previous_results A tibble of verification results already accumulated,
#'   typically the output of [argos_check_verifications()]. Must contain at
#'   least the columns `verif_fn`, `verification`, `execution`, `n_issues`, and
#'   `issues` (a list-column of tibbles).
#' @param completeness_results A tibble produced by a completeness check
#'   function. Must contain at least the columns `variable` (the field with the
#'   missing value), `completeness_issue` (a character column with values
#'   `"Regular missing"` or `"User missing"`), and a first column with the
#'   subject identifier. If user-missing codes are present, a `missing_value`
#'   column is also expected. The tibble must carry a `reviewed_forms` character
#'   attribute listing the REDCap form names that were reviewed.
#' @param verification_text A character scalar, or `NULL`. When provided, it
#'   replaces the auto-generated verification description
#'   `"All variables in the form '<form>' are completed following its branching
#'   logic."` for every form row. The same text is used for all forms. Defaults
#'   to `NULL`, which produces the auto-generated description.
#'
#' @return A tibble with the same columns as `previous_results` extended with
#'   one row per form in `attr(completeness_results, "reviewed_forms")`.
#'   Each appended row contains:
#'   \describe{
#'     \item{`verif_fn`}{The string `"completeness"`.}
#'     \item{`verification`}{A sentence describing the completeness check for
#'       the form. Defaults to
#'       `"All variables in the form '<form>' are completed following its
#'       branching logic."`, or `verification_text` when supplied.}
#'     \item{`execution`}{The string `"ok"`.}
#'     \item{`n_issues`}{Integer count of completeness issues detected in the
#'       form.}
#'     \item{`issues`}{A list-column element containing a tibble of individual
#'       issue records for that form. Each record includes the subject identifier
#'       column, available REDCap context columns (`redcap_event_name`,
#'       `redcap_form_name`, `redcap_instance_number`), and an `issue` column
#'       with a human-readable description of the missing field.}
#'   }
#'
#' @seealso [argos_check_verifications()], [argos_write_verification_report()]
#' @export
argos_add_completeness_results <- function(
  previous_results,
  completeness_results,
  verification_text = NULL
) {
  if (any(names(completeness_results) == "missing_value")) {
    completeness_issue_v0 <-
      completeness_results |>
      dplyr::mutate(
        issue = dplyr::case_when(
          completeness_issue == "Regular missing" ~ glue::glue(
            "{variable} is empty."
          ),
          completeness_issue == "User missing" ~ glue::glue(
            "{variable} has been marked as '{missing_value}'."
          ),
        )
      )
  } else {
    completeness_issue_v0 <-
      completeness_results |>
      dplyr::mutate(
        issue = glue::glue("{variable} is empty.")
      )
  }
  completeness_issue <-
    completeness_issue_v0 |>
    dplyr::select(
      1,
      # Selecciona por patrón por si s incluyen campos de formularios distintos
      tidyselect::matches(c(
        "^redcap_event_name",
        "^redcap_form_name",
        "^redcap_instance_number"
      )),
      .data$issue
    )
  completeness_nested <-
    purrr::map(
      attr(completeness_results, "reviewed_forms"),
      function(x) {
        issues <- completeness_issue |>
          dplyr::filter(stringr::str_detect(.data$redcap_form_name, x)) |>
          drop_all_na_cols()
        tibble::tibble(
          verif_fn = "completeness",
          verif_origin = "auto",
          verif_type = "completeness",
          verification = ifelse(
            is.null(verification_text),
            stringr::str_c(
              "All variables in the form '",
              x,
              "' are completed following its branching logic."
            ),
            verification_text
          ),
          execution = "ok",
          n_issues = nrow(issues),
          issues = list(issues)
        )
      }
    ) |>
    purrr::list_rbind()

  if (nrow(previous_results) == 0) {
    attr(completeness_nested, "redcap_project") <- attr(
      previous_results,
      "redcap_project"
    )
    attr(completeness_nested, "redcap_import_date") <- attr(
      previous_results,
      "redcap_import_date"
    )
    attr(completeness_nested, "reviewed_subjects") <- attr(
      previous_results,
      "reviewed_subjects"
    )

    return(completeness_nested)
  }

  dplyr::bind_rows(
    previous_results,
    completeness_nested
  )
}

#' @title Build an Ad-hoc Verification Result Entry
#' @description
#'   Create a verification-result tibble from an ad-hoc verification output.
#'   The function filters rows where `.ok` is `FALSE` or `NA`, builds a human-
#'   readable issue string with `glue::glue()`, and reshapes the output to the
#'   structure expected by `argos_check_verifications()`.
#'
#' @param verified_data A data frame or tibble containing verification results.
#'   It must include an `.ok` logical column, the subject identifier in the
#'   first column, and optionally REDCap context columns matching
#'   `^redcap_event_name`, `^redcap_form_name`, and
#'   `^redcap_instance_number`.
#' @param verification_description A character string describing the
#'   verification that produced `verified_data`.
#' @param issue_text A character string interpreted as a glue template. It can
#'   reference columns from `verified_data` to create row-level issue messages.
#' @param verif_type A character string indicating the type of verification.
#'   Must be one of `"plausibility"`, `"completeness"`, `"update"`, or
#'   `NA_character_` (default).
#'
#' @return A tibble with one row and the columns:
#'   \describe{
#'     \item{verification}{A character string with `verification_description`.}
#'     \item{verif_type}{A character string with the value of `verif_type`.}
#'     \item{execution}{A character string set to `"ok"`.}
#'     \item{n_issues}{An integer with the number of detected issue rows.}
#'     \item{issues}{A nested tibble containing identifier/context columns and
#'       an `issue` column.}
#'   }
#'
#'   The returned object has attribute `add_to_verifications = TRUE`, used by
#'   `argos_run_ad_hoc_verifications()` to discover ad-hoc outputs.
#'
#' @seealso [argos_run_ad_hoc_verifications()], [argos_check_verifications()]
#' @export
argos_add_to_verifications <- function(
  verified_data,
  verification_description,
  issue_text,
  verif_type = NA_character_
) {
  if (!is.na(verif_type)) {
    verif_type <- stringr::str_to_lower(verif_type)
  }
  if (
    !is.na(verif_type) &&
      !verif_type %in% c("plausibility", "completeness", "update")
  ) {
    rlang::abort(
      '`verif_type` must be NA, "plausibility", "completeness", or "update".'
    )
  }

  issues_tbl <-
    verified_data |>
    dplyr::filter(!.data$.ok | is.na(.data$.ok)) |>
    dplyr::mutate(
      issue = glue::glue(issue_text)
    ) |>
    dplyr::select(
      1, # La primera que siempre ha de ser el id.
      # Selecciona por patrón por si s incluyen campos de formularios distintos
      tidyselect::matches(c(
        "^redcap_event_name",
        "^redcap_form_name",
        "^redcap_instance_number"
      )),
      "issue"
    ) |>
    drop_all_na_cols() |>
    dplyr::mutate(
      verification = verification_description
    ) |>
    tidyr::nest(issues = -"verification") |>
    dplyr::mutate(
      verif_type = verif_type,
      execution = "ok",
      n_issues = purrr::map_int(.data$issues, nrow),
      .after = "verification"
    )

  attr(issues_tbl, "add_to_verifications") <- TRUE
  issues_tbl
}

#' @title Collect Ad-hoc Verification Results from Scripts
#' @description
#'   Sources ad-hoc verification scripts and collects objects marked for
#'   inclusion in the verification output.
#'
#'   The function exposes `rc_data` and `data_sets` to sourced scripts using the
#'   names `redcap_data` and `datasets`, respectively. After sourcing, it scans
#'   objects in the evaluation environment and keeps those with the
#'   `add_to_verifications` attribute (typically created with
#'   [argos_add_to_verifications()]). Selected objects are combined and labeled
#'   with their object name in `verif_fn`.
#'
#' @param rc_data A data frame-like object containing REDCap data. It is passed
#'   to sourced scripts as `redcap_data`.
#' @param data_sets A named list of auxiliary data objects, or `NULL`. It is
#'   passed to sourced scripts as `datasets`.
#' @param script_path A character vector of paths to `.R` scripts implementing
#'   ad-hoc verifications.
#'
#' @return A tibble produced by row-binding all collected ad-hoc verification
#'   objects.
#'   \describe{
#'     \item{verif_fn}{A character string with the object name found after
#'     sourcing each script.}
#'     \item{verification}{A character string describing the verification, as
#'     produced by [argos_add_to_verifications()].}
#'     \item{execution}{A character string indicating execution status
#'     (typically `"ok"`).}
#'     \item{n_issues}{An integer with the number of issues detected in each
#'     verification object.}
#'     \item{issues}{A list-column of tibbles containing issue-level rows.}
#'   }
#'
#' @details
#'   Scripts are sourced in the current function environment (`local =
#'   rlang::current_env()`). Objects are then inspected via their
#'   `add_to_verifications` attribute, so only outputs explicitly marked for
#'   verification reporting are returned.
#'
#' @seealso [argos_add_to_verifications()], [argos_check_verifications()]
#'
#' @export
argos_run_ad_hoc_verifications <- function(
  rc_data,
  data_sets = NULL,
  script_path
) {
  # Para evitar usar redcap_data y datasets como nombres de argumento.
  # Además, esto asegura que al ejecutarse los scripts ad-hoc nunca terminen
  # usando redcap_data o datasets del entorno global.
  redcap_data <- rc_data
  datasets <- data_sets

  purrr::walk(
    script_path,
    source,
    local = rlang::current_env()
  )

  current_objects <- ls()
  verif_index <- purrr::map_lgl(
    current_objects,
    ~ !is.null(
      attr(rlang::env_get(rlang::caller_env(3), .), "add_to_verifications")
    )
  )
  to_verifications <- current_objects[verif_index]
  result <- purrr::map(
    to_verifications,
    ~ rlang::env_get(rlang::caller_env(3), .) |>
      dplyr::mutate(
        verif_fn = .,
        .before = 1
      )
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(verif_origin = "adhoc") |>
    dplyr::select(
      "verif_fn",
      "verif_origin",
      "verif_type",
      "verification",
      "execution",
      "n_issues",
      "issues"
    )

  attach_rc_attributes(result, rc_data)
}

#' @title Write an Excel Verification Report
#' @description
#'   Create an Excel workbook summarising verification results (plausibility and
#'   completeness checks) and write it to disk.
#'
#'   The workbook contains the following sheets:
#'   \itemize{
#'     \item \code{project_info}: project metadata from the \code{redcap_project}
#'       attribute of \code{argos_results}, extended with the import date.
#'     \item \code{reviewed_subjects}: subject-level review data from the
#'       \code{reviewed_subjects} attribute of \code{argos_results}.
#'     \item \code{verifications}: one row per verification with columns
#'       \code{verif_num}, \code{verif_type} (see Details for possible values),
#'       \code{verification}, and \code{n_issues}.
#'     \item One additional sheet per verification with at least one detected
#'       issue, named \code{verif_<verif_num>}.
#'   }
#'
#' @param argos_results A tibble of verification results, typically the combined
#'   output of \code{argos_check_verifications()} and
#'   \code{argos_add_completeness_results()}. It must carry at least the columns
#'   \code{verif_fn}, \code{verification}, \code{n_issues}, and \code{issues}
#'   (a list-column of tibbles), and is expected to have the attributes
#'   \code{redcap_project}, \code{redcap_import_date}, and
#'   \code{reviewed_subjects}.
#' @param file_path A character scalar specifying the output file path or file
#'   stem. If it ends in \code{.xlsx}, the timestamp derived from
#'   \code{redcap_import_date} is inserted before the extension; otherwise the
#'   function appends the timestamp and the \code{.xlsx} extension.
#'
#' @return The result returned by \code{openxlsx2::wb_save()} after writing the
#'   Excel workbook to \code{here::here(final_file_path)}. As a side effect, an
#'   \code{.xlsx} file is created on disk.
#'
#' @details
#'   Only rows where \code{execution == "ok"} are included. Verifications are
#'   sorted first by \code{verif_type} (\code{"completeness"} before
#'   \code{"plausibility"}, then \code{NA}), then by \code{verif_origin}
#'   (\code{"adhoc"} before \code{"auto"}), and finally alphabetically by
#'   \code{verif_fn}. Rows are numbered sequentially in a \code{verif_num}
#'   column. Issue-specific sheets are written only for rows where
#'   \code{n_issues > 0}.
#'
#'   The \code{verif_type} column in the report reflects the value set when
#'   the verification was created: \code{"completeness"},
#'   \code{"plausibility"}, or \code{"update"} (via
#'   [argos_add_to_verifications()] or [argos_add_completeness_results()]),
#'   or \code{NA} when no type was specified. The \code{verif_origin} column is \code{"auto"} for
#'   verifications detected automatically by [argos_check_verifications()] and
#'   \code{"adhoc"} for verifications sourced from ad-hoc scripts.
#'
#'   The output filename is suffixed with a compact timestamp extracted from the
#'   \code{redcap_import_date} attribute using the pattern \code{YYYYMMDD_HHMM}
#'   without separators other than the underscore between date and time.
#'
#' @seealso [argos_check_verifications()], [argos_add_completeness_results()]
#' @export
argos_write_verification_report <- function(argos_results, file_path) {
  results_excel <-
    argos_results |>
    dplyr::filter(.data$execution == "ok") |>
    dplyr::mutate(
      verif_type = factor(
        .data$verif_type,
        levels = c("completeness", "plausibility", "update"),
      )
    ) |>
    dplyr::arrange(.data$verif_type, .data$verif_origin, .data$verif_fn) |>
    dplyr::mutate(
      verif_num = dplyr::row_number()
    )

  verif_num_issues <-
    results_excel |>
    dplyr::filter(.data$n_issues > 0) |>
    dplyr::pull(.data$verif_num)
  project_info <- attr(argos_results, "redcap_project") |>
    dplyr::mutate(
      import_date = attr(argos_results, "redcap_import_date")
    )
  reviewed_subjects <- attr(argos_results, "reviewed_subjects")
  wb <-
    openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("project_info") |>
    openxlsx2::wb_add_data_table(
      x = project_info,
      na = ""
    ) |>
    openxlsx2::wb_set_col_widths(
      cols = 1:ncol(project_info),
      widths = "auto"
    ) |>
    openxlsx2::wb_add_worksheet("reviewed_subjects") |>
    openxlsx2::wb_add_data_table(
      x = reviewed_subjects,
      na = ""
    ) |>
    openxlsx2::wb_set_col_widths(
      cols = 1:ncol(reviewed_subjects),
      widths = "auto"
    ) |>
    openxlsx2::wb_add_worksheet("verifications") |>
    openxlsx2::wb_add_data_table(
      x = results_excel |>
        dplyr::mutate(
          verif_source = stringr::str_c(
            .data$verif_origin,
            " (",
            .data$verif_fn,
            ")"
          )
        ) |>
        dplyr::select(
          "verif_num",
          "verif_type",
          "verif_source",
          "verification",
          "n_issues"
        ),
      na = ""
    ) |>
    openxlsx2::wb_set_col_widths(
      cols = 1:ncol(results_excel),
      widths = "auto"
    )
  # Add hyperlinks in verif_num column for verifications with issues
  for (v in verif_num_issues) {
    dims <- paste0("E", v + 1)
    wb <- openxlsx2::wb_add_hyperlink(
      wb,
      sheet = "verifications",
      dims = dims,
      target = sprintf("verif_%d!A1", v),
      tooltip = sprintf("Go to verif_%d", v),
      is_external = FALSE
    ) |>
      openxlsx2::wb_add_font(
        sheet = "verifications",
        dims = dims,
        color = openxlsx2::wb_color(hex = "FF0563C1"),
        underline = "single"
      )
  }
  for (i in verif_num_issues) {
    issues <- results_excel$issues[[i]]
    sheet_name <- paste0("verif_", results_excel$verif_num[i])
    back_link_dims <- paste0("A", nrow(issues) + 2)
    wb <- openxlsx2::wb_add_worksheet(wb, sheet_name) |>
      openxlsx2::wb_add_data_table(
        x = issues,
        na = ""
      ) |>
      openxlsx2::wb_set_col_widths(
        cols = 1:ncol(issues),
        widths = "auto"
      ) |>
      openxlsx2::wb_add_data(
        x = "\u2190 Back to verifications",
        dims = back_link_dims
      ) |>
      openxlsx2::wb_add_hyperlink(
        dims = back_link_dims,
        target = "verifications!A1",
        tooltip = "Back to verifications",
        is_external = FALSE
      ) |>
      openxlsx2::wb_add_font(
        dims = back_link_dims,
        color = openxlsx2::wb_color(hex = "FF0563C1"),
        underline = "single"
      )
  }
  import_date <- attr(argos_results, "redcap_import_date") |>
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

  cli::cli_alert_info(
    "Writing verification report to {.file {here::here(final_file_path)}}"
  )
  openxlsx2::wb_save(wb, here::here(final_file_path))
}
