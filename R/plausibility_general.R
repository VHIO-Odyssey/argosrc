# Plausability related general functions


# Internal helper to filter issues from the verified data and glue the issue text
filter_issues <- function(verified_data, issue_text) {

  verified_data |>
    dplyr::filter(!.data$.ok | is.na(.data$.ok)) |>
    dplyr::mutate(
      issue = glue::glue(issue_text, .open = "<<", .close = ">>")
    ) |>
    dplyr::select(-".ok") |>
    dplyr::select(
      1,
      dplyr::any_of(c(
        "redcap_event_name",
        "redcap_form_name",
        "redcap_instance_type",
        "redcap_instance_number"
      )),
      issue
    )

}

# Internal helper to find valid set of arguments for each verification.
find_valid_candidates <- function(
    arguments_metadata,
    candidates_mapping,
    complexity,
    metadata) {

  # Para detectar si los argumentos corresponden a variables de redcap hay que
  # quedarse con los asi definidos y extraer las constantes.
  redcap_fields <- arguments_metadata |>
    dplyr::filter(.data[["argument_type"]] == "redcap_field") |>
    dplyr::pull(argument)

  candidates_list <-
    purrr::map(
      1:nrow(candidates_mapping), ~
        candidates_mapping[., ] |>
        dplyr::select(tidyselect::all_of(redcap_fields))
    )

  candidates_index <-
    purrr::map_lgl(
      candidates_list,
      ~ all(. %in% metadata$field_name)
    )

  present_candidates <- candidates_list[candidates_index]

  if (length(present_candidates) == 0) return(NA)


  # If intraform complexity, we make sure al variables from the same set belong to
  # the same form
  if (complexity == "intraform") {

    # Number of diferent forms where the present candidates belong to
    n_distinct_forms <-
      purrr::map_int(
        present_candidates,
        ~ dplyr::filter(metadata, field_name %in% .) |>
          dplyr::select(form_name) |>
          unique() |>
          nrow()
      )

    present_candidates <- present_candidates[n_distinct_forms == 1]

    if (length(present_candidates) == 0) return(NA)

  }

  present_candidates_match <-
    purrr::map(
      present_candidates,
      function(candidates_set) {
        purrr::map2(
          candidates_set, names(candidates_set),
          ~ dplyr::filter(metadata, field_name == .x) |>
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
          dplyr::across(tidyselect::everything(), ~ tidyr::replace_na(., "NA")),
          type_ok = field_type_cand == field_type,
          choices_ok = field_choices_cand == field_choices,
          validation_ok = field_validation_cand == field_validation,
          field_match = type_ok & choices_ok & validation_ok
        )|>
        dplyr::select("argument", "field_name", "field_match")
    )


  valid_candidates_index <-
    purrr::map_lgl(present_candidates_match, ~all(.$field_match))

  valid_candidates <- present_candidates[valid_candidates_index]

  if (length(valid_candidates) == 0) return(NA)


  # Se añaden las constantes si las hubiere.
  if (any(arguments_metadata$argument_type == "constant")) {

    purrr::map(
      valid_candidates,
      ~ dplyr::left_join(., candidates_mapping, by = redcap_fields)
    )
    # constants <-
    #   arguments_metadata |>
    #   dplyr::filter(.data[["argument_type"]] == "constant") |>
    #   dplyr::pull("argument") |>
    #   purrr::map(
    #     ~ tibble::tibble(
    #       "{.}" := NA_character_
    #     )
    #   ) |>
    #   purrr::list_cbind()
    #
    #
    # purrr::map(
    #   valid_candidates,
    #   ~ dplyr::bind_cols(., constants)
    # )

  } else {

    valid_candidates

  }

}

#' Check Plausibility of REDCap Data
#'
#' This function evaluates a REDCap dataset for plausibility issues by
#' applying a series of predefined verifications. It identifies valid
#' candidate fields based on metadata consistency, executes respective
#' verification functions, and returns detected issues.
#'
#' @param rc_data A data frame representing the REDCap export data. It must
#'   be obtained with \code{odytools::ody_rc_import} and contain a "metadata"
#'   attribute describing the data dictionary.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{verif_fn}{The name of the verification function applied.}
#'   \item{verif_arg}{The valid candidate arguments used for verification.}
#'   \item{description}{Description of the verification.}
#'   \item{n_issues}{Number of issues detected by the verification.}
#'   \item{issues}{A list-column of data frames detailing detected issues.}
#' }
#'
#' @details
#' The function uses global object `plausibility_verifications_master` to find
#' applicable verifications. It filters candidates by metadata consistency and
#' complexity ("intraform" constraints). Each verification function is called
#' with its respective arguments on `rc_data`, and the resulting issues are
#' collected.
#'
#' @export
argos_check_plausibility <- function(rc_data, extra_mapping = NULL) {

  rc_data_expr <- rlang::enexpr(rc_data)

  metadata <- attr(rc_data, "metadata")

  detected_verifications <-
    plausibility_verifications_master |>
    dplyr::mutate(
      valid_candidates = purrr::pmap(
        tibble::tibble(arguments_metadata, candidates_mapping, complexity),
        find_valid_candidates,
        metadata
      )
    ) |>
    dplyr::filter(!is.na(.data[["valid_candidates"]])) |>
    dplyr::mutate(
      verif_fn = stringr::str_c(.data[["id"]], "_", .data[["version"]])
    ) |>
    dplyr::select("verif_fn",  verif_arg = "valid_candidates", "description") |>
    tidyr::unnest("verif_arg") |>
    dplyr::mutate(
      needs_constants = purrr::map_lgl(
        .data[["verif_arg"]],
        ~ any(purrr::map_lgl(., ~ any(is.na(.))))
      )
    )

  detected_verifications |>
    dplyr::mutate(
      issues = purrr::map2(
        verif_fn, verif_arg,
        ~ do.call(.x, c(.y, rc_data = rc_data_expr))
      )
    ) |>
    dplyr::mutate(
      n_issues = purrr::map_int(issues, nrow),
      .before = "issues"
    )

}
