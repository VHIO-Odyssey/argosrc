# Plausability related general functions


# Internal helper to filter issues from the verified data and glue the issue text
filter_issues <- function(verified_data, issue_text) {

  verified_data |>
    dplyr::filter(!.data$.ok | is.na(.data$.ok)) |>
    dplyr::mutate(
      issue = glue::glue(issue_text, .open = "<<", .close = ">>")
    ) |>
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
    dplyr::pull(argument) |>
    unique()

  candidates_list <-
    purrr::map(
      1:nrow(candidates_mapping), ~
        candidates_mapping[., ] |>
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
          ~stringr::str_detect(
            metadata$field_name,
            stringr::str_c("^", ., "$")
          ) |>
            any()
        ) |>
          all()
      }
    )

  present_candidates <- candidates_list[candidates_index]

  if (length(present_candidates) == 0) return(NA)

  # Number of diferent forms where the present candidates belong to
  n_distinct_forms <-
    purrr::map_int(
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
        nrow()
    )

  # If intraform complexity, we make sure all variables from the same set belong to
  # the same form
  if (complexity == "intraform") {

    present_candidates <- present_candidates[n_distinct_forms == 1]

    if (length(present_candidates) == 0) return(NA)

  }

  # If interform complexity, we make sure at least two variables from the same set
  # belong to different forms.
  if (complexity == "interform") {

    present_candidates <- present_candidates[n_distinct_forms > 1]

    if (length(present_candidates) == 0) return(NA)

  }

  present_candidates_match <-
    purrr::map(
      present_candidates,
      function(candidates_set) {
        purrr::map2(
          candidates_set, names(candidates_set),
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
          choices_ok = field_choices_cand == field_choices,
          validation_ok = field_validation_cand == field_validation,
          field_match = no_need_match | (type_ok & choices_ok & validation_ok)
        )|>
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

  if (length(valid_candidates) == 0) return(NA)


  # Se añaden las constantes si las hubiere.
  if (any(arguments_metadata$argument_type == "constant")) {

    purrr::map(
      valid_candidates,
      ~ dplyr::left_join(., candidates_mapping, by = redcap_fields)
    )

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

  detected_verifications_v0 <-
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
    tidyr::unnest("verif_arg")

  if (!is.null(extra_mapping)) {

    verif_extra_names <-
      purrr::map_chr(extra_mapping, ~ dplyr::pull(., "verif_fn")) |>
      unique()

    verif_extra_values <-
      purrr::map(
        verif_extra_names,
        function(x) {

          verif_index <- purrr::map_lgl(extra_mapping, ~ .$verif_fn == x)

          extra_mapping[verif_index] |>
            purrr::list_rbind() |>
            dplyr::select(-"verif_fn")

        }
      )

    verif_extra_args <-
      purrr::map(
        verif_extra_names,
        ~ plausibility_verifications_master |>
          dplyr::filter(
            stringr::str_c(.data[["id"]], "_", .data[["version"]]) == .
          ) |>
          dplyr::select("arguments_metadata") |>
          tidyr::unnest("arguments_metadata") |>
          dplyr::filter(.data[["argument_type"]] == "redcap_field") |>
          dplyr::pull("argument")
      )


    detected_verifications_modif <-
      purrr::pmap(
        tibble::tibble(
          verif_extra_names, verif_extra_args, verif_extra_values
        ),
        function(verif_extra_names, verif_extra_args, verif_extra_values) {

          verifs_to_complete <-
            detected_verifications_v0 |>
            dplyr::filter(.data[["verif_fn"]] == verif_extra_names)

          verifs_to_complete |>
            dplyr::mutate(
              verif_arg = purrr::map(
                verif_arg,
                function(x) {
                  args_join <-
                    dplyr::left_join(
                      x, verif_extra_values, by = verif_extra_args
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
      n_issues = NA,
      issues = NA
    )

  # Se comprueba si los argumentos hacen referncia a patrones de nombre. Si así
  # es, el argumento pasa a ser un vector con todos los nombres que cumplen el
  # patrón
  detected_verifications_expanded_args <-
    detected_verifications_ready |>
    dplyr::mutate(
      verif_arg =  purrr::map(
        .data$verif_arg,
        function(args) {
          full_args <-
            purrr::map(
              args,
              ~ metadata |>
                dplyr::filter(
                  stringr::str_detect(
                    field_name, stringr::str_c(
                      "^", ., "$"
                    )
                  )
                ) |>
                dplyr::pull(field_name)
            )
          purrr::map2(
            args, full_args,
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
        verif_fn, verif_arg,
        ~ do.call(.x, c(.y, rc_data = rc_data_expr))
      ),
      n_issues = purrr::map_int(issues, nrow)
    ) |>
    dplyr::relocate(n_issues, .before = "issues")

  if (nrow(detected_verifications_undefined) > 0) {

    warning("Some verifications were not executed because of missing argument definitions. Please provide these via the extra_mapping argument.")

    dplyr::bind_rows(
      detected_verifications_executed,
      detected_verifications_undefined
    ) |>
      dplyr::arrange(.data[["verif_fn"]])

  } else {

    detected_verifications_executed

  }




}
