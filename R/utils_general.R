
filter_issues <- function(verified_data, issue) {

  verified_data |>
    dplyr::filter(!.data$.ok | is.na(.data$.ok)) |>
    dplyr::mutate(issue = glue::glue(issue, .open = "<<", .close = ">>")) |>
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


determine_verification_match <- function(verification, metadata) {

  involved_variables <- verification$involved_variables[[1]]

  candidates_list <-
    purrr::map(
      involved_variables$field_alias,
      ~ metadata |>
        dplyr::filter(field_name %in% .) |>
        dplyr::select(
          field_name,
          form_name,
          field_type,
          select_choices_or_calculations,
          text_validation_type_or_show_slider_number
        )
    )

  # Se tiene que haber encontrado una y solo una candidato por variable involucrada
  one_detection_per_field <- all(purrr::map_int(candidates_list, nrow) == 1)

  if (!one_detection_per_field) return(NA)

  # Todas las candidatas tienen que haber salido del mismo formulario si la
  # complejidad es monoform
  if (verification$complexity == "monoform") {

    detected_forms <-
      purrr::list_rbind(candidates_list)$form_name |>
      unique() |>
      length()

    if (detected_forms != 1) return(NA)

  }

  involved_variables_list <-
    purrr::map(
      1:nrow(involved_variables),
      ~ involved_variables[.,]
    )

  verification_arg <-
    purrr::map2(
      candidates_list, involved_variables_list,
      function(x, y) {

        perfect_match <- c(
          same_field_type = x$field_type == y$field_type,
          same_choices =
            x$select_choices_or_calculations == y$select_choices_or_calculations |
            (is.na(x$select_choices_or_calculations) & is.na(y$select_choices_or_calculations)),
          same_validation =
            x$text_validation_type_or_show_slider_number == y$text_validation_type_or_show_slider_number |
            (is.na(x$text_validation_type_or_show_slider_number) & is.na(y$text_validation_type_or_show_slider_number))
        ) |> all()

        tibble::tibble(
          verification_argument = y$verification_argument,
          detected_field = x$field_name,
          perfect_match
        )

      }
    ) |>
    purrr::list_rbind()


  if (!all(verification_arg$perfect_match)) return(NA)

  tibble::tibble(
    verification,
    verification_match = TRUE,
    verification_arg = list(
      verification_arg[, -3] |>
        tidyr::pivot_wider(
        names_from = verification_argument,
        values_from = detected_field
      )
    )
  )


}


argos_seek <- function(
    rc_data,
    run_verifications = TRUE,
    return_no_issues_verif = FALSE

  ) {

  rc_data_expr <- rlang::enexpr(rc_data)

  detected_verifications <-
    purrr::map(1:nrow(verifications_master), ~ verifications_master[., ]) |>
    purrr::map(determine_verification_match, attr(rc_data, "metadata")) |>
    purrr::list_rbind() |>
    dplyr::filter(verification_match) |>
    dplyr::mutate(
      verification_fn = stringr::str_c(verification, "_", version)
    ) |>
    dplyr::select(verification_fn, verification_arg, description)

  if (run_verifications) {

    runned_verifications <-
      detected_verifications |>
      dplyr::mutate(
        issues = purrr::map2(
          verification_fn, verification_arg,
          ~ do.call(.x, c(.y, rc_data = rc_data_expr))
        )
      ) |>
      dplyr::mutate(
        n_issues = purrr::map_int(issues, nrow),
        .before = issues
      )

    if (return_no_issues_verif) {

      runned_verifications

    } else {

      runned_verifications |>
        dplyr::filter(n_issues > 0)

    }


  } else {

    detected_verifications

  }

}




