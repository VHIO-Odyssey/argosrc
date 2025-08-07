
# This functions is completely specific to the creation of the verifications
# master. That is why it is here.
format_verification_info <- function(arguments, candidates) {

  splited_arguments <-
    arguments |>
    stringr::str_split("\n") |>
    unlist()

  arguments_tbl <-
    purrr::map(
      splited_arguments,
      function(x) {

        argument <- stringr::str_split(x, "::") |> unlist()

        metadata <-
          odytools::ody_rc_get_metadata(
            Sys.getenv(
              stringr::str_c(argument[3], "_api_key") |>
                stringr::str_to_upper()
            )
          ) |>
          dplyr::filter(field_name == argument[2])

        tibble::tibble(
          argument = argument[1],
          field_type = metadata$field_type,
          field_choices = metadata$select_choices_or_calculations,
          field_validation = metadata$text_validation_type_or_show_slider_number,
        )

      }
    ) |>
    purrr::list_rbind()

  candidates_groups <-
    candidates |>
    stringr::str_split("\n") |>
    unlist() |>
    purrr::map(~stringr::str_c("c(", ., ")") |> str2lang() |> eval())

  candidate_fields <-
    purrr::map(
      arguments_tbl$argument,
      function(arg) {
        purrr::map_chr(candidates_groups, ~ .[arg])
      }
    )

  arguments_tbl |>
    dplyr::mutate(candidate_fields, .after = "argument")

}



plausibility_verifications_master <-
  readr::read_csv2(here::here("data-raw", "plausibility_verifications_master.csv")) |>
  dplyr::mutate(
    arguments_info = purrr::map2(arguments, candidate_fields, format_verification_info)
  ) |>
  dplyr::select(
    id, version, description, complexity, arguments_info
  )


usethis::use_data(plausibility_verifications_master, overwrite = TRUE)
