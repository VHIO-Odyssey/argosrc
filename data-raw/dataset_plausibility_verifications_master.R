
# This functions is completely specific to the creation of the verifications
# master. That is why it is here.
create_arguments_metadata <- function(arguments) {

  splited_arguments <-
    arguments |>
    stringr::str_split("\n") |>
    unlist()

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

}



# This functions is completely specific to the creation of the verifications
# master. That is why it is here.
create_candidates_mapping <- function(candidates) {

    candidates |>
    stringr::str_split("\n") |>
    unlist() |>
    purrr::map(
      function(x) {
        arg_vector <- stringr::str_c("c(", x, ")") |> str2lang() |> eval()
        arg_names <- names(arg_vector)
        names(arg_vector) <- NULL
        purrr::map2(
          arg_vector, arg_names,
          ~ tibble::tibble("{.y}" := .x)
        ) |>
          purrr::list_cbind()
      }
    ) |>
    purrr::list_rbind()

}



plausibility_verifications_master <-
  readr::read_csv2(here::here("data-raw", "plausibility_verifications_master.csv")) |>
  dplyr::mutate(
    arguments_metadata = purrr::map(arguments, create_arguments_metadata),
    candidates_mapping = purrr::map(candidate_fields, create_candidates_mapping)
  ) |>
  dplyr::select(
    id, version, description, complexity, arguments_metadata, candidates_mapping
  )


usethis::use_data(plausibility_verifications_master, overwrite = TRUE)
