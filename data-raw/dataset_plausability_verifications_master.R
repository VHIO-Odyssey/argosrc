
# functions ----

# These two functions are completely specific to the creation of the verifications
# master. That is why they are here.

format_involved_vars <- function(involved_variables) {

  involved_vars_list <-
    involved_variables |>
    stringr::str_split("\\|") |>
    unlist() |>
    purrr::map(
      ~ stringr::str_split(., ",") |>
        unlist() |>
        stringr::str_trim()
    )

  names(involved_vars_list) <- purrr::map_chr(involved_vars_list, 1)

  involved_vars_list

}

get_reference_metadata <- function(involved_variables, metadata_origin) {

  metadata <- odytools::ody_rc_get_metadata(
    Sys.getenv(stringr::str_c(metadata_origin, "_API_KEY"))
  )

  purrr::map(
    involved_variables,
    ~ metadata |>
      dplyr::filter(field_name == .[1]) |>
      dplyr::mutate(
        field_alias = list(.)
      ) |>
      dplyr::select(
        verification_argument = field_name,
        field_alias,
        field_type,
        select_choices_or_calculations,
        text_validation_type_or_show_slider_number
      )
  ) |>
    purrr::list_rbind()

}


# dataset ----

plausability_verifications_master <-
  readr::read_csv2(here::here("data-raw", "plausability_verifications_master.csv")) |>
  dplyr::mutate(
    complexity = factor(complexity, levels = c("monoform", "multiform")),
    involved_variables = purrr::map(involved_variables, format_involved_vars) |>
    purrr::map2(
      metadata_origin, get_reference_metadata
    )
  ) |>
    dplyr::select(-metadata_origin)

usethis::use_data(plausability_verifications_master, overwrite = TRUE)
