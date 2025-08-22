


plausibility_verifications_master <-
  readr::read_csv2(here::here("data-raw", "plausibility_verifications_master.csv")) |>
  dplyr::mutate(
    arguments_metadata = purrr::map(arguments, create_arguments_metadata),
    candidates_mapping = purrr::map(candidate_fields, create_candidates_mapping)
  ) |>
  dplyr::select(
    id, version, description,complexity,
    arguments_metadata, candidates_mapping,
    comments,
  )


usethis::use_data(plausibility_verifications_master, overwrite = TRUE)
