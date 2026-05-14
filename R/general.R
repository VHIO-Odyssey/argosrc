argos_add_folders <- function() {
  folders <- list.dirs(here::here(), recursive = FALSE) |>
    basename()

  if (!any(folders == "quality")) {
    stop("No 'quality' folder found in this project")
  }

  dir.create(here::here("quality", "argosrc"))
  dir.create(here::here("quality", "argosrc", "completeness"))
  dir.create(here::here("quality", "argosrc", "plausibility"))

  file.copy(
    system.file(
      "argos_templates",
      "completeness.R",
      package = "argosrc"
    ),
    here::here(
      "quality",
      "argosrc",
      "completeness",
      stringr::str_c(odytools:::get_project_name(), "_completeness.R")
    )
  )

  file.copy(
    system.file(
      "argos_templates",
      "plausibility.R",
      package = "argosrc"
    ),
    here::here(
      "quality",
      "argosrc",
      "plausibility",
      stringr::str_c(odytools:::get_project_name(), "_plausibility.R")
    )
  )
}
