#' @title Build Argos Quality Folders and Templates
#' @description
#'   Create the `quality/argosrc` folder structure in the current project and
#'   copy default completeness and plausibility analysis templates into it.
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
#'   root: `quality/argosrc`, `quality/argosrc/completeness`, and
#'   `quality/argosrc/plausibility`.
#'
#'   It then copies bundled templates from the package `inst/argos_templates`
#'   directory and renames them using the project name obtained from
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
  dir.create(here::here("quality", "argosrc", "completeness"))
  dir.create(here::here("quality", "argosrc", "plausibility"))

  file.copy(
    system.file(
      "argosrc_templates",
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
      "argosrc_templates",
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
