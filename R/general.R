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
#'       \code{verif_num}, \code{verif_type} (\code{"plausibility"} or
#'       \code{"completeness"}), \code{verification}, and \code{n_issues}.
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
#'   sorted by \code{verif_fn} and numbered sequentially in a \code{verif_num}
#'   column. The \code{verif_type} column is derived from \code{verif_fn}: rows
#'   whose name contains \code{"verif"} are labelled \code{"plausibility"}; rows
#'   where \code{verif_fn} equals \code{"completeness"} are labelled
#'   \code{"completeness"}. Issue-specific sheets are written only for rows
#'   where \code{n_issues > 0}.
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
    dplyr::arrange(.data$verif_fn) |>
    dplyr::mutate(
      verif_num = 1:dplyr::n(),
      verif_type = dplyr::case_when(
        stringr::str_detect(.data$verif_fn, "verif") ~ "plausibility",
        verif_fn == "completeness" ~ "completeness",
      ),
      .before = 1
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
        dplyr::select(
          "verif_num",
          "verif_type",
          "verification",
          "n_issues"
        ),
      na = ""
    ) |>
    openxlsx2::wb_set_col_widths(
      cols = 1:ncol(results_excel),
      widths = "auto"
    )
  for (i in verif_num_issues) {
    issues <- results_excel$issues[[i]]
    sheet_name <- paste0("verif_", results_excel$verif_num[i])
    wb <- openxlsx2::wb_add_worksheet(wb, sheet_name) |>
      openxlsx2::wb_add_data_table(
        x = issues,
        na = ""
      ) |>
      openxlsx2::wb_set_col_widths(
        cols = 1:ncol(issues),
        widths = "auto"
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
  openxlsx2::wb_save(wb, here::here(final_file_path))
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
#'
#' @return A tibble with the same columns as `previous_results` extended with
#'   one row per form in `attr(completeness_results, "reviewed_forms")`.
#'   Each appended row contains:
#'   \describe{
#'     \item{`verif_fn`}{The string `"completeness"`.}
#'     \item{`verification`}{A sentence describing the completeness check for
#'       the form, e.g. `"All variables in the form 'my_form' are completed
#'       following its branching logic."`}
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
  completeness_results
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
          dplyr::filter(stringr::str_detect(.data$redcap_form_name, x))
        tibble::tibble(
          verif_fn = "completeness",
          verification = stringr::str_c(
            "All variables in the form '",
            x,
            "' are completed following its branching logic."
          ),
          execution = "ok",
          n_issues = nrow(issues),
          issues = list(issues)
        )
      }
    ) |>
    purrr::list_rbind()
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
#'
#' @return A tibble with one row and the columns:
#'   \describe{
#'     \item{verification}{A character string with `verification_description`.}
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
  issue_text
) {
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
    dplyr::mutate(
      verification = verification_description
    ) |>
    tidyr::nest(issues = -"verification") |>
    dplyr::mutate(
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
  purrr::map(
    to_verifications,
    ~ rlang::env_get(rlang::caller_env(3), .) |>
      dplyr::mutate(
        verif_fn = .,
        .before = 1
      )
  ) |>
    purrr::list_rbind()
}
