# ==============================================================================
# Completeness checks
# ==============================================================================
#
# This script runs completeness checks on the REDCap data. Two functions are
# available:
#
#   - argos_count_forms(): counts how many instances of each form exist per
#     subject and event, and exports the result as a colour-coded Excel matrix.
#
#   - argos_check_completeness(): detects missing and/or unexpected values
#     field by field, respecting the branching logic defined in the REDCap
#     project.
#
# Both functions require a REDCap data object imported with odytools, e.g.:
#   redcap_data <- ody_rc_import(token = Sys.getenv("MY_REDCAP_TOKEN"))
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Form completion matrix
# ------------------------------------------------------------------------------
# argos_count_forms() returns a tibble with the number of form instances per
# subject and event. Combinations with no recorded data are filled with 0.
#
# argos_write_forms_matrix() exports the result as an Excel file with one sheet
# per event. Cells with 0 are grey; cells with > 0 are light blue. The output
# filename is automatically suffixed with the REDCap import timestamp.
#
# Adjust 'file_path' to the desired output location relative to the project
# root (here::here() is used internally).

forms_count <- argos_count_forms(redcap_data)

argos_write_forms_matrix(
  forms_count,
  file_path = "quality/argosrc/outputs/forms_matrix"
)


# ------------------------------------------------------------------------------
# 2. Completeness check
# ------------------------------------------------------------------------------
# argos_check_completeness() inspects every field in the specified forms and
# flags two types of issue:
#
#   "missing"    — fields that should be filled according to the branching logic
#                  but contain a missing value (regular NA or user-defined NA).
#
#   "unexpected" — fields that should be hidden according to the branching
#                  logic but contain a non-missing value.
#
#   "both"       — checks for both issue types simultaneously (default).
#
# KEY ARGUMENTS
#
#   forms
#     Character vector of REDCap instrument names to check, e.g.:
#       forms = c("demographics", "adverse_events")
#     Use "All" (default) to check every form in the project.
#
#   user_na_is_data
#     Logical. When TRUE (default), user-defined missing values (e.g. "Not
#     applicable", "Unknown") are treated as valid data, so fields holding a
#     user NA are NOT flagged as missing. Set to FALSE to flag them too.
#
#   check_for
#     One of "missing", "unexpected", or "both" (default).
#
#   format
#     "friendly" (default) returns human-readable labels (field label, form
#     name, event name). "raw" returns internal REDCap identifiers.
#
#   extra_conditions_list
#     Optional named list of R expression strings to override or supplement the
#     branching logic extracted from the REDCap metadata. Each name is a field
#     name and each value is an R condition string. Use this when the automatic
#     translation of branching logic produces incorrect results, or when you
#     want to impose additional conditions not captured in REDCap.
#     Example:
#       extra_conditions_list = list(
#         adverse_event_grade = "adverse_event_yn == '1'"
#       )

completeness_result <- argos_check_completeness(
  redcap_data,
  forms         = "All",
  user_na_is_data = TRUE,
  check_for     = "both",
  format        = "friendly"
)

# Inspect the result:
completeness_result

# Filter to a specific issue type:
completeness_result |> dplyr::filter(`Completeness Issue` == "Regular missing")
completeness_result |> dplyr::filter(`Completeness Issue` == "User missing")
completeness_result |> dplyr::filter(`Completeness Issue` == "Unexpected")

# Count issues per form:
completeness_result |> dplyr::count(Form, `Completeness Issue`)
