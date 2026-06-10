# ==============================================================================
# Plausibility checks — automatic verifications
# ==============================================================================
#
# This script runs the built-in plausibility verification catalogue and, when
# applicable, appends ad-hoc checks defined in plausibility_adhoc.R.
#
# argos_check_plausibility() inspects the REDCap project metadata to identify
# which built-in verifications are applicable to this study (based on field
# names, types, and choices). It then executes each applicable verification and
# returns a results table with one row per detected verification.
#
# argos_write_plausibility_report() exports the results to a timestamped Excel
# workbook containing:
#   - project_info sheet: project title, ID, and import date.
#   - reviewed_subjects sheet: list of all reviewed subjects (and site/DAG if
#     available).
#   - verifications sheet: summary table with one row per verification and the
#     number of detected issues.
#   - One sheet per verification that detected at least one issue, named
#     "verif_1", "verif_2", etc., matching the row number in the summary.
#
# Both functions require a REDCap data object imported with odytools, e.g.:
#   redcap_data <- ody_rc_import(token = Sys.getenv("MY_REDCAP_TOKEN"))
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Constants (required for some verifications)
# ------------------------------------------------------------------------------
# Some built-in verifications need project-specific constant arguments that
# cannot be derived automatically from the REDCap metadata. These are defined
# in a constants_list: a list of one-row tibbles, one per verification that
# needs constants. Each tibble must contain a 'verif_fn' column identifying the
# target verification (formatted as "<id>_<version>") plus one column per
# constant argument.
#
# Verifications that currently require constants:
#
#   verif_6_1 — Checks that the last follow-up date for alive patients is
#               recent. Constants needed:
#                 time_limit: numeric, maximum acceptable time elapsed.
#                 unit: "months", "years", "weeks", or "days".
#
#   verif_7_1 — Checks that the last visit date for patients without an end
#               date is recent. Constants needed:
#                 time_limit: numeric, maximum acceptable time elapsed.
#                 unit: "months", "years", "weeks", or "days".
#
# Verifications without constants (verif_1_1 through verif_5_1) are detected
# and executed automatically; you do not need to include them here.
#
# Remove any entry below for verifications that are not applicable to this study,
# or leave constants_list as NULL to skip constant-dependent verifications.

constants_list <- list(
  tibble::tibble(
    verif_fn   = "verif_6_1",
    time_limit = 3,
    unit       = "months"
  ),
  tibble::tibble(
    verif_fn   = "verif_7_1",
    time_limit = 2,
    unit       = "months"
  )
)

# To skip constant-dependent verifications entirely, set:
# constants_list <- NULL


# ------------------------------------------------------------------------------
# 2. Run plausibility checks
# ------------------------------------------------------------------------------
# Set ad_hoc_verifications_path to the path of the ad-hoc script when custom
# checks have been defined there. Set it to NULL to skip ad-hoc checks.
#
# data_sets is an optional named list of auxiliary data objects (e.g. external
# reference tables) that will be available inside the ad-hoc script as
# 'datasets'. Set to NULL if not needed.
#
# Execution status values in the result:
#   "ok"               — Verification ran successfully.
#   "fail"             — A runtime error occurred; inspect the data manually.
#   "missing constants" — Constants are needed but were not provided above.

plausibility_result <- argos_check_plausibility(
  redcap_data,
  constants_list          = constants_list,
  ad_hoc_verifications_path = here::here("quality/argosrc/plausibility_adhoc.R"),
  data_sets               = NULL
)

# Inspect the result summary:
plausibility_result |>
  dplyr::select(verif_fn, verification, execution, n_issues)

# Check which verifications failed or are missing constants:
plausibility_result |>
  dplyr::filter(execution != "ok") |>
  dplyr::select(verif_fn, verification, execution)

# Inspect issues from a specific verification (replace 1 with the desired row):
plausibility_result$issues[[1]]


# ------------------------------------------------------------------------------
# 3. Export Excel report
# ------------------------------------------------------------------------------
# The output filename is automatically suffixed with the REDCap import
# timestamp, e.g. "plausibility_report_20250501_1430.xlsx".
# Adjust 'file_path' to the desired output location.

argos_write_plausibility_report(
  plausibility_result,
  file_path = "quality/argosrc/outputs/plausibility_report"
)
