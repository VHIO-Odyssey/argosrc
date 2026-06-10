# ==============================================================================
# Plausibility checks — ad-hoc verifications
# ==============================================================================
#
# Define custom plausibility checks in this script. Each check is a tibble with
# a logical '.ok' column indicating whether each row passed the check. Rows
# where '.ok' is FALSE or NA are reported as issues.
#
# When this script is sourced by argos_check_plausibility() via the
# 'ad_hoc_verifications_path' argument, two objects are automatically available:
#
#   redcap_data — the REDCap data object passed to argos_check_plausibility().
#   datasets    — the named list passed via the 'data_sets' argument (or NULL).
#
# WORKFLOW FOR EACH VERIFICATION
# ================================
# 1. Build a tibble that contains:
#      - The subject identifier in the first column.
#      - Any REDCap context columns you want to appear in the report
#        (redcap_event_name, redcap_form_name, redcap_instance_number).
#      - Any data columns needed to build the issue message.
#      - A logical '.ok' column: TRUE = check passed, FALSE = issue detected,
#        NA = check could not be evaluated (treated as an issue).
#
# 2. Pass the tibble to argos_add_to_plausibility(), providing:
#      - verification_description: a short text describing what is being
#        checked. This appears in the summary sheet of the Excel report.
#      - issue_text: a glue template string that builds the issue message for
#        each failing row. Reference column names with {column_name}.
#
# 3. Assign the result to a variable. argos_check_plausibility() will
#    automatically detect any object in this script that has been created with
#    argos_add_to_plausibility() and include it in the final results.
#
# You can define as many verifications as needed. Variable names are used
# internally as verification identifiers (verif_fn column in the result).
#
# ==============================================================================


# ------------------------------------------------------------------------------
# Example 1: Check that treatment start date is not after treatment end date
# ------------------------------------------------------------------------------
# This example uses odytools::ody_rc_select() to extract the relevant fields,
# then defines .ok based on a date comparison.

# verif_start_before_end <- odytools::ody_rc_select(
#   redcap_data,
#   treatment_start_date,
#   treatment_end_date
# ) |>
#   odytools::ody_rc_format() |>
#   dplyr::mutate(
#     .ok = is.na(treatment_start_date) |
#           is.na(treatment_end_date) |
#           treatment_start_date <= treatment_end_date
#   ) |>
#   argos_add_to_plausibility(
#     verification_description = "Treatment start date is not after treatment end date",
#     issue_text = "treatment_start_date ({treatment_start_date}) is after treatment_end_date ({treatment_end_date})."
#   )


# ------------------------------------------------------------------------------
# Example 2: Check that a dose value is within a plausible range
# ------------------------------------------------------------------------------

# verif_dose_range <- odytools::ody_rc_select(
#   redcap_data,
#   drug_dose
# ) |>
#   odytools::ody_rc_format() |>
#   dplyr::mutate(
#     .ok = is.na(drug_dose) | dplyr::between(drug_dose, 0, 500)
#   ) |>
#   argos_add_to_plausibility(
#     verification_description = "Drug dose is within the expected range (0-500 mg)",
#     issue_text = "drug_dose has unexpected value: {drug_dose}."
#   )


# ------------------------------------------------------------------------------
# Example 3: Cross-form check using a dataset from 'datasets'
# ------------------------------------------------------------------------------
# When datasets is not NULL, auxiliary data objects are available via the
# 'datasets' list. For example, if data_sets = list(ref_table = my_ref),
# then inside this script you can use datasets$ref_table.

# verif_site_consistency <- odytools::ody_rc_select(
#   redcap_data,
#   site_code
# ) |>
#   odytools::ody_rc_format() |>
#   dplyr::left_join(datasets$ref_table, by = "site_code") |>
#   dplyr::mutate(
#     .ok = !is.na(site_name)  # site_code must exist in the reference table
#   ) |>
#   argos_add_to_plausibility(
#     verification_description = "Site code is valid according to the reference table",
#     issue_text = "site_code '{site_code}' was not found in the reference table."
#   )
