# ==============================================================================
# Ad-hoc verifications
# ==============================================================================
#
# Define custom verifications in this script. Each check is a tibble with
# a logical '.ok' column indicating whether each row passed the check. Rows
# where '.ok' is FALSE or NA are reported as issues.
#
# When this script is sourced by argos_check_verifications() via the
# 'ad_hoc_verifications_path' argument, two objects are automatically available:
#
#   redcap_data — the REDCap data object passed to argos_check_verifications().
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
# 2. Pass the tibble to argos_add_to_verifications(), providing:
#      - verification_description: a short text describing what is being
#        checked. This appears in the summary sheet of the Excel report.
#      - issue_text: a glue template string that builds the issue message for
#        each failing row. Reference column names with {column_name}.
#      - verif_type: the type of verification. Must be one of:
#          "plausibility"  — checks logical consistency between variables.
#          "completeness"  — checks that expected fields are filled in.
#          NA              — type not specified (default).
#
# 3. Assign the result to a variable. argos_check_verifications() will
#    automatically detect any object in this script that has been created with
#    argos_add_to_verifications() and include it in the final results.
#
# You can define as many verifications as needed. Variable names are used
# internally as verification identifiers (verif_fn column in the result).
#
# NAMING CONVENTION — EFFECT ON THE REPORT
# ==========================================
# In the Excel report produced by argos_write_verification_report(),
# verifications are ordered by:
#   1. verif_type  — "completeness" before "plausibility", then NA.
#   2. verif_origin — "adhoc" (this script) before "auto" (automatic checks).
#   3. verif_fn    — alphabetically by the variable name you assign here.
#
# The group label shown in the report's 'verif_type' column comes from the
# verif_type argument passed to argos_add_to_verifications().
#
# Examples:
#   comp_screening_form  <- argos_add_to_verifications(..., verif_type = "completeness")
#   verif_adverse_events <- argos_add_to_verifications(..., verif_type = "plausibility")
#
# ==============================================================================
