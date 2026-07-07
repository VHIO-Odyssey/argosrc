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
# The variable name you choose controls how the verification is classified in
# the 'verif_type' column of the Excel report:
#
#   comp_*   → classified as "completeness_adhoc"
#              Use this prefix for ad-hoc completeness checks (e.g. checking
#              that specific forms are filled in for a given subgroup).
#
#   verif_*  → classified as "verification_adhoc"
#              Use this prefix for ad-hoc plausibility or consistency checks.
#
#   anything else → classified as "undefined_adhoc"
#
# Within each type, verifications are sorted alphabetically by variable name
# before being numbered in the final report.
#
# Examples:
#   comp_screening_form  <- argos_add_to_verifications(...)  # completeness_adhoc
#   verif_adverse_events <- argos_add_to_verifications(...)  # verification_adhoc
#
# ==============================================================================
