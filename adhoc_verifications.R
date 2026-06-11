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

# adhoc_1 ----
# Si hay EOT, hay un follow-up o un detah date no ser que el EOT sea de hace menos de 3 meses.

library(odytools)
library(dplyr)
library(lubridate)


import_date <- attr(redcap_data, "import_date") |> as.Date()

eot <-
  ody_rc_select(redcap_data, eot_date) |>
  select(-redcap_instance_number) |>
  ody_rc_format()

death <-
  ody_rc_select(redcap_data, death_date) |>
  ody_rc_simplify_selection() |>
  ody_rc_format()


first_last_fu <-
  ody_rc_select(redcap_data, last_fu_date) |>
  ody_rc_format() |>
  filter(redcap_instance_number == 1) |>
  select(record_id, last_fu_date)

adhoc_1 <-
  left_join(eot, first_last_fu, by = "record_id") |>
  left_join(death, by = "record_id") |>
  mutate(
    .ok = case_when(
      !is.na(last_fu_date) ~ TRUE,
      !is.na(death_date) ~ TRUE,
      time_length(import_date - eot_date, "months") < 3 ~ TRUE,
      .default = FALSE
    )
  ) |>
  argos_add_to_verifications(
    verification_description = "If there is an EOT date, there should be a follow-up date or a death date, unless the EOT date is less than 3 months ago.",
    issue_text = "EOT on {eot_date} without a registered follow-up or death date."
  )
