# argosrc 0.0.2

## New exported functions

- Added `argos_add_to_verifications()` to transform ad-hoc verification outputs into the nested verification result format expected by `argos_check_verifications()`. The returned object carries an `add_to_verifications` attribute that marks it for collection.
- Added `argos_run_ad_hoc_verifications()` to source ad-hoc scripts, collect objects marked with the `add_to_verifications` attribute (typically created with `argos_add_to_verifications()`), and bind them into a unified verification result table.
- Added `argos_write_forms_matrix()` to export form completion matrices to a timestamped Excel workbook (one worksheet per event), using color-coded cell formatting to highlight zero vs non-zero completion counts.
- Added `argos_write_verification_report()` to export a unified verification report (plausibility + completeness) to a timestamped Excel workbook. Supersedes `argos_write_plausibility_report()`, which has been removed.
- Added `argos_add_completeness_results()` to convert the output of `argos_check_completeness()` into the standard verification row format and append it to a verification results tibble, enabling a single combined report via `argos_write_verification_report()`.

## Verification catalogue

- Added `verif_1_3`: date A is not after date B across all instances of a repeating form, where date A is defined as the minimum of two candidate dates (`date1a`, `date1b`).
- Updated `verif_1_2` to use the minimum `date1` value per subject when `date1` comes from multiple instances.
- Updated `plausibility_verifications_master` descriptions for `verif_1_1` and `verif_1_2` and added the new `verif_1_3` entry.

## Plausibility workflow improvements

- Enhanced `argos_check_verifications()` with `ad_hoc_verifications_path` and `data_sets` to append ad-hoc verification results to automatic checks in one output.
- Improved verification descriptions in `argos_check_verifications()` for `verif_1_3` and corrected wording in `verif_7_1` descriptions.

## Completeness improvements

- `argos_check_completeness()` now attaches a `reviewed_forms` character attribute to its raw output, listing the REDCap form names that were reviewed. This attribute is required by `argos_add_completeness_results()`.
- Added `include_non_evaluable_conditions` parameter to `argos_check_completeness()`. When `TRUE` (default), rows where the branching logic condition cannot be evaluated (because the variables it depends on are themselves missing) are retained and flagged with `evaluable_condition = "No"`. Set to `FALSE` to silently drop these rows.
- `argos_check_completeness()` output in `format = "raw"` now includes an `evaluable_condition` column (`"Yes"` / `"No"`) indicating whether the branching logic condition could be resolved for each flagged row.
- `argos_check_completeness()` in `format = "friendly"` now uses the project's dynamic `id_var` instead of the hard-coded `record_id` column name, and correctly handles non-longitudinal projects (those without an `events` attribute).
- Branching logic condition suffixes (after `___`) are now lowercased for consistency with REDCap field naming conventions.
- Replaced internal helper `safe_filter()` with `safe_condition_definition()`. The new helper adds a `meets_condition` column instead of filtering directly, enabling finer-grained handling of unresolvable conditions: returns `NA` for rows where the branching logic variables are themselves missing, rather than assuming the condition is met for all rows.
- `argos_count_forms()` has been refactored: the `save_path` parameter has been removed and the function now returns a flat tibble (instead of a list) with `redcap_event_name`, `redcap_form_name`, and `n` columns, plus REDCap metadata as attributes (`events`, `forms`, `redcap_import_date`, `project_info`). Pass this tibble to `argos_write_forms_matrix()` to export the Excel workbook.

## Breaking changes

- Removed `argos_write_plausibility_report()`. Use `argos_write_verification_report()` instead.
- `argos_count_forms()` no longer accepts a `save_path` argument. Use `argos_write_forms_matrix()` to export the Excel workbook.

## Project setup templates

- Updated `argos_add_folders()` to copy `*_verifications_master.R` and `*_verifications_adhoc.R` templates (renamed from `*_plausibility.R` and `*_plausibility_adhoc.R`) into `quality/argosrc/` and creates a `verification_results/` subfolder instead of separate `completeness/` and `plausibility/` directories.

# argosrc 0.0.1

First release of **argosrc** (Automatic Review and Governance for Oncology Studies in REDCap).

## Project setup

- Added `argos_add_folders()` to create the `quality/argosrc` folder structure in an existing project and copy bundled completeness and plausibility template scripts named after the current project.

## Completeness

- Added `argos_check_completeness()` to verify missing and unexpected values across REDCap forms. It respects branching logic defined in the project metadata, translating REDCap conditions into R expressions. Supports `"missing"`, `"unexpected"`, or `"both"` checks, user-defined missing value handling, and additional user-supplied conditions. Output can be returned in raw or human-readable `"friendly"` format.
- Added `argos_count_forms()` to count the number of completed form instances per subject and event. Optionally exports the result to a colour-coded Excel workbook (one sheet per event) using `openxlsx`.

## Plausibility

- Added `argos_check_verifications()` to evaluate a REDCap export against the built-in plausibility verification catalogue (`plausibility_verifications_master`). The function automatically identifies applicable verifications by matching field metadata (type, choices, validation), respects intraform, interform, and multiinstance complexity constraints, and supports user-supplied constant arguments via `constants_list`. Results include execution status (`"ok"`, `"fail"`, `"missing constants"`), issue counts, and issue detail tables.
- Added `argos_write_plausibility_report()` to export plausibility results to a timestamped Excel workbook. The workbook includes a project info sheet, a reviewed subjects sheet, a verifications summary sheet, and one additional sheet per verification with detected issues.

## Verification catalogue

- Added the internal dataset `plausibility_verifications_master`, which defines the catalogue of available plausibility verifications with their argument specifications and candidate field mappings.
- Implemented seven verification functions covering the most common plausibility rules in oncology REDCap projects:
  - `verif_1_1`: date A is not after date B (intraform).
  - `verif_1_2`: date A is not after date B across all instances of a repeating form (interform).
  - `verif_2_1`: time elapsed between two dates is within an expected range.
  - `verif_3_1`: a variable holds a specific expected value.
  - `verif_4_1`: a variable has the same value across all instances of a repeating form.
  - `verif_5_1`: overall tumour response is consistent with target, non-target, and new lesions responses (RECIST logic).
  - `verif_6_1`: last follow-up date for alive patients is within the expected recency window.
  - `verif_7_1`: last visit date is within the expected recency window for patients without a recorded end date.
