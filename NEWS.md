# argosrc 0.0.1.9000

## New exported functions

- Added `argos_add_to_plausibility()` to transform ad-hoc verification outputs into the nested plausibility result format used by `argos_check_plausibility()`.
- Added `argos_run_ad_hoc_verifications()` to source ad-hoc scripts, detect objects marked with `add_to_plausibility`, and bind them into a unified plausibility result table.
- Added `argos_write_forms_matrix()` to export form completion matrices to a timestamped Excel workbook (one worksheet per event), using color-coded cell formatting to highlight zero vs non-zero completion counts.

## Verification catalogue

- Added `verif_1_3`: date A is not after date B across all instances of a repeating form, where date A is defined as the minimum of two candidate dates (`date1a`, `date1b`).
- Updated `verif_1_2` to use the minimum `date1` value per subject when `date1` comes from multiple instances.
- Updated `plausibility_verifications_master` descriptions for `verif_1_1` and `verif_1_2` and added the new `verif_1_3` entry.

## Plausibility workflow improvements

- Enhanced `argos_check_plausibility()` with `ad_hoc_verifications_path` and `data_sets` to append ad-hoc verification results to automatic checks in one output.
- Improved verification descriptions in `argos_check_plausibility()` for `verif_1_3` and corrected wording in `verif_7_1` descriptions.
- Updated `argos_write_plausibility_report()` to include only verifications with `execution == "ok"`, simplify summary columns, and standardize issue sheet names.

## Project setup templates

- Updated `argos_add_folders()` to copy `*_plausibility_master.R` (renamed from `*_plausibility.R`) and include a new `*_plausibility_adhoc.R` template for ad-hoc checks.

# argosrc 0.0.1

First release of **argosrc** (Automatic Review and Governance for Oncology Studies in REDCap).

## Project setup

- Added `argos_add_folders()` to create the `quality/argosrc` folder structure in an existing project and copy bundled completeness and plausibility template scripts named after the current project.

## Completeness

- Added `argos_check_completeness()` to verify missing and unexpected values across REDCap forms. It respects branching logic defined in the project metadata, translating REDCap conditions into R expressions. Supports `"missing"`, `"unexpected"`, or `"both"` checks, user-defined missing value handling, and additional user-supplied conditions. Output can be returned in raw or human-readable `"friendly"` format.
- Added `argos_count_forms()` to count the number of completed form instances per subject and event. Optionally exports the result to a colour-coded Excel workbook (one sheet per event) using `openxlsx`.

## Plausibility

- Added `argos_check_plausibility()` to evaluate a REDCap export against the built-in plausibility verification catalogue (`plausibility_verifications_master`). The function automatically identifies applicable verifications by matching field metadata (type, choices, validation), respects intraform, interform, and multiinstance complexity constraints, and supports user-supplied constant arguments via `constants_list`. Results include execution status (`"ok"`, `"fail"`, `"missing constants"`), issue counts, and issue detail tables.
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
