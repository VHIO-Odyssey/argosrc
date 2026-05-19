# argosrc 0.0.1.9000

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
