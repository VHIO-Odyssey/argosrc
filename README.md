# argosrc

<!-- badges: start -->

<!-- badges: end -->

`argosrc` is an `odytools` companion package to review REDCap databases for data quality issues, with a focus on:

- **Completeness**: missing values and unexpected values according to branching logic.
- **Plausibility**: cross-field consistency and temporal logic checks.

It is designed for clinical research workflows where REDCap exports need quick, reproducible quality control before analysis.

Argos was Odysseus's dog. Old and weak, he is the first to recognize Odysseus when he returns to Ithaca after 20 years away, even while disguised; he wags his tail and lowers his ears, then dies shortly afterward.

![](man/figures/Ulysses_and_Argo.png)

## Installation

Install from GitHub with:

``` r
# install.packages("pak")
pak::pak("VHIO-Odyssey/argosrc")
```

## Main functions

**Completeness**

- `argos_check_completeness()`: checks missing and unexpected values form-by-form using REDCap branching logic.
- `argos_add_completeness_results()`: converts completeness output into the standard verification row format so it can be included in a unified report.

**Plausibility**

- `argos_check_verifications()`: runs the built-in catalogue of plausibility rules on a REDCap export, with optional ad-hoc script integration.
- `argos_add_to_verifications()`: wraps ad-hoc verification output into the nested format expected by `argos_check_verifications()`.
- `argos_run_ad_hoc_verifications()`: sources ad-hoc verification scripts and binds their results into a unified verification table.

**Reporting**

- `argos_write_verification_report()`: exports a unified completeness + plausibility report to a timestamped Excel workbook.
- `argos_count_forms()`: counts available form instances by subject and event; returns a flat tibble with project metadata as attributes.
- `argos_write_forms_matrix()`: exports form completion matrices to a timestamped Excel workbook with colour-coded formatting.

**Project setup**

- `argos_add_folders()`: creates the `quality/argosrc/` folder structure and copies bundled template scripts into an existing project.

## Typical workflow

```r
library(odytools)
library(argosrc)

# REDCap import done with odytools
redcap_data <- ody_rc_import(
	token = Sys.getenv("MY_REDCAP_TOKEN")
)

# 1) Plausibility checks
verification_results <- argos_check_verifications(redcap_data)

# 2) Completeness checks, appended to the same results table
verification_results <- argos_check_completeness(
	redcap_data,
	forms = "All",
	check_for = "both"
) |>
	argos_add_completeness_results(verification_results)

# 3) Export unified report
argos_write_verification_report(
	verification_results,
	file_path = "outputs/verification_report.xlsx"
)
```

## Outputs

- Completeness checks return a tidy table of issues per record, event, form and field; the `evaluable_condition` column flags rows where branching logic could not be resolved.
- Plausibility checks return one row per verification candidate, with execution status (`"ok"`, `"fail"`, `"missing constants"`), issue counts, and nested issue tables.
- The unified Excel report includes project metadata, reviewed subjects, a verifications summary, and one sheet per verification with detected issues.
- Form completion matrices are exported as a separate colour-coded workbook via `argos_write_forms_matrix()`.

## Dependency note

`argosrc` expects REDCap objects created with `odytools` (for example via `ody_rc_import()`) because it relies on the attributes and structure defined there.
