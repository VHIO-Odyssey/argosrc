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

- `argos_check_completeness()`: checks missing and unexpected values form-by-form using REDCap branching logic.
- `argos_count_forms()`: counts available form instances by subject and event; optionally exports an Excel summary.
- `argos_check_verifications()`: runs a catalogue of plausibility rules on the current REDCap export.
- `argos_write_plausibility_report()`: writes plausibility results to a timestamped Excel report.

## Typical workflow

```r
library(odytools)
library(argosrc)

# REDCap import done with odytools
redcap_data <- ody_rc_import(
	token = Sys.getenv("MY_REDCAP_TOKEN")
)

# 1) Completeness checks
completeness_tbl <- argos_check_completeness(
	redcap_data,
	forms = "All",
	check_for = "both",
	format = "friendly"
)

# 2) Plausibility checks
plausibility_tbl <- argos_check_verifications(redcap_data)

# 3) Export plausibility report
argos_write_plausibility_report(
	plausibility_tbl,
	file_path = "outputs/plausibility_report.xlsx"
)
```

## Outputs

- Completeness checks return a tidy table of issues per record, event, form and field.
- Plausibility checks return one row per detected verification candidate, with execution status and issue tables.
- Excel reports include project metadata, reviewed subjects and one sheet per verification with issues.

## Dependency note

`argosrc` expects REDCap objects created with `odytools` (for example via `ody_rc_import()`) because it relies on the attributes and structure defined there.
