## **general**

Project scaffolding helpers.

-   argos_add_folders [get_project_name, here, stringr]

## **completeness**

Completeness checking functions for REDCap data.

-   filter_condition [dplyr]
-   get_conditions_from_metadata [filter_condition, dplyr, stringr, labelled]
-   verify_completeness_form [get_conditions_from_metadata, safe_filter, odytools, dplyr, purrr, tibble, tidyselect, stringr, labelled]
-   argos_check_completeness [get_conditions_from_metadata, verify_completeness_form, dplyr, purrr, tidyselect, stringr, rlang]
-   argos_count_forms [odytools, dplyr, purrr, tibble, tidyr, stringr, openxlsx]

## **plausibility_general**

General plausibility checking and reporting functions.

-   filter_issues [dplyr, glue, tidyselect]
-   find_valid_candidates [dplyr, purrr, stringr, tidyselect, tibble, tidyr]
-   argos_add_to_plausibility [dplyr, glue, purrr, tidyr, tidyselect]
-   argos_run_ad_hoc_verifications [argos_add_to_plausibility, dplyr, here, purrr, rlang]
-   argos_check_plausibility [find_valid_candidates, argos_run_ad_hoc_verifications, create_verification_description, plausibility_verifications_master, dplyr, purrr, rlang, stringr, tibble, tidyr, tidyselect]
-   create_verification_description [glue, stringr]
-   argos_write_plausibility_report [dplyr, purrr, stringr, openxlsx2, here]

## **plausibility_verifications**

Specific verification functions called by `argos_check_plausibility`.

-   verif_1_1 [filter_issues, odytools, dplyr, glue]
-   verif_1_2 [filter_issues, odytools, dplyr, stringr, glue]
-   verif_2_1 [filter_issues, odytools, dplyr, lubridate, glue]
-   verif_3_1 [filter_issues, odytools, dplyr, tidyr, tidyselect, glue]
-   verif_4_1 [filter_issues, odytools, dplyr, tidyr, stringr, glue]
-   verif_5_1 [filter_issues, odytools, dplyr, glue]
-   verif_6_1 [filter_issues, odytools, dplyr, lubridate, glue]
-   verif_7_1 [filter_issues, odytools, dplyr, lubridate, glue, tidyr]

## **utils**

Internal utility functions.

-   safe_filter [dplyr]

## **data**

Internal dataset and helper functions for its construction.

-   plausibility_verifications_master (dataset)
-   create_arguments_metadata [odytools, dplyr, purrr, stringr, tibble]
-   create_candidates_mapping [purrr, stringr, tibble]

## **globals**

Package-level setup only.

-   No user-facing functions
-   Registers the internal dataset name for `R CMD check`
