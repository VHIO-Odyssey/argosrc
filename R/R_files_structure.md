## **general**

Funciones para crear la estructura base de carpetas del proyecto.

-   argos_add_folders [here, odytools::get_project_name, stringr]

## **completeness**

Funciones de verificación de completitud en datos REDCap.

-   filter_condition [dplyr]
-   get_conditions_from_metadata [dplyr, labelled, stringr]
-   verify_completeness_form [safe_filter, dplyr, labelled, odytools::ody_rc_select, odytools::ody_rc_select_form, purrr, stringr, tibble, tidyselect]
-   argos_check_completeness [get_conditions_from_metadata, verify_completeness_form, dplyr, purrr, rlang, stringr, tidyselect]
-   argos_count_forms [dplyr, odytools::ody_rc_select_form, purrr, tibble, tidyr]
-   argos_write_forms_matrix [dplyr, here, openxlsx, purrr, stringr, tibble, tidyr]

## **plausibility_general**

Funciones generales para detección, ejecución y reporte de verificaciones de plausibilidad.

-   filter_issues [dplyr, glue, tidyselect]
-   find_valid_candidates [dplyr, purrr, stringr, tibble, tidyr, tidyselect]
-   argos_add_to_plausibility [dplyr, glue, purrr, tidyr, tidyselect]
-   argos_run_ad_hoc_verifications [dplyr, purrr, rlang]
-   argos_check_plausibility [find_valid_candidates, create_verification_description, argos_run_ad_hoc_verifications, plausibility_verifications_master, dplyr, purrr, rlang, stringr, tibble, tidyr, tidyselect]
-   create_verification_description [glue, stringr]
-   argos_write_plausibility_report [dplyr, here, openxlsx2, stringr]

## **plausibility_verifications**

Funciones específicas de verificación llamadas por `argos_check_plausibility`.

-   verif_1_1 [filter_issues, dplyr, glue, odytools::ody_rc_format, odytools::ody_rc_select]
-   verif_1_2 [filter_issues, dplyr, glue, odytools::ody_rc_format, odytools::ody_rc_select, stringr]
-   verif_1_3 [filter_issues, dplyr, glue, odytools::ody_rc_format, odytools::ody_rc_select, stringr, tidyselect]
-   verif_2_1 [filter_issues, dplyr, glue, lubridate, odytools::ody_rc_format, odytools::ody_rc_select]
-   verif_3_1 [filter_issues, dplyr, glue, odytools::ody_rc_format, odytools::ody_rc_select, tidyr, tidyselect]
-   verif_4_1 [filter_issues, dplyr, odytools::ody_rc_format, odytools::ody_rc_select, stringr, tidyr]
-   verif_5_1 [filter_issues, dplyr, glue, odytools::ody_rc_format, odytools::ody_rc_select]
-   verif_6_1 [filter_issues, dplyr, glue, lubridate, odytools::ody_rc_format, odytools::ody_rc_select]
-   verif_7_1 [filter_issues, dplyr, glue, lubridate, odytools::ody_rc_format, odytools::ody_rc_select, tidyselect]

## **utils**

Funciones utilitarias internas.

-   safe_filter [dplyr]

## **data**

Dataset interno y funciones auxiliares para su construcción.

-   plausibility_verifications_master (dataset)
-   create_arguments_metadata [dplyr, odytools::ody_rc_get_metadata, purrr, stringr, tibble]
-   create_candidates_mapping [purrr, stringr, tibble]

## **globals**

Configuración global del paquete.

-   No user-facing functions
-   Registers the internal dataset name for `R CMD check`
