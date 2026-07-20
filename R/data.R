#' Extra conditions for RECIST assessment baseline form
#'
#' A named list of extra branching conditions for use with the
#' `extra_conditions_list` argument of [argos_check_completeness()]. It covers
#' the completeness logic of cascading lesion fields in a typical RECIST
#' baseline assessment form, where the number of lesions entered determines
#' which downstream fields are required.
#'
#' @format
#' A named list of 42 character strings. Each name is a REDCap variable name
#' and each value is an R expression (as a string) that evaluates to `TRUE`
#' when the variable is expected to have a value. The list covers two groups:
#'
#' **Target lesions** (`targ2_*` – `targ5_*`, 12 entries):
#' Fields for lesions 2–5 are required only when the preceding lesion exists,
#' implementing a cascading hierarchy. The terminal lesion (`targ5_organ`) is
#' never expected (`"FALSE"`). Measurement (`_mm`) and description
#' (`_description`) fields are required whenever their corresponding organ
#' field is present.
#'
#' **Non-target lesions** (`notarg1_*` – `notarg10_*`, 30 entries):
#' Analogous cascading hierarchy for up to 10 non-target lesions. Each lesion
#' also has a status field (`_stat`) required when the organ field is present.
#'
#' @seealso [argos_check_completeness()]
#'
#' @examples
#' \dontrun{
#' argos_check_completeness(
#'   rc_data,
#'   forms = "recist_assessment_baseline",
#'   extra_conditions_list = argos_conditions_recist_assessment_baseline
#' )
#' }
"argos_conditions_recist_assessment_baseline"


#' Argos Verifications Master
#'
#' Dataset containing information on autodetectable verifications. The function `argos_check_verifications` uses this information to determine the applicable verifications for the current REDCap database.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{id}{Verification identification}
#'   \item{version}{Verification version. Different versions are used to handle the same verification with variables of different types.}
#'   \item{type}{The type of verification (e.g. plausibility, completeness).}
#'   \item{description}{Short verbal description of the purpose of the verification.}
#'   \item{comments}{Further explanations to fully understand how the verification works.}
#'   \item{complexity}{
#'     The complexity of the verification, determined by the origin of the variables (definitions in progress):
#'     \describe{
#'       \item{intraform}{All involved variables come from the same form.}
#'       \item{interform}{Variables come from different forms.}
#'       \item{multiinstance}{The verification involves variables from forms that have multiple instances.}
#'     }
#'     Note than intra and interform are mutually exclusive, but multiinstance can be combined with either of them.
#'   }
#'   \item{arguments_metadata}{
#'     A table with information about the arguments. It has one row per argument and the following columns:
#'     \describe{
#'       \item{argument}{The name of the argument.}
#'       \item{field_type}{The expected REDCap variable type.}
#'       \item{field_choices}{The expected REDCap variable choices.}
#'       \item{field_validation}{The expected REDCap variable validation type.}
#'     }
#'   }
#'   \item{candidates_mapping}{
#'     Possible names of the variables with which to run the verification. It is a table with as many columns as verification arguments and one row per each group of possible variable names.
#'   }
#' }
"argos_verifications_master"


# This functions is completely specific to the creation of the verifications
# master. That is why it is here.
create_arguments_metadata <- function(arguments) {
  splited_arguments <-
    arguments |>
    stringr::str_replace_all("\r", "") |>
    stringr::str_split("\n") |>
    unlist()

  purrr::map(
    splited_arguments,
    function(x) {
      if (!stringr::str_detect(x, "::")) {
        result <-
          tibble::tibble(
            argument = x,
            argument_type = "constant",
            field_type = NA,
            field_choices = NA,
            field_validation = NA,
          )

        return(result)
      }

      if (stringr::str_detect(x, "::any$")) {
        result <-
          tibble::tibble(
            argument = stringr::str_remove(x, "::any$"),
            argument_type = "redcap_field",
            field_type = NA,
            field_choices = NA,
            field_validation = NA,
          )

        return(result)
      }

      argument <- stringr::str_split(x, "::") |> unlist()

      metadata <-
        odytools::ody_rc_get_metadata(
          Sys.getenv(
            stringr::str_c(argument[3], "_api_key") |>
              stringr::str_to_upper()
          )
        ) |>
        dplyr::filter(.data$field_name == argument[2])

      tibble::tibble(
        argument = argument[1],
        argument_type = "redcap_field",
        field_type = metadata$field_type,
        field_choices = metadata$select_choices_or_calculations,
        field_validation = metadata$text_validation_type_or_show_slider_number,
      )
    }
  ) |>
    purrr::list_rbind()
}


# This functions is completely specific to the creation of the verifications
# master. That is why it is here.
create_candidates_mapping <- function(candidates) {
  candidates |>
    stringr::str_replace_all("\r", "") |>
    stringr::str_split("\n") |>
    unlist() |>
    purrr::map(
      function(x) {
        arg_vector <- stringr::str_c("c(", x, ")") |> str2lang() |> eval()
        arg_names <- names(arg_vector)
        names(arg_vector) <- NULL
        purrr::map2(
          arg_vector,
          arg_names,
          ~ tibble::tibble("{.y}" := .x)
        ) |>
          purrr::list_cbind()
      }
    ) |>
    purrr::list_rbind()
}
