#' Plausibility Verifications Master
#'
#' Dataset containing information on plausibility verifications. The function `argos_check_plausibility` uses this information to determine the applicable verifications for the current REDCap database.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{id}{Verification identification}
#'   \item{version}{Verification version. Different versions are used to handle the same verification with variables of different types.}
#'   \item{description}{Short verbal description of the purpose of the verification.}
#'   \item{comments}{Further explanations to fully understand how the verification works.}
#'   \item{complexity}{
#'     The complexity of the verification, determined by the origin of the variables (definitions in progress):
#'     \describe{
#'       \item{intraform}{All involved variables come from the same form.}
#'       \item{interform}{Variables come from different forms.}
#'       \item{interinstance}{The verification compares values of the same variable(s) from different instances.}
#'     }
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
"plausibility_verifications_master"


# This functions is completely specific to the creation of the verifications
# master. That is why it is here.
create_arguments_metadata <- function(arguments) {

  splited_arguments <-
    arguments |>
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

      argument <- stringr::str_split(x, "::") |> unlist()

      metadata <-
        odytools::ody_rc_get_metadata(
          Sys.getenv(
            stringr::str_c(argument[3], "_api_key") |>
              stringr::str_to_upper()
          )
        ) |>
        dplyr::filter(field_name == argument[2])

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
    stringr::str_split("\n|\r\n") |>
    unlist() |>
    purrr::map(
      function(x) {
        arg_vector <- stringr::str_c("c(", x, ")") |> str2lang() |> eval()
        arg_names <- names(arg_vector)
        names(arg_vector) <- NULL
        purrr::map2(
          arg_vector, arg_names,
          ~ tibble::tibble("{.y}" := .x)
        ) |>
          purrr::list_cbind()
      }
    ) |>
    purrr::list_rbind()

}

