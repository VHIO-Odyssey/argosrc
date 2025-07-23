
redcap_data <- odytools::ody_rc_import(Sys.getenv("SERONCO1_API_KEY"))






ody_rc_select(redcap_data, adverse_events)

ae_fields <-
  attr(redcap_data, "metadata") |>
  filter(
    form_name == "adverse_events",
    field_type != "complete_info"
  ) |>
  select(field_name, field_type, select_choices_or_calculations,  text_validation_type_or_show_slider_number)





form_definitions <-
  tibble(
    form = "adverse_events",
    version = 1:2,
    structure = list(
      ae_fields[-6,],
      ae_fields

    )
  )



form_definitions$structure[[1]]


ody_rc_get_metadata(Sys.getenv("SERONCO1_API_KEY")) |>
  filter(form_name == "adverse_events")




var1 <- "ae_startdate"
var2 <- "ae_enddate"


var1_loe_than_var2 <- function(var1, var2) {

  ody_rc_select(redcap_data, !!var1, !!var2) |>
    ody_rc_format() |>
    mutate(
      .ok = !!var1 <= !!var2
    ) |>
    ody_filter_fails()

}


verification_core1(ae_startdate, ae_enddate)







