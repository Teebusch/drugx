#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # List the first level callModules here
  filters <- callModule(mod_filters_server, "filters_ui_1")
  
  pat_flt <- reactive({
    pat %>% 
      filter(
        betweenRange(AGE, filters$flt_age()),
        SEX %in% filters$flt_sex(),
        RACE %in% filters$flt_race(),
        betweenRange(BMRKR1, filters$flt_biomarker1()),
        BMRKR2 %in% filters$flt_biomarker2()
      )
  })
  
  lab_flt <- reactive({
    selected_patients <- pat_flt()$USUBJID
    filter(lab, USUBJID %in% selected_patients)
  })
  
  callModule(mod_patients_server, "patients_ui_1", pat_flt, lab_flt)
  callModule(mod_trial_server, "trial_ui_1", pat_flt, lab_flt)
}
