#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_patients_server, "patients_ui_1")
  callModule(mod_trial_server, "trial_ui_1")
}
