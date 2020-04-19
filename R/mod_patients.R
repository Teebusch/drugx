#' patients UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' @import ggplot2
mod_patients_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Patient Characteristics"),
    plotOutput(ns("plot"))
  )
}
    
#' patients Server Function
#'
#' @noRd 
mod_patients_server <- function(input, output, session){
  ns <- session$ns
  output$plot <- renderPlot({
    ggplot(pat, aes(AGE)) +
      geom_density(fill = "grey90", color = "grey90") +
      geom_density(aes(color = ACTARM)) +
      theme_linedraw()
  })
}
    
## To be copied in the UI
# mod_patients_ui("patients_ui_1")
    
## To be copied in the server
# callModule(mod_patients_server, "patients_ui_1")
 
