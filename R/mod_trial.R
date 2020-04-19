#' trial UI Function
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
mod_trial_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Trial"),
    plotOutput(ns("plot"))
  )
}
    
#' trial Server Function
#'
#' @noRd 
mod_trial_server <- function(input, output, session){
  ns <- session$ns
  output$plot <- renderPlot({
    ggplot(lab, aes(AVISIT, AVAL, group = USUBJID)) +
      geom_line(alpha = .1) +
      facet_wrap(~LBTEST, ncol = 1) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
  })
}
    
## To be copied in the UI
# mod_trial_ui("trial_ui_1")
    
## To be copied in the server
# callModule(mod_trial_server, "trial_ui_1")
 
