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
    h1( "Clinical Trial Results" ),
    radioButtons(
      ns("measurement"), 
      "Select how measurements are displayed",
      choices = list(
        "Difference to Baseline" = "AVAL_CHANGE",
        "Absolute" = "AVAL"
      ), 
      selected = "AVAL_CHANGE"
    ),
    
    h2( "Group Means" ), 
    plotOutput(ns("plot_groupDiff")),
    
    h2( "Measurements by Arm" ),
    sliderInput(
      ns("sampleSize"), 
      "Max. number of individual patients to show.", 
      min = 0, max = 164, value = 50
    ),
    plotOutput(ns("plot_allvisits")),
    
    h2( "Measurements by Arm and Visit" ),
    sliderInput(
      ns("day"), 
      "Select day to be displayed.", 
      min = 1, max = 36, step = 7, value = 36,
      pre = "Day ", 
      ticks = FALSE,
      animate = animationOptions(
        interval = 2000
      )
    ),
    fluidRow(
      column( width = 4, plotOutput(ns("plot_visitALT")) ),
      column( width = 4, plotOutput(ns("plot_visitCRP")) ),
      column( width = 4, plotOutput(ns("plot_visitIGA")) ),
    )
  )
}
    
#' trial Server Function
#'
#' @noRd
mod_trial_server <- function(input, output, session){
  ns <- session$ns
  
  # desired output (absolute vs. relative to baseline)
  measurement <- reactive({ sym(input$measurement) })
  
  # convert selected day to visit factor level
  visits <- levels(lab$AVISIT)[-1]
  visit <- reactive({ visits[[(input$day %/% 7) + 1]] })
  
  # max subjects in each arm * measurent
  maxSampleSize <- group_by(lab, ACTARM) %>% 
    count( USUBJID ) %>% 
    tally() %>% 
    .$n %>% 
    max()
  
  updateSliderInput( session, "sampleSize", max = maxSampleSize )
  
  
  output$plot_groupDiff <- renderPlot({
    lab %>% 
      filter( AVISIT != "SCREENING" ) %>% 
      plotGroupDifferences( !!measurement() )
  })
  
  output$plot_allvisits <- renderPlot({
    lab %>% 
      filter( AVISIT != "SCREENING" ) %>% 
      plotAllVisits( !!measurement(), max_sample = input$sampleSize )
  })
  
  output$plot_visitALT <- renderPlot({
    plotLabMeasurementAtVisit( lab, !!measurement(), visit(), "ALT" ) +
      labs(
        title = "Alanine Aminotransferase (ALT)",
        subtitle = visit(), 
        x = "U/L"
      )
  })
  
  output$plot_visitCRP <- renderPlot({
    plotLabMeasurementAtVisit( lab, !!measurement(), visit(), "CRP" ) +
      labs(
        title = "C-Reactive Protein (CRP)", 
        subtitle = visit(), 
        x = "mg/L")
  })
  
  output$plot_visitIGA <- renderPlot({
    plotLabMeasurementAtVisit( lab, !!measurement(), visit(), "IGA" ) +
      labs(
        title = "Immunoglobulin A (IGA)",
        subtitle = visit(), 
        x = "g/L"
      )
  })
}
    
 
