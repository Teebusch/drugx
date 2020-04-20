#' patients UI Function
#'
#' @description Graphs of patient characteristics.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom magrittr "%>%"
#' 
#' @import ggplot2
#' @import dplyr
mod_patients_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Patients by Study Arm"),
    h2("Patient Characteristics"),
    fluidRow(
      column( width = 4, plotOutput(ns("plot_age")) ),
      column( width = 4, plotOutput(ns("plot_sex")) ),
      column( width = 4, plotOutput(ns("plot_race")) ),
    ),
    h2("Biomarkers at Basline"),
    fluidRow(
      column( width = 6, plotOutput(ns("plot_biomarker1")) ),
      column( width = 6, plotOutput(ns("plot_biomarker2")) ),
    ),
    h2("Lab Measurements at Screening"),
    fluidRow(
      column( width = 4, plotOutput(ns("plot_screenALT")) ),
      column( width = 4, plotOutput(ns("plot_screenCRP")) ),
      column( width = 4, plotOutput(ns("plot_screenIGA")) ),
    ),
  )
}
    
#' patients Server Function
#'
#' @noRd 
mod_patients_server <- function(input, output, session){
  ns <- session$ns
  
  output$plot_age <- renderPlot({
    plotHistogramByArm(pat, AGE) + 
      labs(title = "Age", x = "Age (years)", y = "count")
  })
  
  output$plot_sex <- renderPlot({
    pat %>% 
      mutate(SEX = forcats::fct_infreq(SEX)) %>% 
      plotProportionsByArm(SEX) + 
      labs(title = "Sex")
      
  })
  
  output$plot_race <- renderPlot({
    pat %>% 
      mutate(
        # prettify RACE variable
        RACE = forcats::fct_lump(RACE, prop = 0.01, other_level = "OTHER") %>% 
          stringr::str_wrap(width = 12) %>% 
          forcats::fct_infreq() %>% 
          forcats::fct_rev()
      ) %>% 
      plotProportionsByArm(RACE) + 
      labs(title = "Race")
  })
  
  output$plot_biomarker1 <- renderPlot({
    pat %>% 
      plotHistogramByArm(BMRKR1) + 
      labs(title = "Biomarker 1", x = "Measurement")
  })
  
  output$plot_biomarker2 <- renderPlot({
    pat %>% 
      plotProportionsByArm(BMRKR2) + 
      labs(title = "Biomarker 2")
  })
  
  output$plot_screenALT <- renderPlot({
    plotLabMeasurementAtVisit(lab, "SCREENING", "ALT") +
      labs(title = "Alanine Aminotransferase (ALT)", x = "U/L")
  })
  
  output$plot_screenCRP <- renderPlot({
    plotLabMeasurementAtVisit(lab, "SCREENING", "CRP") +
      labs(title = "C-Reactive Protein (CRP)", x = "mg/L")
  })
  
  output$plot_screenIGA <- renderPlot({
    plotLabMeasurementAtVisit(lab, "SCREENING", "IGA") +
      labs(title = "Immunoglobulin A (IGA)", x = "g/L")
  })
  
}