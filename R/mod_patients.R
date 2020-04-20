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
    h2("Biomarkers at Baseline"),
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
mod_patients_server <- function(input, output, session, pat_flt, lab_flt){
  ns <- session$ns
  
  checkData <- reactive({
    validate(
      need(nrow(pat_flt()) > 0, "Not enough data selected.")
    )
  })
  
  output$plot_age <- renderPlot({
    checkData()
    pat_flt() %>% 
    plotHistogramByArm(AGE) + 
      labs(title = "Age", x = "Age (years)", y = "count")
  })
  
  output$plot_sex <- renderPlot({
    checkData()
    pat_flt() %>% 
      mutate(SEX = forcats::fct_infreq(SEX)) %>% 
      plotProportionsByArm(SEX) + 
      labs(title = "Sex")
      
  })
  
  output$plot_race <- renderPlot({
    checkData()
    pat_flt() %>%  
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
    checkData()
    pat_flt() %>% 
      plotHistogramByArm(BMRKR1) + 
      labs(title = "Biomarker 1", x = "Measurement")
  })
  
  output$plot_biomarker2 <- renderPlot({
    checkData()
    pat_flt() %>% 
      plotProportionsByArm(BMRKR2) + 
      labs(title = "Biomarker 2")
  })
  
  output$plot_screenALT <- renderPlot({
    checkData()
    lab_flt() %>% 
    plotLabMeasurementAtVisit(AVAL, "SCREENING", "ALT") +
      labs(title = "Alanine Aminotransferase (ALT)", x = "U/L")
  })
  
  output$plot_screenCRP <- renderPlot({
    checkData()
    lab_flt() %>% 
    plotLabMeasurementAtVisit(AVAL, "SCREENING", "CRP") +
      labs(title = "C-Reactive Protein (CRP)", x = "mg/L")
  })
  
  output$plot_screenIGA <- renderPlot({
    checkData()
    lab_flt() %>% 
    plotLabMeasurementAtVisit(AVAL, "SCREENING", "IGA") +
      labs(title = "Immunoglobulin A (IGA)", x = "g/L")
  })
  
}