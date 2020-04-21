#' patients UI Function
#'
#' @description Graphs of patient characteristics.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' @import ggplot2
#' @import dplyr
mod_patients_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Patients by Study Arm"),
    h2("Patient Characteristics"),
    fluidRow(
      column(width = 4, plotOutput(ns("plot_age"))),
      column(width = 4, plotOutput(ns("plot_sex"))),
      column(width = 4, plotOutput(ns("plot_race"))),
    ),
    h2("Biomarkers at Baseline"),
    fluidRow(
      column(width = 6, plotOutput(ns("plot_biomarker1"))),
      column(width = 6, plotOutput(ns("plot_biomarker2"))),
    ),
    h2("Lab Measurements at Screening"),
    fluidRow(
      column(width = 4, plotOutput(ns("plot_screenALT"))),
      column(width = 4, plotOutput(ns("plot_screenCRP"))),
      column(width = 4, plotOutput(ns("plot_screenIGA"))),
    ),
  )
}
    
#' patients Server Function
#'
#' @noRd 
mod_patients_server <- function(input, output, session, pat_flt, lab_flt) {
  ns <- session$ns
  
  check_data <- reactive({
    validate(
      need(nrow(pat_flt()) > 0, 
           "Busy loading the data or not enough data selected.")
    )
  })
  
  output$plot_age <- renderPlot({
    check_data()
    pat_flt() %>% 
    plot_histogram_by_arm(AGE) + 
      labs(title = "Age", x = "Age (years)", y = "count")
  })
  
  output$plot_sex <- renderPlot({
    check_data()
    pat_flt() %>% 
      mutate(SEX = forcats::fct_infreq(SEX)) %>% 
      plot_proportions_by_arm(SEX) + 
      labs(title = "Sex")
  })
  
  output$plot_race <- renderPlot({
    check_data()
    pat_flt() %>%  
      mutate(
        # prettify RACE variable
        RACE = forcats::fct_lump(RACE, prop = 0.01, other_level = "OTHER") %>% 
          stringr::str_wrap(width = 12) %>% 
          forcats::fct_infreq() %>% 
          forcats::fct_rev()
      ) %>% 
      plot_proportions_by_arm(RACE) + 
      labs(title = "Race")
  })
  
  output$plot_biomarker1 <- renderPlot({
    check_data()
    pat_flt() %>% 
      plot_histogram_by_arm(BMRKR1) + 
      labs(title = "Biomarker 1", x = "Measurement")
  })
  
  output$plot_biomarker2 <- renderPlot({
    check_data()
    pat_flt() %>% 
      plot_proportions_by_arm(BMRKR2) + 
      labs(title = "Biomarker 2")
  })
  
  output$plot_screenALT <- renderPlot({
    check_data()
    lab_flt() %>% 
      plot_lab_measurement_at_visit(AVAL, "SCREENING", "ALT") +
      labs(title = "Alanine Aminotransferase (ALT)", x = "U/L")
  })
  
  output$plot_screenCRP <- renderPlot({
    check_data()
    lab_flt() %>% 
      plot_lab_measurement_at_visit(AVAL, "SCREENING", "CRP") +
      labs(title = "C-Reactive Protein (CRP)", x = "mg/L")
  })
  
  output$plot_screenIGA <- renderPlot({
    check_data()
    lab_flt() %>% 
      plot_lab_measurement_at_visit(AVAL, "SCREENING", "IGA") +
      labs(title = "Immunoglobulin A (IGA)", x = "g/L")
  })
  
}
