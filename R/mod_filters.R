#' filters UI Function
#'
#' @description A shiny Module that shows filters to filter the data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Filter Data"),
    h5("Select which data to include in the graphs."),
    
    sliderInput(
      ns("filter_age"), 
      "Age", 
      min = 0, max = 100, value = c(0, 100)
    ),
    
    checkboxGroupInput(
      ns("filter_sex"), 
      "Sex", 
      choices = list("Male" = "M", "Female" = "F", "Undeclared" = "U"), 
      selected = c("M", "F", "U")
    ),
    
    selectizeInput(
      ns("filter_race"), 
      "Race", 
      choices = list(), 
      selected = NULL, 
      multiple = TRUE,
      options = NULL
    ),
    
    sliderInput(
      ns("filter_biomarker1"), 
      "Biomarker 1 at Baseline", 
      min = 0, max = 100, 
      value = c(0, 100)
    ),
    
    checkboxGroupInput(
      ns("filter_biomarker2"), 
      "Biomarker 2 at Baseline", 
      choices = list("High" = "HIGH", "Medium" = "MEDIUM", "Low" = "LOW"), 
      selected = c("HIGH", "MEDIUM", "LOW")
    ),
    
    # sliderInput(
    #   ns("filter_ALT"), "ALT at Sreening", 
    #   min = 0, max = 100, value = c(0, 100)
    # ),
    # 
    # sliderInput(
    #   ns("filter_CRP"), "CRP at Sreening", 
    #   min = 0, max = 100, value = c(0, 100)
    # ),
    # 
    # sliderInput(
    #   ns("filter_IGA"), "IGA at Sreening",
    #   min = 0, max = 100, value = c(0, 100)
    # ),
  )
}
    
#' filters Server Function
#'
#' @noRd 
mod_filters_server <- function(input, output, session){
  ns <- session$ns
  
  # Update Widget values with ranges from the actual data
  r <- range(pat$AGE)
  updateSliderInput(session, "filter_age", min = r[1], max = r[2], value = r)
  
  r <- prettyRange(pat$BMRKR1)
  updateSliderInput(session, "filter_biomarker1", min = r[1], max = r[2], value = r)
  
  # screening <- dplyr::filter(lab, AVISIT == "SCREENING")
  # for (test in c("ALT", "CRP", "IGA")) {
  #   r <- prettyRange( dplyr::filter(screening, LBTESTCD == test)$AVAL )
  #   updateSliderInput(session, paste0("filter_", test), min = r[1], max = r[2], value = r)
  # }
 
  races <- unique(pat$RACE)
  updateSelectizeInput(session, "filter_race", choices = races, selected = races)
  
  return(
    list(
      flt_age        = reactive({ input$filter_age }),
      flt_sex        = reactive({ input$filter_sex }),
      flt_race       = reactive({ input$filter_race }),
      flt_biomarker1 = reactive({ input$filter_biomarker1 }),
      flt_biomarker2 = reactive({ input$filter_biomarker2 })
      #flt_ALT        = reactive({ input$filter_ALT }),
      #flt_CRP        = reactive({ input$filter_CRP }),
      #flt_IGA        = reactive({ input$filter_IGA })
    )
  )
  
}
