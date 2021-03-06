#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(

      titlePanel("Drug X Clinical Trial", 
                 windowTitle = "Drug X Clinical Trial"),
  
      sidebarLayout(
        
        sidebarPanel(
          width = 3,
          mod_filters_ui("filters_ui_1"),
        ),
        
        mainPanel(
          mod_patients_ui("patients_ui_1"),
          mod_trial_ui("trial_ui_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  
  add_resource_path(
    "www", app_sys("app/www")
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "drugx"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

