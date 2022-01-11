#' search_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_search_home_ui <- function(id){
  ns <- NS(id)
  tagList(

    # fluidRow(
    #   shinyjs::useShinyjs(),
    #   # column(width = 4,
             mod_search_options_ui("search_options_ui_1")
      # ),
      # column(width = 8,
      #        mod_search_results_ui("search_results_ui_1")
      # )
    # )

  )
}

#' search_home Server Functions
#'
#' @noRd
mod_search_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_search_home_ui("search_home_ui_1")

## To be copied in the server
# mod_search_home_server("search_home_ui_1")
