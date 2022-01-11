#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  mod_search_home_server("search_home_ui_1")
  mod_search_options_server("search_options_ui_1")
  # mod_search_results_server("search_results_ui_1",results_o=results_o)
}
