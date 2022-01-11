#' search_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_search_results_ui <- function(id){
  ns <- NS(id)
  tagList(





  )
}

#' search_results Server Functions
#'
#' @noRd
mod_search_results_server <- function(id,results_o=""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$text <- renderText({

      paste("Hi , waite for you")

    })

    observeEvent({
      results_o$do_position()
      results_o$do_gene()},{

      print("here to go results")
      print(class(results_o))
      print(length(results_o))
      print(class(results_o[[1]]))
      print(length(results_o[[1]]))
      print(dim(results_o[[1]]))
      print(is.null(results_o[[1]]))
      print(is.null(results_o[[1]]))
      print(results_o[[1]])
      print(nrow(results_o[[1]]))
      print(ncol(results_o[[1]]))

    })
    #
    # print(nrow(results_o[[1]]))

    # rows_of_res <- observe({
    #
    #   nrow(results_o[[1]])
    #
    # })
#
#     if(!is.null(nrow(results_o[[1]]))){
#
#       dat <- reactive({
#
#         results_o$results_o()
#
#       })
#
#       output$gene_dat <- DT::renderDT({
#         DT::datatable(dat())
#       })
#
#     }


    # print(class(dat()))
    # #
    # # output$text <- renderText({
    # #   paste0(dim(results_o))
    # #   })
    # #
    # output$gene_dat <- DT::renderDT({
    #   DT::datatable(dat())
    # })

    # output$coord_dat <- DT::renderDT({
    #   DT::datatable(table2())
    # })


  })
}

## To be copied in the UI
# mod_search_results_ui("search_results_ui_1")

## To be copied in the server
# mod_search_results_server("search_results_ui_1")
