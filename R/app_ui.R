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
    # Your application UI logic
    bs4Dash::dashboardPage(
      title = "TFmapper",
      fullscreen = TRUE,

      header = bs4Dash::dashboardHeader(

        title = bs4Dash::dashboardBrand(
          title = "TFmapper",
          color = "primary",
          image = "www/favicon.png"
        ),

        skin = "black",
        status = "white",
        border = TRUE,
        sidebarIcon = icon("bars"),
        #controlbarIcon = icon("th"),
        fixed = FALSE
      ),

      sidebar = bs4Dash::bs4DashSidebar(
        skin = "black",
        status = "primary",
        elevation = 3,
        collapsed = FALSE,
        expandOnHover = FALSE,
        bs4Dash::sidebarMenu(
          bs4Dash::menuItem("Search",tabName = "search", icon = icon("searchengin"))
          # bs4Dash::menuItem("Statistics", tabName = "stat", icon = icon("tachometer-alt")),
          # bs4Dash::menuItem("About",tabName = "about",icon = icon("fort-awesome-alt")),
          # bs4Dash::menuItem("Help",tabName = "help",icon = icon("book"))
        ) # end of menuItem
      ), # end of sidebarMenu

      body = bs4Dash::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "search", mod_search_home_ui("search_home_ui_1"))
          # shinydashboard::tabItem(tabName = "stat", page_statistics),
          # shinydashboard::tabItem(tabName = "about", page_about),
          # shinydashboard::tabItem(tabName = "help", page_help)
        ) # end of tabItems
      ),

      footer = bs4Dash::dashboardFooter(
        left = a(
          href = "https://github.com/jmzeng1314/TF_map",
          target = "_blank", "@GitHub",
          icon("github")
        ),
        right = "2021"
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
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'TFmapper2'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

