library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(visNetwork)
library(shinydashboardPlus)

dashboardPagePlus(title = "Social Butterfly",
  header = dashboardHeaderPlus(fixed = TRUE, enable_rightsidebar = TRUE, rightSidebarIcon = "users",
    left_menu = tagList(
      dropdownBlock(id = "view_options", title = "Change View", icon = icon("gears"),
        prettySwitch(inputId = "popularity", label = "Show by popularity", fill = TRUE, status = "primary")),
      dropdownBlock(id = "change_user", title = "Login as", icon = icon("gears"),
        uiOutput(outputId = 'user_list')
    ))),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    fluidRow(
      tags$head(tags$link(rel="shortcut icon", href="./favicon.png")),
      column(width = 9,
        visNetworkOutput("network_proxy_nodes", height = "900px")
      ),
      column(width = 3)
    )
  ),
  rightsidebar = rightSidebar(width = 500, background = "dark",
    rightSidebarTabContent(id = 1, icon = "address-card", active = TRUE,
      uiOutput(outputId = 'my_profile'),
      conditionalPanel("output.is_selected == 'TRUE'",
         uiOutput(outputId = 'selected_profile')
      ),
      conditionalPanel("output.is_selected == 'FALSE'",
         uiOutput(outputId = 'friend_suggestions')
      )
    )
  )
)
