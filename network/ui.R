library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(visNetwork)
library(shinydashboardPlus)

dashboardPagePlus(
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
      column(width = 9,
        visNetworkOutput("network_proxy_nodes", height = "900px")
      ),
      column(width = 3)
    )
  ),
  rightsidebar = rightSidebar(width = 500, background = "dark",
    rightSidebarTabContent(id = 1, icon = "address-card", active = TRUE,
      uiOutput(outputId = 'my_profile'),
      conditionalPanel("output.selected == 'TRUE'",
        uiOutput(outputId = 'selected_profile')
      )
    ),
    rightSidebarTabContent(id = 2, icon = "heart",
      timelineBlock(
        timelineEnd(color = "danger"),
        timelineLabel(2018, color = "teal"),
        timelineItem(title = "Item 1", icon = "gears", color = "olive", time = "now", footer = "Here is the footer", "This is the body"),
        timelineItem(title = "Item 2",border = FALSE),
        timelineLabel(2015, color = "orange"),
        timelineItem(title = "Item 3", icon = "paint-brush", color = "maroon",
          timelineItemMedia(src = "http://placehold.it/150x100"),
          timelineItemMedia(src = "http://placehold.it/150x100")
        ),
        timelineStart(color = "gray")
      )
    )
  )
)
