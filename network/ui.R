library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(visNetwork)
library(shinydashboardPlus)

dashboardPagePlus(
  header = dashboardHeaderPlus(fixed = TRUE, enable_rightsidebar = TRUE, rightSidebarIcon = "users"),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    fluidRow(
      column(width = 9,
        visNetworkOutput("network_proxy_nodes", height = "900px")
      ),
      column(width = 3)
    )
  ),
  rightsidebar = rightSidebar(width = 450, background = "dark",
    rightSidebarTabContent(id = 1, icon = "address-card", active = TRUE,
      uiOutput(outputId = 'my_profile'),
      conditionalPanel("output.selected == 'TRUE'",
        box(width = 12, status = "primary",
          uiOutput(outputId = 'selected_profile')
        ),
        box(width = 12, status = NULL,
          appButton(url = "http://google.com", label = "Users", icon = "fa fa-users", enable_badge = TRUE, badgeColor = "purple", badgeLabel = 891),
          appButton(label = "Edit", icon = "fa fa-edit", enable_badge = FALSE, badgeColor = NULL, badgeLabel = NULL),
          appButton(label = "Likes", icon = "fa fa-heart-o", enable_badge = TRUE, badgeColor = "red",badgeLabel = 3)
        )
      )
    ),
    rightSidebarTabContent(id = 2, icon = "history",
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
    ),
    rightSidebarTabContent(id = 3, icon = "calendar",
      box(width = NULL,
        rightSidebarMenu(
          rightSidebarMenuItem(
            icon = menuIcon(name = "birthday-cake", color = "red"),
            info = menuInfo(title = "Langdon's Birthday", description = "Will be 23 on April 24th")
          ),
          rightSidebarMenuItem(
            icon = menuIcon(name = "user", color = "yellow"),
            info = menuInfo(title = "Frodo Updated His Profile", description = "New phone +1(800)555-1234")
          )
        )
      )
    )
  )
)
