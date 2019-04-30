library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

dashboardPagePlus(
  header = dashboardHeaderPlus(
    fixed = TRUE,
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears"
  ),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(),
  rightsidebar = rightSidebar(width = 450,
    background = "dark",
    rightSidebarTabContent(
      id = 1,
      icon = "desktop",
      active = TRUE,
      widgetUserBox(
        title = "Nadia Carmichael",
        subtitle = "lead Developer",
        width = 12,
        type = 2,
        src = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
        color = "yellow",
        "Some text here!",
        footer = "The footer here!"
      ),
      box(
        width = 12,
        title = "Box with profile",
        status = "primary",
        boxProfile(
          src = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
          title = "Nina Mcintire",
          subtitle = "Software Engineer",
          boxProfileItemList(
            bordered = TRUE,
            boxProfileItem(
              title = "Followers",
              description = 1322
            ),
            boxProfileItem(
              title = "Following",
              description = 543
            ),
            boxProfileItem(
              title = "Friends",
              description = 13287
            )
          )
        )
      ),
      box(
        width = 12,
        title = "App Buttons",
        status = NULL,
        appButton(
          url = "http://google.com",
          label = "Users",
          icon = "fa fa-users",
          enable_badge = TRUE,
          badgeColor = "purple",
          badgeLabel = 891
        ),
        appButton(
          label = "Edit",
          icon = "fa fa-edit",
          enable_badge = FALSE,
          badgeColor = NULL,
          badgeLabel = NULL
        ),
        appButton(
          label = "Likes",
          icon = "fa fa-heart-o",
          enable_badge = TRUE,
          badgeColor = "red",
          badgeLabel = 3
        )
      )
      ),
    rightSidebarTabContent(
      id = 2,
      title = "Tab 2",
        timelineBlock(
          timelineEnd(color = "danger"),
          timelineLabel(2018, color = "teal"),
          timelineItem(
            title = "Item 1",
            icon = "gears",
            color = "olive",
            time = "now",
            footer = "Here is the footer",
            "This is the body"
          ),
          timelineItem(
            title = "Item 2",
            border = FALSE
          ),
          timelineLabel(2015, color = "orange"),
          timelineItem(
            title = "Item 3",
            icon = "paint-brush",
            color = "maroon",
            timelineItemMedia(src = "http://placehold.it/150x100"),
            timelineItemMedia(src = "http://placehold.it/150x100")
          ),
          timelineStart(color = "gray")
        )
      ),
    rightSidebarTabContent(
      id = 3,
      icon = "paint-brush",
      box(
        title = "rightSidebarMenu",
        width = NULL,
        rightSidebarMenu(
          rightSidebarMenuItem(
            icon = menuIcon(
              name = "birthday-cake",
              color = "red"
            ),
            info = menuInfo(
              title = "Langdon s Birthday",
              description = "Will be 23 on April 24th"
            )
          ),
          rightSidebarMenuItem(
            icon = menuIcon(
              name = "user",
              color = "yellow"
            ),
            info = menuInfo(
              title = "Frodo Updated His Profile",
              description = "New phone +1(800)555-1234"
            )
          )
        )
      )
      )
  ),
  title = "Right Sidebar"
)
