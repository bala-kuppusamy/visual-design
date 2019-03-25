
library(MASS)
library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(plotly)
library(leaflet)
library(formattable)

min_year <- 1995
max_year <- 2000
default_day <- 1995

shinyUI(
  dashboardPage(
    dashboardHeader(title = "NASA Weather Data"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Atmosphere", tabName = "atmos", icon = icon("cloud")),
        menuItem("Storms", tabName = "storms", icon = icon("umbrella")),
        menuItem("Glaciers", tabName = "glaciers", icon = icon("snowflake"), badgeLabel = "new", badgeColor = "green")
      ),
      br(),
      h5(" Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "using",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"))
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "atmos",
          fluidRow(
            column(width = 4,
              box(title = 'Monthly average temperature variation in Gulf of Mexico', width = NULL, height = 440, status = "primary", solidHeader = TRUE,
                plotOutput(outputId = "tempYrPlot")
              ),
              box(title = 'Storm count variations across years', width = NULL, height = 440, status = "primary", solidHeader = TRUE,
                formattableOutput(outputId = "stormCountFmtTable")
              )
            ),
            column(width = 8,
              box(title = 'Temperature variation in Gulf of Mexico across years', width = NULL, height = 440, status = "primary", solidHeader = TRUE,
                plotOutput(outputId = "temp6yPlot")
              ),
              box(title = 'Pressure variation in Gulf of Mexico across years', width = NULL, height = 440, status = "primary", solidHeader = TRUE,
                plotOutput(outputId = "pressure6yPlot")
              )
            )
          )
        ),
        tabItem(tabName = "glaciers",
          fluidRow(
            box(title = 'Glacier locations in South America', width = 8, height = 890, status = 'info', solidHeader = TRUE,
              leafletOutput(outputId = "glacierPlot", height = 810)
            ),
            box(title = 'Data', width = 4, height = 890, status = 'info', solidHeader = TRUE,
              DT::dataTableOutput(outputId = "glacierTable")
            )
          )
        ),
        tabItem(tabName = "storms",
          fluidRow(
            column(width = 8,
              box(width = NULL, height = 580, solidHeader = TRUE,
                plotOutput(outputId = "stormPlot", brush = 'plot_brush', click = 'plot_click')
              ),
              fluidRow(
                box(title = 'Wind Speed comparison against 2 adjacent years', width = 6, height = 290, solidHeader = TRUE,
                  plotlyOutput(outputId = "windPlot", height = 240)
                ),
                box(title = 'Pressure comparison against 2 adjacent years', width = 6, height = 290, solidHeader = TRUE,
                  plotlyOutput(outputId = "pressurePlot", height = 240)
                )
              )
            ),
            column(width = 4,
              box(title = "Controls", width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                sliderInput(inputId = "year", label = "Year:", min = min_year, max = max_year, value = min_year, step = 1, sep = ''),
                sliderInput(inputId = "seasonDay", label = "Day of the storm season:", min = 1, max = default_day, value = default_day, step = 1, sep = '')
              ),
              box(title = textOutput(outputId = 'selectedYear'), width = NULL, status = "primary", solidHeader = TRUE, collapsible = FALSE,
                tableOutput(outputId = 'stormCount')
              ),
              conditionalPanel('input.plot_brush != null',
                box(width = NULL, solidHeader = FALSE,
                  DT::dataTableOutput(outputId = "stormTable2")
                )
              )
            )
          )
        )
      )
    )
  )
)
