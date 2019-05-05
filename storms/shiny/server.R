
library(MASS)
library(shiny)
library(tidyverse)
library(nasaweather)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(DT)
library(lubridate)
library(plotly)
library(leaflet)
library(ggthemes)
library(ggridges)
library(formattable)

# data-prep -> plotting the world map
world <- ne_countries(scale = "medium", returnclass = "sf")
plot <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-112.15, -4.12), ylim = c(2.65, 53.97), expand = FALSE)

# data-prep -> glaciers data
glaciers <- nasaweather::glaciers

# data-prep -> atmospheric data
atmos <- nasaweather::atmos
kelvin <- 272.15

gulf <- atmos %>%
  dplyr::filter(lat < 40 & lat > 10) %>%
  dplyr::filter(long > -100 & long < -70) %>%
  dplyr::mutate(mon = factor(month), temp_c = temp - kelvin, surftemp_c = surftemp - kelvin)

gulf_yr_summary <- gulf %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(avg_yr_temp = mean(temp_c), avg_yr_surftemp = mean(surftemp_c), avg_yr_pressure = mean(pressure))

gulf_summary <- gulf %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(avg_temp = mean(temp_c), avg_surftemp = mean(surftemp_c), avg_pressure = mean(pressure))

gulf <- gulf %>%
  dplyr::inner_join(gulf_yr_summary, by = "month") %>%
  dplyr::inner_join(gulf_summary, by = c("year", "month"))

# data-prep -> storms data
storms <- nasaweather::storms %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(last_seasday = max(seasday))

storms_info <- storms %>%
  dplyr::select(name, year, month, day, hour, wind) %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(date = dmy(paste(day, month, 1995, sep = '/'))) %>%
  dplyr::mutate(min_date = min(date), max_date = max(date), max_wind = max(wind)) %>%
  dplyr::select(name, year, min_date, max_date, max_wind) %>%
  dplyr::distinct()

storms_season <- storms %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(min_day = min(seasday), max_day = max(seasday)) %>%
  dplyr::mutate(start = 1, end = max_day - min_day + 1) %>%
  dplyr::select(year, min_day, max_day, start, end) %>%
  dplyr::distinct()

hurricanes <- storms %>%
  dplyr::filter(type == 'Hurricane') %>%
  dplyr::select(year, name) %>%
  dplyr::distinct() %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(hurricanes = n())

count_by_year <- storms %>%
  dplyr::select(year, name) %>%
  dplyr::distinct() %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(storms = n()) %>%
  dplyr::inner_join(hurricanes, by = "year") %>%
  dplyr::mutate(severity = dplyr::if_else(hurricanes >= 10, 'HIGH', dplyr::if_else(hurricanes < 5, 'LOW', 'MEDIUM')))

# class representation to track the combination of year & date selected in UI
setClass("YearDay", representation(year = "numeric", day = "numeric"))
yearDay <- new("YearDay", year = 1995, day = 154)

shinyServer(function(input, output, session) {

  ## listening to UI actions

  # action #1 -> change of year results in recalc of date limits & value
  observeEvent(input$year, {
    season <- storms_season %>%
      dplyr::filter(year == input$year)
    updateSliderInput(session = session, inputId = 'seasonDay', min = season$start, max = season$end, value = season$end)
  })

  # action #2 -> change of year / date results in recalc of YearDay data structure.
  year_day <- reactive({
    if(!is.null(input$year)) {
      attr(yearDay, 'year') <- input$year
    }
    if(!is.null(input$seasonDay)) {
      attr(yearDay, 'day') <- input$seasonDay
    }
    yearDay
  })

  # action #3 -> brushing.
  brush <- reactive({
    if(is.null(input$plot_brush)) {
      NULL
    } else {
      input$plot_brush
    }
  })

  ## recalculating reactive variables based on UI inputs

  # throttling / debouncing the actions to drop changes within few milliseconds.
  year_day_throttled <- year_day %>% debounce(500)

  year <- reactive({
    req(year_day_throttled())
    year_day <- year_day_throttled()
    attr(year_day, 'year')
  })

  day <- reactive({
    req(year_day_throttled())
    year_day <- year_day_throttled()
    attr(year_day, 'day')
  })

  brush_thr <- brush

  season_for_year <- reactive({
    req(year())
    storms_season %>%
      dplyr::filter(year == year())
  })

  storms_for_season_day <- reactive({
    req(year(), day())
    season <- season_for_year()
    storms %>%
      dplyr::filter(year == year(), seasday <= day() - 1 + season$min_day)
  })

  storms_brushed <- reactive({
    req(brush_thr())
    brushedPoints(df = storms_for_season_day(), brush = brush_thr(), xvar = 'long', yvar = 'lat') %>%
      dplyr::select(name) %>%
      dplyr::distinct()
  })

  storm_year_comparison <- reactive({
    req(year())
    storms %>%
      dplyr::filter(year %in% c(year() - 1, year() + 1)) %>%
      dplyr::bind_rows(storms_for_season_day()) %>%
      dplyr::mutate(year_ch = as.character(year))
  })

  ## producing output to be rendered in UI

  # storms tab -> geo plot with storm paths.
  output$stormPlot <- renderPlot({
    storms_for_plot <- storms_for_season_day()

    active_storms <- storms_for_plot %>%
      dplyr::group_by(name) %>%
      dplyr::filter(last_seasday > max(seasday)) %>%
      dplyr::filter(seasday == max(seasday), hour == max(hour))

    plot + geom_path(data = storms_for_plot, mapping = aes(x = long, y = lat, group = name, color = type), size = 1.2, alpha = 0.5) +
      geom_point(data = active_storms, mapping = aes(x = long, y = lat), color = 'Red', size = 3) +
      labs(color = 'Storm Type', caption = 'Source: NASA weather data from 1995 - 2000') +
      theme_map() + theme(legend.position = 'bottom', text = element_text(size = 15))
  }, height = 550, width = 1000)

  # storms tab -> wind speed trend plot.
  output$windPlot <- renderPlotly({
    p <- storm_year_comparison() %>%
      ggplot(mapping = aes(x = wind, group = year_ch, color = year_ch, fill = year_ch)) +
      geom_density(alpha = 0.4) +
      labs(title = '', x = '', y = '', color = '', fill = '')
    ggplotly(p = p)
  })

  # storms tab -> pressure trend plot.
  output$pressurePlot <- renderPlotly({
    p <- storm_year_comparison() %>%
      ggplot(mapping = aes(x = pressure, group = year_ch, color = year_ch, fill = year_ch)) +
      geom_density(alpha = 0.4) +
      labs(title = '', x = '', y = '', color = '', fill = '')
    ggplotly(p = p)
  })

  # storms tab -> display text for selected year.
  output$selectedYear <- renderText({
    paste('Year: ', year())
  })

  # storms tab -> table for storm count.
  output$stormCount <- renderTable({
    count <- count_by_year %>%
      dplyr::filter(year == year())

    tbl <- tribble(
      ~x, ~y,
      'Total # of Storms:', count$storms,
      'Total # of Hurricanes:', count$hurricanes
    )
    tbl
  }, striped = FALSE, hover = FALSE, bordered = TRUE, rownames = FALSE, colnames = FALSE)

  # storms tab -> datatable for brushed storms list.
  output$stormTable2 <- DT::renderDataTable({
    storms_info %>%
      dplyr::filter(name %in% storms_brushed()$name) %>%
      dplyr::select(name, min_date, max_date, max_wind)
  }, options = list(dom = 'pt'), colnames = c('Name', 'Start Date', 'End Date', 'Max Wind Speed'))

  # glaciers tab -> geo leaflet plot for glacier locations.
  output$glacierPlot <- renderLeaflet({
    adj <- 0.5
    border <- glaciers %>%
      dplyr::summarise(min_long = min(long) - adj, max_long = max(long) + adj, min_lat = min(lat) - adj, max_lat = max(lat) + adj)

    leaf <- leaflet(glaciers, options = leafletOptions(minZoom = 4, maxZoom = 15, height = 700)) %>%
      addTiles() %>%
      addRectangles(lng1 = border$min_long, lng2 = border$max_long, lat1 = border$min_lat, lat2 = border$max_lat, fillColor = 'transparent') %>%
      addMiniMap(toggleDisplay = TRUE) %>%
      addCircles(lng = ~long, lat = ~lat, radius = 1000, color = '#00c0ff', fillColor = '#00c0ff', label = ~id)
    leaf
  })

  # glaciers tab -> datatable for glacier locations list.
  output$glacierTable <- DT::renderDataTable({
    glaciers %>%
      dplyr::select(name, lat, long, area, country)
  }, options = list(dom = 'pt'), colnames = c('Glacier Name', 'Latitude', 'Longitude', 'Area', 'Country'))

  # atmosphere tab -> formattable for yearly hurricane counts.
  output$stormCountFmtTable <- renderFormattable({
    names(count_by_year) = c('Year', 'Storms', 'Hurricanes', 'Severity')
    formattable(count_by_year, list(
      area(col = `Storms`) ~ normalize_bar("pink", 0.2),
      area(col = `Hurricanes`) ~ normalize_bar("orange", 0.2),
      `Severity` = formatter("span",
                             style = x ~ style(color = ifelse(x == 'HIGH', 'red', ifelse(x == 'LOW', 'green', 'orange'))),
                             x ~ icontext(ifelse(x == 'HIGH', "arrow-up", ifelse(x == 'LOW', 'arrow-down', 'arrow-right'))))
    ))
  })

  # atmosphere tab -> ridgeline plot for displaying consolidated monthly trend of temperature.
  output$tempYrPlot <- renderPlot({
    p_temp_yr <- gulf %>%
      ggplot(mapping = aes(x = temp_c, y = mon, color = avg_yr_temp, fill = avg_yr_temp)) +
      geom_density_ridges(rel_min_height = 0.01, bandwidth = 1, alpha = 0.4) +
      labs(x = 'Temperature (C)', y = 'Month', color = 'Avg Temp (C)', fill = 'Avg Temp (C)') +
      theme(legend.position = 'bottom') +
      # scale_colour_brewer(palette = 'heat')
      scale_colour_gradient(low = 'Orange', high = 'Red') +
      scale_fill_gradient(low = 'Orange', high = 'Red') +
      scale_y_discrete(labels = function(x) {
        month.abb[as.numeric(x)]
      })
    p_temp_yr
  }, height = 380, width = 480)

  # atmosphere tab -> ridgeline plot for displaying yearly variations in monthly temperature.
  output$temp6yPlot <- renderPlot({
    p_temp <- gulf %>%
      dplyr::filter(month > 4 & month < 11) %>%
      ggplot(mapping = aes(x = temp_c, y = mon, color = avg_temp, fill = avg_temp)) +
      geom_density_ridges(rel_min_height = 0.01, bandwidth = 1, alpha = 0.4) +
      facet_wrap(~ year) +
      labs(x = 'Temperature (C)', y = 'Month', color = 'Avg Temp (C)', fill = 'Avg Temp (C)') +
      scale_colour_gradient(low = 'Orange', high = 'Red') +
      scale_fill_gradient(low = 'Orange', high = 'Red') +
      scale_y_discrete(labels = function(x) {
        month.abb[as.numeric(x)]
      })
    p_temp
  }, height = 380, width = 1050)

  # atmosphere tab -> ridgeline plot for displaying yearly variations in monthly pressure.
  output$pressure6yPlot <- renderPlot({
    p_pressure <- gulf %>%
      dplyr::filter(month > 4 & month < 11) %>%
      dplyr::filter(pressure > 940) %>%
      ggplot(mapping = aes(x = pressure, y = mon, color = avg_pressure, fill = avg_pressure)) +
      geom_density_ridges(rel_min_height = 0.01, bandwidth = 1, alpha = 0.4) +
      facet_wrap(~ year) +
      labs(x = 'Pressure (mb)', y = 'Month', color = 'Avg Pressure (mb)', fill = 'Avg Pressure (mb)') +
      scale_colour_gradient(low = 'Purple', high = 'Blue') +
      scale_fill_gradient(low = 'Purple', high = 'Blue') +
      scale_y_discrete(labels = function(x) {
        month.abb[as.numeric(x)]
      })
    p_pressure
  }, height = 380, width = 1050)

})
