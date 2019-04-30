library(shiny)
library(visNetwork)
library(jsonlite)
library(curl)
library(dplyr)
library(stringr)
library(readr)
library(igraph)

source(file = 'data.R')
source(file = 'igraph-data.R')

# using visNetwork proxy for improved rendering performance
shinyServer(function(input, output) {

  sizeOption <- reactive({
    'DEFAULT'
  })

  output$network_proxy_nodes <- renderVisNetwork({
    print('Drawing..')
    v_sizeOption <- sizeOption()

    nodes <- nodes %>%
      mutate(size = if_else(v_sizeOption == 'DEGREE', size_degree, size_default))

    visNetwork(nodes, edges) %>%
      visNodes(shadow = TRUE, shapeProperties = list(useBorderWithImage = TRUE), borderWidth = 5) %>%
      visLayout(randomSeed = 2) %>%
      visOptions(manipulation = FALSE, nodesIdSelection = list(enabled = TRUE, style = 'visibility: hidden;')) %>%
      visInteraction(hideEdgesOnDrag = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE)
  })

  output$my_profile <- renderUI({
    my_profile <- student_filtered %>%
      dplyr::filter(id == my_id)

    selected_node <- nodes_cntr %>%
      dplyr::filter(id == my_id)

    box(width = 12,
      widgetUserBox(title = my_profile$name.full, subtitle = my_profile$email, width = 12, type = 2, color = "yellow",
        src = my_profile$picture.medium,
        boxProfileItemList(bordered = TRUE,
           boxProfileItem(title = "Class", description = paste0(my_profile$class, ' (', my_profile$id, ')'))
        ),
        footer = 'I love my science classes...'
      ),
      box(width = 12, status = NULL,
          appButton(label = "Friends", icon = "fa fa-users", enable_badge = TRUE, badgeColor = "purple", badgeLabel = selected_node$degree),
          appButton(label = "Centrality", icon = "fa fa-street-view", enable_badge = TRUE, badgeColor = "purple", badgeLabel = selected_node$centrality_pct),
          appButton(label = "Closeness", icon = "fa fa-edit", enable_badge = TRUE, badgeColor = "purple", badgeLabel = selected_node$closeness_pct),
          appButton(label = "Betweenness", icon = "fa fa-heart-o", enable_badge = TRUE, badgeColor = "red",badgeLabel = selected_node$betweenness_pct)
      )
    )
  })

  output$selected <- renderText({
    input$network_proxy_nodes_selected != '' & input$network_proxy_nodes_selected != my_id
  })

  output$selected_profile <- renderUI({
    selected_profile <- student_filtered %>%
      dplyr::filter(id == input$network_proxy_nodes_selected)

    selected_node <- nodes_cntr %>%
      dplyr::filter(id == input$network_proxy_nodes_selected)

    box(width = 12, status = "primary",
      boxProfile(title = selected_profile$name.full, subtitle = selected_profile$email,
        src = selected_profile$picture.medium,
       boxProfileItemList(bordered = TRUE,
          boxProfileItem(title = "Class", description = paste0(selected_profile$class, ' (', selected_profile$id, ')'))
       )
      ),
      box(width = 12, status = NULL,
          appButton(label = "Friends", icon = "fa fa-users", enable_badge = TRUE, badgeColor = "purple", badgeLabel = selected_node$degree),
          appButton(label = "Closeness", icon = "fa fa-edit", enable_badge = TRUE, badgeColor = "purple", badgeLabel = selected_node$closeness_pct),
          appButton(label = "Betweenness", icon = "fa fa-heart-o", enable_badge = TRUE, badgeColor = "red",badgeLabel = selected_node$betweenness_pct)
      )
    )
  })

  outputOptions(output, "selected", suspendWhenHidden = FALSE)
})
