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

  self_profile_ui <- callModule(userProfile, 'profile', my_id, TRUE, student_filtered, nodes)

  output$my_profile <- renderUI({
    self_profile_ui()
  })

  output$selected <- renderText({
    input$network_proxy_nodes_selected != '' & input$network_proxy_nodes_selected != my_id
  })

  output$selected_profile <- renderUI({
    id <- input$network_proxy_nodes_selected
    other_profile_ui <- callModule(userProfile, 'profile', id, FALSE, student_filtered, nodes)
    other_profile_ui()
  })

  outputOptions(output, "selected", suspendWhenHidden = FALSE)
})
