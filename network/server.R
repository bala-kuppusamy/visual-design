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
    if(input$popularity) 'DEGREE' else 'DEFAULT'
  })

  loginId <- reactive({
    if(!is.null(input$login_id) && !is.na(input$login_id)) input$login_id else my_id
  })

  output$network_proxy_nodes <- renderVisNetwork({
    v_sizeOption <- sizeOption()
    v_loginId <- loginId()

    nodes <- nodes %>%
      dplyr::mutate(size_default = dplyr::if_else(id == v_loginId, 50, 20)) %>%
      dplyr::mutate(size = dplyr::if_else(v_sizeOption == 'DEGREE', size_degree, size_default))

    visNetwork(nodes, edges) %>%
      visNodes(shadow = TRUE, shapeProperties = list(useBorderWithImage = TRUE), borderWidth = 5) %>%
      visLayout(randomSeed = 2) %>%
      visOptions(manipulation = FALSE, nodesIdSelection = list(enabled = TRUE, style = 'visibility: hidden;')) %>%
      visInteraction(hideEdgesOnDrag = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE)
  })

  output$my_profile <- renderUI({
    v_loginId <- loginId()

    self_profile_ui <- shiny::callModule(userProfile, 'profile', v_loginId, TRUE, student_filtered, nodes)
    self_profile_ui()
  })

  output$selected <- renderText({
    v_loginId <- loginId()

    input$network_proxy_nodes_selected != '' & input$network_proxy_nodes_selected != v_loginId
  })

  output$selected_profile <- renderUI({
    selected_id <- input$network_proxy_nodes_selected
    other_profile_ui <- shiny::callModule(userProfile, 'profile', selected_id, FALSE, student_filtered, nodes)
    other_profile_ui()
  })

  output$user_list <- renderUI({
    names <- nodes %>%
      select(id, name.full)

    names_options <- setNames(as.character(names$id), names$name.full)
    selectInput(inputId = "login_id", label = "", selected = my_id, choices = names_options)
  })

  shiny::outputOptions(output, "selected", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "user_list", suspendWhenHidden = FALSE)
})
