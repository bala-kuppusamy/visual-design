library(shiny)
library(visNetwork)
library(jsonlite)
library(curl)
library(dplyr)
library(stringr)
library(readr)
library(igraph)

source(file = 'igraph-data.R')
source(file = 'data.R')

# using visNetwork proxy for improved rendering performance
shinyServer(function(input, output) {

  my_id <- reactiveVal(c_my_id)
  nodes <- reactiveVal(nodes)
  edges <- reactiveVal(edges)

  sizeOption <- reactive({
    if(input$popularity) 'DEGREE' else 'DEFAULT'
  })

  loginId <- reactive({
    login_id <- input$login_id
    v_my_id <- my_id()

    if(!is.null(login_id) && !is.na(login_id)) login_id else v_my_id
  })

  output$network_proxy_nodes <- renderVisNetwork({
    v_sizeOption <- sizeOption()
    v_loginId <- loginId()
    v_nodes <- nodes()
    v_edges <- edges()

    v_nodes <- v_nodes %>%
      dplyr::mutate(size_default = dplyr::if_else(id == v_loginId, 50, 20)) %>%
      dplyr::mutate(size = dplyr::if_else(v_sizeOption == 'DEGREE', size_degree, size_default))

    visNetwork(v_nodes, v_edges) %>%
      visNodes(shadow = TRUE, shapeProperties = list(useBorderWithImage = TRUE), borderWidth = 5) %>%
      visLayout(randomSeed = 2) %>%
      visOptions(manipulation = FALSE, nodesIdSelection = list(enabled = TRUE, style = 'visibility: hidden;')) %>%
      visInteraction(hideEdgesOnDrag = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE)
  })

  output$my_profile <- renderUI({
    v_loginId <- loginId()
    v_nodes <- nodes()

    self_profile_ui <- shiny::callModule(userProfile, 'profile', v_loginId, TRUE, v_nodes)
    self_profile_ui()
  })

  output$selected <- renderText({
    selected_id <- input$network_proxy_nodes_selected
    v_loginId <- loginId()

    selected_id != '' & selected_id != v_loginId
  })

  output$selected_profile <- renderUI({
    selected_id <- input$network_proxy_nodes_selected
    v_nodes <- nodes()

    other_profile_ui <- shiny::callModule(userProfile, 'profile', selected_id, FALSE, v_nodes)
    other_profile_ui()
  })

  output$user_list <- renderUI({
    v_nodes <- nodes()
    v_my_id <- my_id()

    names <- v_nodes %>%
      dplyr::select(id, name.full)

    names_options <- stats::setNames(as.character(names$id), names$name.full)
    shiny::selectInput(inputId = "login_id", label = "", selected = v_my_id, choices = names_options)
  })

  shiny::outputOptions(output, "selected", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "user_list", suspendWhenHidden = FALSE)
})
