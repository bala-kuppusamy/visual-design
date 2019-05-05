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
  net <- reactiveVal(net)
  edges_2 <- reactive({
    edges <- tibble::rowid_to_column(edges, 'id')
    edges
  })

  sizeOption <- reactive({
    if(input$popularity) 'DEGREE' else 'DEFAULT'
  })

  loginId <- reactive({
    login_id <- input$login_id
    v_my_id <- my_id()

    if(!is.null(login_id) && !is.na(login_id)) login_id else v_my_id
  })

  isSelected <- reactive({
    selected_id <- input$network_proxy_nodes_selected
    v_loginId <- loginId()

    !is.null(selected_id) && selected_id != '' && selected_id != v_loginId
  })

  selectedId <- reactive({
    v_isSelected <- isSelected()
    selected_id <- input$network_proxy_nodes_selected

    id <- if(v_isSelected) selected_id else NULL
    id
  })

  output$my_profile <- renderUI({
    v_loginId <- loginId()
    v_nodes <- nodes()

    self_profile_ui <- shiny::callModule(userProfile, 'profile', v_loginId, NULL, TRUE, v_nodes)
    self_profile_ui()
  })

  output$is_selected <- renderText({
    v_isSelected <- isSelected()
    v_isSelected
  })

  output$selected_profile <- renderUI({
    v_selectedId <- selectedId()
    v_loginId <- loginId()
    v_nodes <- nodes()
    v_net <- net()

    if(!is.null(v_selectedId)) {
      path <- calc_path(v_net, v_loginId, v_selectedId)
      path_nodes <- v_nodes[unlist(path$vpath), ]
      path_node_ids <- path_nodes %>%
        dplyr::pull(id)

      other_profile_ui <- shiny::callModule(userProfile, 'profile', v_selectedId, path_node_ids, FALSE, v_nodes)
      other_profile_ui()
    }
  })

  # observe({
  #   v_selectedId <- selectedId()
  #   v_loginId <- loginId()
  #   v_nodes <- nodes()
  #   v_edges <- edges_2()
  #   v_net <- net()
  #
  #   if(!is.null(v_selectedId)) {
  #     path <- calc_path(v_net, v_loginId, v_selectedId)
  #     path_nodes <- unlist(path$vpath)
  #     path_nodes <- v_nodes[path_nodes, ]
  #     path_node_ids <- path_nodes %>%
  #       dplyr::select(id)
  #     print(path_node_ids)
  #
  #     path_edges <- unlist(path$epath)
  #     path_edges <- v_edges[path_edges, ]
  #     path_edge_ids <- path_edges %>%
  #       dplyr::select(id)
  #     print(path_edge_ids)
  #
  #     visNetworkProxy("network_proxy_nodes") %>%
  #       visSelectNodes(id = path_node_ids) %>%
  #       visSelectEdges(id = path_edge_ids)
  #   }
  # })

  output$network_proxy_nodes <- renderVisNetwork({
    v_sizeOption <- sizeOption()
    v_loginId <- loginId()
    v_nodes <- nodes()
    v_edges <- edges_2()

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

  output$user_list <- renderUI({
    v_nodes <- nodes()
    v_my_id <- my_id()

    names <- v_nodes %>%
      dplyr::select(id, name.full)

    names_options <- stats::setNames(as.character(names$id), names$name.full)
    shiny::selectInput(inputId = "login_id", label = "", selected = v_my_id, choices = names_options)
  })

  shiny::outputOptions(output, "is_selected", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "user_list", suspendWhenHidden = FALSE)
})
