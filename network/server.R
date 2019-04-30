library(shiny)
library(visNetwork)
library(jsonlite)
library(curl)
library(dplyr)
library(stringr)
library(readr)

source(file = 'data.R')

# using visNetwork proxy for improved rendering performance
shinyServer(function(input, output) {
  output$network_proxy_nodes <- renderVisNetwork({
    print('Drawing..')
    visNetwork(nodes, edges) %>%
      visNodes(shadow = TRUE, shapeProperties = list(useBorderWithImage = TRUE), borderWidth = 5) %>%
      visLayout(randomSeed = 2) %>%
      visOptions(manipulation = TRUE, nodesIdSelection = list(enabled = TRUE, style = 'visibility: hidden;')) %>%
      visInteraction(hideEdgesOnDrag = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE)
  })

  output$my_profile <- renderUI({
    my_profile <- student_filtered %>%
      dplyr::filter(id == my_id)

    widgetUserBox(title = my_profile$name.full, subtitle = my_profile$email, width = 12, type = 2, color = "yellow",
      src = my_profile$picture.medium,
      boxProfileItemList(bordered = TRUE,
         boxProfileItem(title = "Class", description = paste0(my_profile$class, ' (', my_profile$id, ')')),
         boxProfileItem(title = "# of Friends", description = my_profile$id)
      ),
      footer = 'I love my science classes...'
    )
  })

  output$selected <- renderText({
    input$network_proxy_nodes_selected != '' & input$network_proxy_nodes_selected != my_id
  })

  output$selected_profile <- renderUI({
    selected_profile <- student_filtered %>%
      dplyr::filter(id == input$network_proxy_nodes_selected)

    boxProfile(title = selected_profile$name.full, subtitle = selected_profile$email,
      src = selected_profile$picture.medium,
     boxProfileItemList(bordered = TRUE,
        boxProfileItem(title = "Class", description = paste0(selected_profile$class, ' (', selected_profile$id, ')')),
        boxProfileItem(title = "# of Friends", description = selected_profile$id)
     )
    )
  })

  outputOptions(output, "selected", suspendWhenHidden = FALSE)
})
