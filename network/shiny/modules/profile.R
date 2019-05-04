
box_items <- function(node) {
  boxProfileItemList(bordered = TRUE,
     boxProfileItem(title = "Class", description = paste0(node$class, ' #', node$id))
  )
}

widget_box <- function(node) {
  widgetUserBox(title = node$name.full, subtitle = node$email, width = 12, type = 2,
                color = "yellow", src = node$picture.medium,
    box_items(node),
    footer = 'I love my science classes...'
  )
}

regular_box <- function(node) {
  boxProfile(title = node$name.full, subtitle = node$email, src = node$picture.medium,
    box_items(node)
  )
}

get_color <- function(value) {
  if(is.na(value) || is.null(value)) {
    color <- 'grey'
  } else if(value <= 1) {
    color <- 'red'
  } else if(value == 2) {
    color <- 'orange'
  } else if(value == 3) {
    color <- 'blue'
  } else if(value == 4) {
    color <- 'green'
  } else {
    color <- 'grey'
  }
  color
}

# MODULE: userProfile
userProfile <- function(input, output, session, user_id, is_self, nodes) {
  text <- reactive({
    node <- nodes %>%
      dplyr::filter(id == user_id)

    user_box <- if(is_self) widget_box(node) else regular_box(node)

    box(width = 12,
        user_box,
        box(width = 12, status = NULL,
            appButton(label = "Friends", icon = "fa fa-users", enable_badge = TRUE,
                      badgeColor = get_color(node$degree_pct), badgeLabel = node$degree),
            appButton(label = "Centrality", icon = "fa fa-street-view", enable_badge = TRUE,
                      badgeColor = get_color(node$centrality_pct), badgeLabel = node$centrality_pct),
            appButton(label = "Closeness", icon = "fa fa-heart-o", enable_badge = TRUE,
                      badgeColor = get_color(node$closeness_pct), badgeLabel = node$closeness_pct),
            appButton(label = "Betweenness", icon = "fa fa-anchor", enable_badge = TRUE,
                      badgeColor = get_color(node$betweenness_pct), badgeLabel = node$betweenness_pct)
        )
    )
  })
  return (text)
}
