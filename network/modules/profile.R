
box_items <- function(profile) {
  boxProfileItemList(bordered = TRUE,
     boxProfileItem(title = "Class", description = profile$class),
     boxProfileItem(title = "ID", description = profile$id)
  )
}

widget_box <- function(profile) {
  widgetUserBox(title = profile$name.full, subtitle = profile$email, width = 12, type = 2, color = "yellow", src = profile$picture.medium,
    box_items(profile),
    footer = 'I love my science classes...'
  )
}

regular_box <- function(profile) {
  boxProfile(title = profile$name.full, subtitle = profile$email, src = profile$picture.medium,
    box_items(profile)
  )
}


get_color <- function(value) {
  if(is.na(value)) {
    color <- 'purple'
  } else if(value <= 1) {
    color <- 'red'
  } else if(value == 2) {
    color <- 'orange'
  } else if(value == 3) {
    color <- 'green'
  } else {
    color <- 'blue'
  }
  color
}

userProfile <- function(input, output, session, user_id, is_self, profiles, nodes) {
  text <- reactive({
    profile <- profiles %>%
      dplyr::filter(id == user_id)

    node <- nodes %>%
      dplyr::filter(id == user_id)

    user_box <- if(is_self) widget_box(profile) else regular_box(profile)

    box(width = 12,
        user_box,
        box(width = 12, status = NULL,
            appButton(label = "Friends", icon = "fa fa-users", enable_badge = TRUE, badgeColor = 'purple', badgeLabel = node$degree),
            appButton(label = "Centrality", icon = "fa fa-street-view", enable_badge = TRUE, badgeColor = 'purple', badgeLabel = node$centrality_pct),
            appButton(label = "Closeness", icon = "fa fa-heart-o", enable_badge = TRUE, badgeColor = 'purple', badgeLabel = node$closeness_pct),
            appButton(label = "Betweenness", icon = "fa fa-anchor", enable_badge = TRUE, badgeColor = 'red', badgeLabel = node$betweenness_pct)
        )
    )
  })

  return (text)
}
