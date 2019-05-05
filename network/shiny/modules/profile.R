
box_items <- function(node) {
  boxProfileItemList(bordered = TRUE,
     boxProfileItem(title = "Class", description = paste0(node$class, ' #', node$id))
  )
}

widget_box <- function(node) {
  widgetUserBox(title = node$name.full,
                width = 12, type = 2, color = "yellow", src = node$picture.medium,
    footer = paste0(node$class, ' #', node$id)
  )
}

regular_box <- function(node) {
  boxProfile(title = node$name.full, subtitle = paste0(node$class, ' #', node$id), src = node$picture.medium)
}

get_color <- function(value) {
  if(is.na(value) || is.null(value)) {
    color <- 'purple'
  } else if(value >= 0 & value <= 1) {
    color <- 'red'
  } else if(value > 1 & value <= 2) {
    color <- 'orange'
  } else if(value > 2 & value <= 3) {
    color <- 'blue'
  } else if(value > 3) {
    color <- 'green'
  } else {
    color <- 'purple'
  }
  color
}

buttons_row <- function(node) {
  box(width = 12, status = NULL,
      appButton(label = "Friendship", icon = "fa fa-users", enable_badge = TRUE,
                badgeColor = get_color(node$degree_pct), badgeLabel = node$degree),
      appButton(label = "Centrality", icon = "fa fa-street-view", enable_badge = TRUE,
                badgeColor = get_color(node$centrality_pct), badgeLabel = node$centrality_pct),
      appButton(label = "Closeness", icon = "fa fa-heart-o", enable_badge = TRUE,
                badgeColor = get_color(node$closeness_pct), badgeLabel = node$closeness_pct),
      appButton(label = "Betweenness", icon = "fa fa-anchor", enable_badge = TRUE,
                badgeColor = get_color(node$betweenness_pct), badgeLabel = node$betweenness_pct)
  )
}

friend_options <- function(path_counts, path_nodes_names, reco, rating) {
  box(width = 12, status = NULL,
      fluidRow(
        column(width = 6,
           boxPad(color = "green",
              descriptionBlock(
                header = path_counts,
                text = path_nodes_names,
                right_border = FALSE,
                margin_bottom = FALSE
              )
           )
        ),
        column(width = 6,
           boxPad(color = get_color(rating),
              descriptionBlock(
                text = reco,
                right_border = FALSE,
                margin_bottom = FALSE
              )
           )
        )
      )
  )
}

get_node_name <- function(node_id, nodes_subset) {
  name <- nodes_subset %>%
    dplyr::filter(id == node_id) %>%
    dplyr::pull(name.full)
  name
}

path_node_names <- function(path_node_ids, nodes) {
  path_counts <- length(path_node_ids)

  limit <- if(length(path_node_ids) > 5) 5 else length(path_node_ids)
  path_node_ids_subset <- path_node_ids[1:limit]

  nodes_subset <- nodes %>%
    dplyr::filter(id %in% path_node_ids_subset)

  path_nodes_names <- sapply(1:length(path_node_ids_subset), function(x) get_node_name(path_node_ids_subset[x], nodes_subset))
  path_nodes_name <- stringr::str_c(path_nodes_names, collapse = ' -> ')
  path_nodes_name <- if(path_counts > length(path_node_ids_subset)) paste0(path_nodes_name, ' ...') else path_nodes_name
  path_nodes_name
}

self_box <- function(node) {
  box(width = 12,
      widget_box(node),
      buttons_row(node)
  )
}

other_box <- function(node, nodes, path_node_ids, neighbor_node_ids) {
  path_counts <- length(path_node_ids)
  path_nodes_names <- path_node_names(path_node_ids, nodes)

  rating <- (node$degree_pct + node$centrality_pct + node$closeness_pct + node$betweenness_pct) / 4
  reco <- if(rating > 2) 'YES' else 'NO'

  if(node$id %in% neighbor_node_ids) {
    rating <- -1
    reco <- 'ALREADY FRIEND'
  }

  box(width = 12,
      regular_box(node),
      buttons_row(node),
      friend_options(path_counts, path_nodes_names, reco, rating)
  )
}

# MODULE: userProfile
userProfile <- function(input, output, session, user_id, is_self, nodes, path_node_ids, neighbor_node_ids) {
  text <- reactive({
    node <- nodes %>%
      dplyr::filter(id == user_id)

    box <- if(is_self) self_box(node) else other_box(node, nodes, path_node_ids, neighbor_node_ids)
    box
  })
  return (text)
}
