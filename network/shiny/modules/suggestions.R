
suggestion_item <- function(nodes, suggestion_id) {
  node <- nodes %>%
    dplyr::filter(id == suggestion_id)

  productListItem(
    src = node$picture.thumbnail,
    productTitle = node$name.full,
    productPrice = paste0(node$degree_pct, ', ', node$centrality_pct, ', ', node$closeness_pct, ', ', node$betweenness_pct),
    priceColor = 'primary',
    paste0(node$class, ' #', node$id)
  )
}

suggestion_list <- function(nodes, suggestion_ids) {
  productList(
    lapply(1:length(suggestion_ids), function(i) {
      suggestion_item(nodes, suggestion_ids[i])
    })
  )
}

suggestion_box <- function(nodes, suggestion_ids) {
  box(width = 12,
      box(
        title = "Friend Suggestions (Top 5)",
        width = NULL,
        status = "primary",
        suggestion_list(nodes, suggestion_ids)
      )
  )
}

# MODULE: suggestions
suggestions <- function(input, output, session, nodes, suggestion_ids) {
  text <- reactive({
    box <- suggestion_box(nodes, suggestion_ids)
    box
  })
  return (text)
}
