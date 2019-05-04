
calc_centrality <- function(nodes, edges, selected_id) {
  # 01 --> remove edges with non-existant nodes
  edges_igraph <- edges %>%
    dplyr::filter(from %in% nodes$id & to %in% nodes$id)

  nodes_igraph <- nodes %>%
    dplyr::mutate(type = group, id_2 = id)

  # 02 --> calculate force-directed igraph
  net <- igraph::graph_from_data_frame(d = edges_igraph, vertices = nodes_igraph, directed = F)
  self_node <- igraph::V(net)[id_2 == selected_id]

  # 03 --> calculate centrality measures
  nodes$centrality <- igraph::centr_betw(graph = net)$res
  nodes$degree <- igraph::degree(graph = net, mode = 'all')
  nodes$closeness <- igraph::closeness(graph = net)
  nodes$betweenness <- igraph::betweenness(graph = net)
  nodes$distances <- igraph::distances(graph = net, v = igraph::V(net), to = self_node, weights = NA)[,1]

  # 04 --> calculate quartile measures
  nodes$centrality_pct <- dplyr::ntile(nodes$centrality, 4)
  nodes$degree_pct <- dplyr::ntile(nodes$degree, 4)
  nodes$closeness_pct <- dplyr::ntile(nodes$closeness, 4)
  nodes$betweenness_pct <- dplyr::ntile(nodes$betweenness, 4)
  nodes$distances_pct <- dplyr::ntile(nodes$distances, 4)

  # 05 --> calculate node size based on degree
  nodes <- nodes %>%
    dplyr::mutate(size_degree = 10 + (degree  * 2))

  nodes
}

