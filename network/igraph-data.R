
# 01 --> prepare data for igraph
calcCentrality <- function(nodes, edges, selected_id) {
  # remove edges with non-existant nodes
  edges_igraph <- edges %>%
    dplyr::filter(from %in% nodes$id & to %in% nodes$id)

  nodes_igraph <- nodes %>%
    dplyr::mutate(type = group, id_2 = id)

  # 02 --> calculate force-directed igraph
  net <- igraph::graph_from_data_frame(d = edges_igraph, vertices = nodes_igraph, directed = F)
  self_node <- igraph::V(net)[id_2 == selected_id]

  # 03 --> calculate centrality measures
  nodes_centrality <- nodes
  nodes_centrality$centrality <- igraph::centr_betw(graph = net)$res
  nodes_centrality$degree <- igraph::degree(graph = net, mode = 'all')
  nodes_centrality$closeness <- igraph::closeness(graph = net)
  nodes_centrality$betweenness <- igraph::betweenness(graph = net)
  nodes_centrality$distances <- igraph::distances(graph = net, v = igraph::V(net), to = self_node, weights = NA)[,1]

  # 04 --> calculate quartile measures
  nodes_centrality$centrality_pct <- dplyr::ntile(nodes_centrality$centrality, 4)
  nodes_centrality$degree_pct <- dplyr::ntile(nodes_centrality$degree, 4)
  nodes_centrality$closeness_pct <- dplyr::ntile(nodes_centrality$closeness, 4)
  nodes_centrality$betweenness_pct <- dplyr::ntile(nodes_centrality$betweenness, 4)
  nodes_centrality$distances_pct <- dplyr::ntile(nodes_centrality$distances, 4)

  # 05 --> calculate node size based on degree
  nodes <- nodes_centrality %>%
    dplyr::mutate(size_degree = 10 + (degree  * 2))

  nodes
}

