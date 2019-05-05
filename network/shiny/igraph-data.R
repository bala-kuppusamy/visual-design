
build_igraph <- function(nodes, edges) {
  # 01 --> remove edges with non-existant nodes
  edges_igraph <- edges %>%
    dplyr::filter(from %in% nodes$id & to %in% nodes$id)

  nodes_igraph <- nodes %>%
    dplyr::mutate(type = group, id_2 = id)

  # 02 --> calculate force-directed igraph
  net <- igraph::graph_from_data_frame(d = edges_igraph, vertices = nodes_igraph, directed = F)
  net
}

calc_centrality <- function(net, nodes) {
  # 03 --> calculate centrality measures
  nodes$centrality <- igraph::centr_betw(graph = net)$res
  nodes$degree <- igraph::degree(graph = net, mode = 'all')
  nodes$closeness <- igraph::closeness(graph = net)
  nodes$betweenness <- igraph::betweenness(graph = net)

  # 04 --> calculate quartile measures
  nodes$centrality_pct <- dplyr::ntile(nodes$centrality, 4)
  nodes$degree_pct <- dplyr::ntile(nodes$degree, 4)
  nodes$closeness_pct <- dplyr::ntile(nodes$closeness, 4)
  nodes$betweenness_pct <- dplyr::ntile(nodes$betweenness, 4)

  # 05 --> calculate node size based on degree
  nodes <- nodes %>%
    dplyr::mutate(size_degree = 10 + (degree  * 2))

  nodes
}

calc_distance <- function(net, nodes, selected_id) {
  self_node <- igraph::V(net)[id_2 == selected_id]

  nodes$distances <- igraph::distances(graph = net, v = igraph::V(net), to = self_node, weights = NA)[,1]
  nodes$distances_pct <- dplyr::ntile(nodes$distances, 4)
  nodes
}

calc_path <- function(net, from_id, to_id) {
  from_node <- igraph::V(net)[id_2 == from_id]
  to_node <- igraph::V(net)[id_2 == to_id]
  path <- igraph::shortest_paths(graph = net, from = from_node, to = to_node, output = 'both')
  path
}

friend_suggestions <- function(nodes) {
  potential <- nodes %>%
    dplyr::filter(centrality_pct > 2 & degree_pct > 2 & closeness_pct > 2 & betweenness_pct > 2)

  potential
}

