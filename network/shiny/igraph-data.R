
# build igraph from nodes & edges
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

# calculate centrality measures from the igraph & add them to the nodes dataframe
calc_centrality <- function(net, nodes) {
  # 03 --> calculate centrality measures
  nodes$centrality <- igraph::centr_eigen(graph = net)$vector
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

## unused method
calc_distance <- function(net, nodes, selected_id) {
  self_node <- igraph::V(net)[id_2 == selected_id]

  nodes$distances <- igraph::distances(graph = net, v = igraph::V(net), to = self_node, weights = NA)[,1]
  nodes$distances_pct <- dplyr::ntile(nodes$distances, 4)
  nodes
}

# calculate shortest distance between 2 nodes & identifies ids of all nodes in the path.
calc_path_node_ids <- function(net, nodes, from_id, to_id) {
  from_node <- igraph::V(net)[id_2 == from_id]
  to_node <- igraph::V(net)[id_2 == to_id]
  path <- igraph::shortest_paths(graph = net, from = from_node, to = to_node, output = 'both')
  path_nodes <- nodes[unlist(path$vpath), ]
  path_node_ids <- path_nodes %>%
    dplyr::pull(id)
  path_node_ids
}

# identify all immediate neighbors in the igraph for a given node & get their ids.
calc_neighbor_node_ids <- function(net, nodes, selected_id) {
  selected_node <- igraph::V(net)[id_2 == selected_id]
  neighbors <- igraph::neighbors(graph = net, v = selected_node)
  neighbor_nodes <- nodes[unlist(neighbors), ]
  neighbor_node_ids <- neighbor_nodes %>%
    dplyr::pull(id)
  neighbor_node_ids <- unique(neighbor_node_ids)
  neighbor_node_ids
}

# potential friend suggestions
# --> exclude self
# --> exclude immediate neighbors
# --> ensure ALL centrality measures should be in the 3rd or 4th quartile.
# --> descending order of total centrality measures
# --> sliced with top 5.
friend_suggestions <- function(net, nodes, selected_id) {
  neighbor_node_ids <- calc_neighbor_node_ids(net, nodes, selected_id)
  # friend suggestion should exclude self & existing friends
  suggestion_ids <- nodes %>%
    dplyr::filter(id != selected_id) %>%
    dplyr::filter(!(id %in% neighbor_node_ids)) %>%
    dplyr::filter(centrality_pct > 2 & degree_pct > 2 & closeness_pct > 2 & betweenness_pct > 2) %>%
    dplyr::mutate(sum_pct = (centrality_pct + degree_pct + closeness_pct + betweenness_pct)) %>%
    dplyr::arrange(dplyr::desc(sum_pct)) %>%
    dplyr::slice(1:5) %>%
    dplyr::pull(id)
  suggestion_ids
}

