
# 01 --> prepare data for igraph
# remove edges with non-existant nodes
edges_ig <- edges %>%
  dplyr::filter(from %in% nodes$id & to %in% nodes$id)

nodes_ig <- nodes %>%
  dplyr::mutate(type = group, id_2 = id)

# # 02 --> do igraph
net <- igraph::graph_from_data_frame(d = edges_ig, vertices = nodes_ig, directed = F)
self_node <- igraph::V(net)[id_2 == my_id]

# # 03 --> calculate centrality measures
nodes_cntr <- nodes

nodes_cntr$centrality <- igraph::centr_betw(graph = net)$res
nodes_cntr$degree <- igraph::degree(graph = net, mode = 'all')
nodes_cntr$closeness <- igraph::closeness(graph = net)
nodes_cntr$betweenness <- igraph::betweenness(graph = net)
nodes_cntr$distances <- igraph::distances(graph = net, v = igraph::V(net), to = self_node, weights = NA)[,1]

nodes_cntr$centrality_pct <- dplyr::ntile(nodes_cntr$centrality, 4)
nodes_cntr$degree_pct <- dplyr::ntile(nodes_cntr$degree, 4)
nodes_cntr$closeness_pct <- dplyr::ntile(nodes_cntr$closeness, 4)
nodes_cntr$betweenness_pct <- dplyr::ntile(nodes_cntr$betweenness, 4)
nodes_cntr$distances_pct <- dplyr::ntile(nodes_cntr$distances, 4)

nodes <- nodes_cntr %>%
  dplyr::mutate(size_degree = 10 + (degree  * 2))
  # dplyr::mutate(group = if_else(id == my_id, 'SELF', group))

