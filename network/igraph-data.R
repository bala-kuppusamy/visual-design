
# 01 --> prepare data for igraph
# remove edges with non-existant nodes
edges_ig <- edges %>%
  filter(from %in% nodes$id & to %in% nodes$id)

nodes_ig <- nodes %>%
  mutate(type = group, id_2 = id)

# # 02 --> do igraph
net <- graph_from_data_frame(d = edges_ig, vertices = nodes_ig, directed = F)
self_node <- V(net)[id_2 == my_id]

# # 03 --> calculate centrality measures
nodes_cntr <- nodes

nodes_cntr$centrality <- centr_betw(graph = net)$res
nodes_cntr$degree <- degree(graph = net, mode = 'all')
nodes_cntr$closeness <- closeness(graph = net)
nodes_cntr$betweenness <- betweenness(graph = net)
nodes_cntr$distances <- distances(graph = net, v = V(net), to = self_node, weights = NA)[,1]

nodes_cntr$centrality_pct <- ntile(nodes_cntr$centrality, 4)
nodes_cntr$degree_pct <- ntile(nodes_cntr$degree, 4)
nodes_cntr$closeness_pct <- ntile(nodes_cntr$closeness, 4)
nodes_cntr$betweenness_pct <- ntile(nodes_cntr$betweenness, 4)
nodes_cntr$distances_pct <- ntile(nodes_cntr$distances, 4)

nodes <- nodes_cntr %>%
  mutate(size_degree = 10 + (degree  * 2))
  # mutate(group = if_else(id == my_id, 'SELF', group))

