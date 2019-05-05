# Visual Analytics
### Visual Analytics modules using R, R Markdown, Shiny

These modules were prepared as part of Visual Analytics graduate course work in UNC Charlotte in 2019. 

More details of these modules can be found in the blog -
https://data-science-5122-blog.netlify.com/

## A. Storms
Contains a visualization of NASA dataset to identify the storm path & weather pattern.

Shiny Application -
https://balavigneswaran-kuppusamy.shinyapps.io/nasa/

Web link to the presentation - 
https://data-science-5122.netlify.com/storms/design-contest.html#1

### Major R Libraries used
shiny, ggplot2, tidyverse, sf, DT, plotly, leaflet, ggridges, formattable, xaringan (slides)

### Example visualization
![Storm Pattern](https://github.com/bala-kuppusamy/visual-design/blob/master/storms/storm-pattern.png)

### Example code
```` R
# storms tab -> geo plot with storm paths.
output$stormPlot <- renderPlot({
storms_for_plot <- storms_for_season_day()

active_storms <- storms_for_plot %>%
    dplyr::group_by(name) %>%
    dplyr::filter(last_seasday > max(seasday)) %>%
    dplyr::filter(seasday == max(seasday), hour == max(hour))

plot + geom_path(data = storms_for_plot, mapping = aes(x = long, y = lat, group = name, color = type), size = 1.2, alpha = 0.5) +
    geom_point(data = active_storms, mapping = aes(x = long, y = lat), color = 'Red', size = 3) +
    labs(color = 'Storm Type', caption = 'Source: NASA weather data from 1995 - 2000') +
    theme_map() + theme(legend.position = 'bottom', text = element_text(size = 15))
}, height = 550, width = 1000)
````

## B. Network
This module contains a visualization of High school friends network dataset to visualize the social network connection, & providing networking recommendations. This module is a proof of concept for the visual interface & experience for social networking.

The user names, & profile pictures were obtained from https://randomuser.me & randomly matched with the student dataset.

Shiny Application -
https://balavigneswaran-kuppusamy.shinyapps.io/network/

### Web link to the presentation & report
https://data-science-5122.netlify.com/network/presentation.html

https://data-science-5122.netlify.com/network/report.html

### Data source
High school friends network dataset - 
http://www.sociopatterns.org/datasets/high-school-contact-and-friendship-networks/

User names & profile pictures - 
https://randomuser.me

### Major R Libraries used
shiny, visNetwork, igraph, tidyverse, jsonlite

### Example visualization
![Storm Pattern](https://github.com/bala-kuppusamy/visual-design/blob/master/network/network.png)

### Example code
#### visNetwork
```` R
visNetwork(v_nodes, v_edges) %>%
    visNodes(shadow = TRUE, shapeProperties = list(useBorderWithImage = TRUE), borderWidth = 5) %>%
    visLayout(randomSeed = 2) %>%
    visOptions(manipulation = FALSE, nodesIdSelection = list(enabled = TRUE, style = 'visibility: hidden;')) %>%
    visInteraction(hideEdgesOnDrag = TRUE) %>%
    visPhysics(stabilization = FALSE) %>%
    visEdges(smooth = FALSE)
````

#### igraph
```` R
net <- igraph::graph_from_data_frame(d = edges_igraph, vertices = nodes_igraph, directed = F)
nodes$centrality <- igraph::centr_betw(graph = net)$res
nodes$degree <- igraph::degree(graph = net, mode = 'all')
nodes$closeness <- igraph::closeness(graph = net)
nodes$betweenness <- igraph::betweenness(graph = net)
path <- igraph::shortest_paths(graph = net, from = from_node, to = to_node, output = 'both')
neighbors <- igraph::neighbors(graph = net, v = selected_node)
````