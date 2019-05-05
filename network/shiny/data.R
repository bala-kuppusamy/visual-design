# download user profiles json from web API & do data prep.
download_profiles <- function(seed, no_of_profiles) {
  url <- paste('https://randomuser.me/api/?nat=us&seed=', seed, '&results=', no_of_profiles, sep = '')
  json_raw <- jsonlite::fromJSON(txt = url)
  json_flat <- jsonlite::flatten(json_raw$results)

  profiles <- json_flat %>%
    dplyr::mutate(gender = dplyr::if_else(gender == 'male', 'M', 'F')) %>%
    dplyr::mutate(name.first = stringr::str_to_title(name.first), name.last = stringr::str_to_title(name.last)) %>%
    dplyr::mutate(name.full = paste(name.first, name.last, sep = ' ')) %>%
    dplyr::select(id.value, gender, email, dob.age, name.full, picture.large, picture.medium, picture.thumbnail) %>%
    dplyr::rename(ssn = id.value)
  profiles
}

# find a random ssn (matching the gender) to match student with user profile
get_random_ssn <- function(profiles, gender) {
  gender_profiles <- profiles %>%
    dplyr::filter(gender == gender)

  rand <- sample(1:nrow(gender_profiles), size = 1)
  ssn <- gender_profiles$ssn[rand]
  ssn
}

# merge student info with user profile matching by ssn.
merge_profiles <- function(student_info, seed, profiles) {
  set.seed(seed)

  nodes <- student_info %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ssn = get_random_ssn(profiles, gender)) %>%
    dplyr::left_join(profiles, by = 'ssn')
  nodes
}

# add a mouseover tooltip for each node
add_title <- function(row) {
  img <- paste0('<img src="', row$picture.large, '">')
  name <- paste0('<br>', row$name.full,' <br>', row$class, ' #', row$id)
  title <- paste0('<p>', img, name, '</p>')
  title
}

# build edges from the contacts file
# --> save to rds for future use
build_edges <- function(contacts_file) {
  contacts <- readr::read_delim(file = contacts_file, delim = ' ', col_names = c('from', 'to', 'weight'), col_types = 'nnn')
  edges <- contacts %>%
    dplyr::mutate(width = weight / 2)

  saveRDS(object = edges, file = c_rds_edges)
  print('Edges: Saved to rds file for future use.')
  edges
}

# build nodes from the students file
# --> download user profiles from web API
# --> merge student info with user profiles
# --> build igraph for calculating centrality measures
# --> save igraph to rds for future use
# --> calculate centrality measures using igraph & store it in nodes
# --> save nodes to rds for future use
build_nodes <- function(students_file, seed, no_of_profiles, edges, rds_nodes, rds_net) {
  # load student data from file & download mock profiles from web service
  students <- readr::read_tsv(file = c_students_file, col_names = c('id', 'class', 'gender'), col_types = 'ncc')
  profiles <- download_profiles(seed = seed, no_of_profiles = no_of_profiles)

  nodes <- students %>%
    merge_profiles(seed = seed, profiles = profiles)
  nodes$title <- sapply(1:nrow(nodes), function(x) add_title(nodes[x,]))

  nodes <- nodes %>%
    dplyr::mutate(label = name.full) %>%
    dplyr::mutate(group = class) %>%
    dplyr::mutate(shape = 'circularImage', image = picture.thumbnail)

  nodes <- nodes %>%
    dplyr::filter(id %in% edges$from | id %in% edges$to)

  if(c_mode_development) {
    nodes <- nodes %>%
      dplyr::filter(id > 900)
  }

  # build igraph
  net <- build_igraph(nodes = nodes, edges = edges)
  saveRDS(object = net, file = rds_net)
  print('igraph: Saved to rds file for future use.')

  # add centrality measures from igraph to the nodes
  nodes <- calc_centrality(net = net, nodes = nodes)

  saveRDS(object = nodes, file = rds_nodes)
  print('Nodes: Saved to rds file for future use.')
  nodes
}

## data-preparation - START

# define constants
c_rds_edges <- './data/edges.rds'
c_rds_nodes <- './data/nodes.rds'
c_rds_net <- './data/net.rds'
c_contacts_file <- './data/high-school/Contact-diaries-network_data_2013.csv'
c_students_file <- './data/high-school/metadata_2013.txt'

c_seed <- 20150419
c_no_of_profiles <- 500
c_my_id <- 984
# mode should be OFF before deploying to server.
c_mode_development <- TRUE

# load edges either from rds or csv
if(file.exists(c_rds_edges) & !c_mode_development) {
  print('Edges: Loading from pre-stored rds file.')
  edges <- readRDS(file = c_rds_edges)
} else {
  print('Edges: Loading from raw csv file.')
  edges <- build_edges(contacts_file = c_contacts_file)
}

# load nodes either from rds or csv
if(file.exists(c_rds_nodes) & file.exists(c_rds_net) & !c_mode_development) {
  print('Nodes: Loading from pre-stored rds file.')
  nodes <- readRDS(file = c_rds_nodes)
} else {
  print('Nodes: Loading from raw csv file.')
  nodes <- build_nodes(c_students_file, c_seed, c_no_of_profiles, edges, c_rds_nodes, c_rds_net)
}

# load igraph either from rds
net <- readRDS(file = c_rds_net)

## data-preparation - END
