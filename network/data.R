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

get_random_ssn <- function(profiles, gender) {
  gender_profiles <- profiles %>%
    dplyr::filter(gender == gender)

  rand <- sample(1:nrow(gender_profiles), size = 1)
  ssn <- gender_profiles$ssn[rand]
  ssn
}

merge_profiles <- function(student_info, seed, profiles) {
  set.seed(seed)

  nodes <- student_info %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ssn = get_random_ssn(profiles, gender)) %>%
    dplyr::left_join(profiles, by = 'ssn')
  nodes
}

add_title <- function(row) {
  img <- paste0('<img src="', row$picture.large, '">')
  name <- paste0('<br><b>', row$name.full,'</b>')
  title <- paste0('<p>', img, name, '</p>')
  title
}

build_edges <- function(contacts) {
  edges <- contacts %>%
    dplyr::mutate(width = weight / 2)
  edges
}

build_nodes <- function(students, profiles, seed) {
  nodes <- students %>%
    merge_profiles(seed = seed, profiles = profiles)
  nodes$title <- sapply(1:nrow(nodes), function(x) add_title(nodes[x,]))

  nodes <- nodes %>%
    dplyr::mutate(label = name.full) %>%
    dplyr::mutate(group = class) %>%
    dplyr::mutate(shape = 'circularImage', image = picture.thumbnail)
  nodes
}

filter_nodes <- function(nodes, edges) {
  nodes <- nodes %>%
    dplyr::filter(id %in% edges$from | id %in% edges$to)

  # TEMP - TO BE REMOVED
  nodes <- nodes %>%
    dplyr::filter(id > 900)
  nodes
}

## data-preparation - START

# 01 --> define constants
c_rds_folder <- './data/'
c_folder <- './data/high-school/'
c_contacts_file <- paste0(folder, 'Contact-diaries-network_data_2013.csv')
c_students_file <- paste0(folder, 'metadata_2013.txt')
c_seed <- 20150419
c_no_of_profiles <- 500
c_my_id <- 984

# 02 --> load edges data from file
contacts_raw <- readr::read_delim(file = contacts_file, delim = ' ', col_names = c('from', 'to', 'weight'), col_types = 'nnn')
edges <- build_edges(contacts = contacts_raw)

# 03 --> load student data from file & download mock profiles from web service
students_raw <- readr::read_tsv(file = students_file, col_names = c('id', 'class', 'gender'), col_types = 'ncc')
profiles <- download_profiles(seed = c_seed, no_of_profiles = c_no_of_profiles)

# 04 --> execution of data merge & filtering for nodes
nodes <- build_nodes(students = students_raw, profiles = profiles, seed = seed)
nodes <- filter_nodes(nodes = nodes, edges = edges)

# 05 --> add centrality measures from igraph to the nodes
nodes <- calc_centrality(nodes = nodes, edges = edges, selected_id = my_id)

## data-preparation - END
