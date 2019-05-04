download_profiles <- function(seed, no_of_profiles) {
  url <- paste('https://randomuser.me/api/?nat=us&seed=', seed, '&results=', no_of_profiles, sep = '')
  json_raw <- jsonlite::fromJSON(txt = url)
  json_flat <- jsonlite::flatten(json_raw$results)

  json_profiles <- json_flat %>%
    dplyr::mutate(gender = dplyr::if_else(gender == 'male', 'M', 'F')) %>%
    dplyr::mutate(name.first = stringr::str_to_title(name.first), name.last = stringr::str_to_title(name.last)) %>%
    dplyr::mutate(name.full = paste(name.first, name.last, sep = ' ')) %>%
    dplyr::select(id.value, gender, email, dob.age, name.full, picture.large, picture.medium, picture.thumbnail) %>%
    dplyr::rename(ssn = id.value)
  json_profiles
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

  merged <- student_info %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ssn = get_random_ssn(profiles, gender)) %>%
    dplyr::left_join(profiles, by = 'ssn')
  merged
}

add_title <- function(row) {
  img <- paste0('<img src="', row$picture.large, '">')
  name <- paste0('<br><b>', row$name.full,'</b>')
  title <- paste0('<p>', img, name, '</p>')
  title
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

# 02 --> load student data from files
contact_raw <- readr::read_delim(file = contacts_file, delim = ' ', col_names = c('from', 'to', 'weight'), col_types = 'nnn')
student_raw <- readr::read_tsv(file = students_file, col_names = c('id', 'class', 'gender'), col_types = 'ncc')

# 03 --> download mock profiles
profiles <- download_profiles(seed = c_seed, no_of_profiles = c_no_of_profiles)

contact_info <- contact_raw %>%
  dplyr::mutate(width = weight / 2)

# 04 --> execution of data merge
student_merged <- student_raw %>%
  merge_profiles(seed = seed, profiles = profiles)

student_filtered <- student_merged %>%
  dplyr::filter(id %in% contact_info$from | id %in% contact_info$to)

student_filtered$title <- sapply(1:nrow(student_filtered), function(x) add_title(student_filtered[x,]))

# TEMP - TO BE REMOVED
student_filtered <- student_filtered %>%
  dplyr::filter(id > 900)

# 06 --> getting nodes & edges data ready for rendering
nodes <- student_filtered %>%
  dplyr::mutate(label = name.full) %>%
  dplyr::mutate(group = class) %>%
  dplyr::mutate(shape = 'circularImage', image = picture.thumbnail)

edges <- contact_info

nodes <- calcCentrality(nodes = nodes, edges = edges, selected_id = my_id)

## data-preparation - END
