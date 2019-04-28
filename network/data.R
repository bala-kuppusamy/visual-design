## data-preparation - START

# 01 --> define constants
folder <- './data/high-school/'
contacts_file <- paste0(folder, 'Contact-diaries-network_data_2013.csv')
students_file <- paste0(folder, 'metadata_2013.txt')
seed <- 20150419
no_of_profiles <- 500
my_id <- 984

# 02 --> download mock profiles
download_profiles <- function() {
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

profiles <- download_profiles()

# 03 --> load student data from files
contact_raw <- readr::read_delim(file = contacts_file, delim = ' ', col_names = c('from', 'to', 'weight'), col_types = 'nnn')
student_raw <- readr::read_tsv(file = students_file, col_names = c('id', 'class', 'gender'), col_types = 'ncc')

# 04 --> data merge utility function definitions
get_random_ssn <- function(gender) {
  gender_profiles <- profiles %>%
    dplyr::filter(gender == gender)

  rand <- sample(1:nrow(gender_profiles), size = 1)
  gender_profiles$ssn[rand]
}

merge_profiles <- function(student_info) {
  set.seed(seed)

  student_info %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ssn = get_random_ssn(gender)) %>%
    dplyr::left_join(profiles, by = 'ssn')
}

add_title <- function(row) {
  img <- paste0('<img src="', row$picture.large, '">')
  name <- paste0('<br><b>', row$name.full,'</b>')
  paste0('<p>', img, name, '</p>')
}

# 05 --> execution of data merge
contact_info <- contact_raw %>%
  dplyr::mutate(width = weight / 2)

student_merged <- student_raw %>%
  merge_profiles()

student_filtered <- student_merged %>%
  dplyr::filter(id %in% contact_info$from | id %in% contact_info$to)

student_filtered$title <- sapply(1:nrow(student_filtered), function(x) add_title(student_filtered[x,]))
# TEMP - TO BE REMOVED
student_filtered <- student_filtered %>%
  dplyr::filter(id > 900)

# 06 --> getting nodes & edges data ready for rendering
nodes <- student_filtered %>%
  dplyr::mutate(label = name.full) %>%
  dplyr::mutate(group = class, size = dplyr::if_else(id == my_id, 50, 20)) %>%
  dplyr::mutate(shape = 'circularImage', image = picture.thumbnail)

edges <- contact_info

## data-preparation - END
