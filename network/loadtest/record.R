library(shinyloadtest)

app_url <- 'http://localhost:3587/'
base <- './network/loadtest'

shinyloadtest::record_session(app_url, output_file = paste(base, '/recording/recording2.log', sep = ''))
