library(shinyloadtest)

base <- './network/loadtest'

data <- shinyloadtest::load_runs('5-Workers' = paste(base, '/output/out3', sep = ''))
shinyloadtest::shinyloadtest_report(data, paste(base, '/report/report3.html', sep = ''))
