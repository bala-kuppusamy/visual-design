library(shinyloadtest)

base <- './network/loadtest'

data <- shinyloadtest::load_runs('5-Workers' = paste(base, '/out', sep = ''))
shinyloadtest::shinyloadtest_report(data, paste(base, 'report.html', sep = ''))
