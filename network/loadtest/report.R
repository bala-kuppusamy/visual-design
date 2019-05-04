df <- shinyloadtest::load_runs('5-Workers' = './network/loadtest/out')
shinyloadtest::shinyloadtest_report(df, './network/loadtest/report.html')
