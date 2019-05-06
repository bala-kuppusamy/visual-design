# Load Testing Process
### Shiny Load Test - https://rstudio.github.io/shinyloadtest/

- Step 1.  Start running shiny app.
- Step 2a. Verify the app URL in `record.R`.
- Step 2b. Execute `record.R`; recording log will be stored under `/recording` folder.
- Step 3a. Update the app URL in `load.sh` verify that the *RECORDING_LOG* path points to the recording log.
- Step 3b. Execute `load.sh`; output will be stored under `/output` folder.
- Step 4a. Verify the output path specified in the `report.R`.
- Step 4b. Execute `report.R`; output html will be stored under `/report` folder.
