# cs_crispr
code for Chris Shoemaker's R/Shiny viz of CRISPR screening data

## Current contents of the repository:
- __Shiny_shell.R:__ the R shiny script to run the app.
- __data_manip.R:__ Nick's file for adding rank to Compiled_beta_values.csv, etc.; not relevant to running the Shiny app, just hanging onto it so we can re-capitulate data manipulation from original files if needed.
- __Compiled_beta_values.csv:__ The csv-formatted data file that contains all of the data that's currently used for the viz. Has beta values for each experiment as well as the rank of each gene in the list.
- __All_beta_values_all_screens.csv:__ Chris's file that includes beta values from each sample for each experiment, not currently used in the Shiny app.

## Instructions for getting and implementing this Shiny app on your computer:
### Requirements
- R
- R packages: Shiny, ggplot2, dplyr (and all of their dependencies)
- git
### Acquiring the repo files
- Navigate to the folder that you want to contain this repo
- clone the repo: `git clone https://github.com/deniclab/cs_crispr.git` _(You'll need to enter your github username and password since it's a private repo)_
### Getting the app running
- Change line 2 in Shiny_shell.R to point to the folder _above_ the repo
- Run Shiny_shell.R
