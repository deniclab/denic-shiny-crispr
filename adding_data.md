# Updating the WebApp and its data

## Data format

The Shiny app looks for one file for all of its data: combined_spread.rds. This file contains the following columns for each experiment that it can plot:
- [experiment]\_mean\_beta: The mean beta score across all replicates for [experiment].
- [experiment]\_[replicate]\_beta: The beta scores for individual replicates.
- [experiment]\_mean\_rank: The rank ordering of each gene based on the mean beta scores.
- [experiment]\_[replicate]\_rank: The rank ordering of beta scores for each gene from individual replicates.

## Adding new data

#### Adding data to the data file
New data should be added in the same format as described above:
- one column each for mean beta score and rank-ordering from that beta score, named as above.
- one column for beta score for each individual replicate, named as above.
- one column for rank for each individual replicate, named as above.  

Re-save the file with `saveRDS('/path/to/denic-shiny-crispr/combined_spread.rds')` once you're done. If you want to generate different local and web versions, make a new branch of the git repo to store the local version of the data file. If you don't want those data to be publicly available, don't push that branch to GitHub.

#### Adding experiments to the app
Once you have added data to the data file, you need to update the code in a few specific places:
- __helpers.R__
    - `plot_ax_select` at line ~85: the `choices` list within this function must be updated to include the correct set of experiments.
- __app.R__
    - `checkboxGroupInput` at line ~137: The list of choices here needs to be updated to include the correct set of possible inputs. Be sure to update both `choices` and `selected`.

## Running the app locally
To run the app locally, set your working directory to point to the denic-shiny-crispr folder and click Run App in the RStudio window. You may need to install a few additional packages:
- ggplot2
- stringr
- DT
- shiny
- ggrepel

## Updating the web version
To update the web app, you need to update the master branch of the GitHub repo. Once you have cloned the GitHub repo and updated the files as described above:
1. make sure you've updated the files in the master branch of the repo. If you did the work on another branch of the repo, see the notes at the end of this section.
2. `git add [filename]` for each file you updated
3. `git commit -m "[commit message]"` use an informative commit message, e.g. "added LC3-WIPI double mutant screen"
4. `git push origin master` will update the GitHub version and trigger an automagic update of the web version of the application. Give it a few minutes, then check it to make sure it's running correctly at crispr.deniclab.com!

If you made edits on another branch of the repo which you want to then add to the web app, do the following:
1. add and commit all changes within the alternate branch as described above
2. switch to the master version of the repo on your local machine: `git checkout master`
3. merge the changes from the other branch into the master version: `git merge [branch_name]` This may require you to reconcile conflicts between the master branch and the other branch; if so, Google to see how to do that. You'll need to commit the changes again after reconciling conflicts.
4. Push the changes to GitHub with `git push origin master`
