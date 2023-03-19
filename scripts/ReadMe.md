# Table of contents

Most of these scripts are supplementary scripts which are used by the analysis scripts. You probably will only need to use:

- ***graph_contingency.R***: all analyses performed as contingency analysis (chi²-tests)

- ***graph_regression.R*** : all regression analyses

- ***kappa.R***: analysis of intra-rater-agreement

- ***shapesByPersonAndLetter.R***: descriptive analysis of letter forms per person and per letter

The other files are used to perform the analyses, but are called by the scripts:

- ***contingencyTests.R***: function to perfom contingency tests
- ***dataHandling.R***: function to load data from the corpus file
- ***graphics.R***: functions for graphical output
- ***pval.R***: function to store the p-values to perfom bonferroni correction later
- ***regressions.R***: support function for regressiona analysis
