# pVal.R 
# automatically perform bonferoni-holm-tests to p-values
# based on scripts by Niklas Reinken, July 2021
# version 3, March 2023

# load required packages
if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
}
library(tidyverse)

pval <- function(pvalue, title) {

    # validate input arguments
    if (!is.numeric(pvalue)) {
        stop("pvalue must be numeric")
    }
    if (is.na(title) || title == "") {
        stop("title must be a non-empty string")
    }

    # create tibble for p-values in the global environment, if it
    # doesn't exist
    if (!exists("pvals")) {
        assign("pvals", dplyr::tibble(id = "", pvalue = 0), envir = globalenv())
    }

    # store p-value
    pvals.loc <- pvals %>%
        add_row(tibble(id = title, pvalue = pvalue)) %>%
        filter(id != "") %>%
        distinct(id, .keep_all = TRUE)

    # apply bonferoni-holm correction
    pvals.loc$padj <- round(p.adjust(pvals.loc$pvalue, method = "holm"), 5)


    # store pvals in the global environment to use it in the next call
    # of this function
    assign("pvals", pvals.loc, envir = globalenv())

    # write pvals to csv.file
    readr::write_csv(pvals, "results/pvalues.csv")

    # return adjusted p-value
    case <- filter(pvals, id == title)
    return(case$padj)
}
