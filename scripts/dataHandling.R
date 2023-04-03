# dataHandling.R 
# data handling functions 
# based on scripts by Niklas Reinken, July 2021 
# version 1, December 2022

if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
}
library(tidyverse)
if (!requireNamespace("janitor", quietly = TRUE)) {
    install.packages("janitor")
}
library(janitor)

data.loadData <- function(whichColumns = NULL, letter = NULL, removeWaZ = T,
    removeWordEnds = F, removeUpperCase = F, removeUnrecognisable = F) {

    # determine the columns to be loaded
    if (is.null(whichColumns)) {
        whichColumns_intern <- c("letter_rec", "WaZ", "word_struc", "code")
    } else {
        whichColumns_intern <- append(whichColumns, c("letter_rec", "WaZ",
            "word_struc", "code"))
    }
    whichColumns_intern <- unique(whichColumns_intern[whichColumns_intern !=
        ""])


    # read in the desired columns only
    d <- suppressMessages(readr::read_csv2("Graphen_MAIN.csv", col_select = all_of(whichColumns_intern),
        show_col_types = FALSE))

    # keep only needed letters
    if (!is.null(letter)) {
        d <- d %>%
            filter(letter_rec %in% letter)
    }

    # remove line break separations
    if (removeWaZ) {
        d <- d %>%
            filter(WaZ != T)
    }

    # remove word ends
    if (removeWordEnds) {
        d <- d %>%
            filter(word_struc != "fin")
    }

    # remove upper case
    if (removeUpperCase) {
        d <- d %>%
            filter(letter_rec != "0")
    }

    # remove unrecognisable letters
    if (removeUnrecognisable) {
        d <- d %>%
            filter(!grepl("99", code))
    }

    # select desired columns
    if (!is.null(whichColumns)) {
        # check if all column names in whichColumns exist in the data
        if (any(!whichColumns %in% colnames(d))) {
            stop("Column name(s) not found in data: ", paste(setdiff(whichColumns,
                colnames(d)), collapse = ", "))
        }
        d <- d %>%
            select(dplyr::any_of(whichColumns))
    }

    # convert to factors and integers
    d <- d %>%
        mutate_if(is.character, as.factor)
    d <- d %>%
        mutate_if(is.logical, as.factor)
    d <- d %>%
        mutate_if(is.double, as.integer)

    return(droplevels(d))
}
