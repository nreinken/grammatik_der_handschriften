# contingencyTests.R 
# flexible functions to test the automatically test the contingency of letter form, junction and grammatical features
# based on scripts by Niklas Reinken, July 2021 
# version 1, December 2022

# load required functions
source("scripts/pval.R")
source("scripts/graphics.R")

# load required packages
if (!requireNamespace("vcd", quietly = TRUE)) {
    install.packages("vcd")
}
library(vcd)
if (!requireNamespace("chisq.posthoc.test", quietly = TRUE)) {
    install.packages("chisq.posthoc.test")
}
library(chisq.posthoc.test)

# contingency test function
cont_test <- function(data, x_title = "", y_title = "", alpha = 0.05) {

    if (!is.data.frame(data) || ncol(data) != 2) {
        stop("Input data must be a data frame with two columns.")
    }

    # check if there are enough levels for contingency tests
    levels_x <- nlevels(data[[1]])
    levels_y <- nlevels(data[[2]])
    if (levels_x <= 1 || levels_y <= 1) {
        print("At least one column has only one factor level; no further contingency analysis possible.")
        return()
    }

    # create plot title and pval ID
    title = paste0(x_title, "X", y_title)

    # print the frequency table
    t <- table(data)
    print(t)

    # determine whether to use Fisher's exact test or chi-square
    suppressWarnings(pre_test <- chisq.test(t))
    fisher <- if (min(pre_test$expected) < 5)
        TRUE else FALSE

    # calculate contingency tests
    if (fisher) {
        print("Fisher test")
        print(test <- fisher.test(t, simulate.p.value = TRUE))
    } else {
        print("Chi² test")
        print(test <- chisq.test(t))
    }

    # return if not significant
    if (test$p.value > alpha) {
        print("### not significant ###")
        return(list(table = t, test = test))
    }

    # apply bonferoni-holm correction
    padj <- pval(pval = test$p.value, title = title)
    print(paste0("p.adj = ", padj))
    if (padj >= alpha) {
        print("### adjusted p-value is not significant ###")
    } else {
        print("adjusted p-value is significant")

        # run post-hoc tests
        print(vcd::assocstats(t))
        print(chisq.posthoc.test::chisq.posthoc.test(t))

        # create graphical output
        suppressWarnings(plot_assoc(chisq.test(t), x_title = x_title, y_title = y_title))
    }

    # return contingency table and test results
    return(list(table = t, test = test))
}
