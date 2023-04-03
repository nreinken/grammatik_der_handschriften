# graphics.R 
# functions for graphical output and diagrams
# based on scripts by Niklas Reinken, July 2021 
# version 2, March 2023

if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
}
library(tidyverse)

#' Function to plot the results of a contingency analysis (association plot)
#'
#'  @param test A contingency table test object
#'  @param x_title Title for the x-axis
#'  @param y_title Title for the y-axis
#'  @param eps Whether to save the plot as an EPS file (default is TRUE)
#'  @param png Whether to save the plot as a PNG file (default is TRUE)
#'  
#'  @return A ggplot object
plot_assoc <- function(test, x_title = "", y_title = "", eps = TRUE, png = TRUE) {
    # Check input parameters
    if (!is.character(x_title)) {
        stop("Input 'x_title' must be a character string.")
    }
    if (!is.character(y_title)) {
        stop("Input 'y_title' must be a character string.")
    }
    if (!is.logical(eps)) {
        stop("Input 'eps' must be a logical value.")
    }
    if (!is.logical(png)) {
        stop("Input 'png' must be a logical value.")
    }

    # Create a data frame of residuals with color and count columns
    resids <- data.frame(test$residuals)
    resids$color <- ifelse(resids$Freq < 0, "negative", "positive")
    resids$count <- data.frame(test$observed)$Freq

    p <- ggplot(resids, aes(x = resids[, 2], y = Freq, fill = color)) + geom_bar(position = "dodge",
        stat = "identity", width = resids$count/sum(resids$count)) + scale_fill_grey() +
        facet_wrap(~resids[, 1], dir = "v", strip.position = "left") + theme_minimal() +
        theme(legend.position = "none", panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()) + scale_y_continuous(position = "right") +
        xlab("") + ylab("Residuals") + geom_hline(aes(yintercept = 0))

    if (eps) {
        ggsave(paste0("graphs/assoc_", x_title, "_", y_title, ".eps"), width = 3,
            height = 3)
    }
    if (png) {
        ggsave(paste0("graphs/assoc_", x_title, "_", y_title, ".png"), width = 3,
            height = 3)
    }
    return(p)
}

#' Function to plot the results of a regression (coefficient plot)
#'
#'  @param coefs A data frame of regression coefficients and their standard errors
#'  @param name Name for the plot
#'  @param eps Whether to save the plot as an EPS file (default is TRUE)
#'  @param png Whether to save the plot as a PNG file (default is TRUE)
#'  
#'  @return A ggplot object
plot_coefs <- function(coefs, name, eps = T, png = T) {
    p <- ggplot(data = coefs, aes(x = reorder(names, coefs), y = coefs)) +
        geom_point(stat = "identity", size = 5, fill = "white", color = "#222222",
            shape = 16) + geom_segment(aes(y = 0, x = names, yend = coefs,
        xend = names), color = "#222222") + geom_text(aes(y = pos, label = str_c(names,
        ": ", coefs)), color = "#222222", size = 3) + geom_segment(aes(x = length(names) +
        1, y = 0, yend = 0, xend = 0), color = "black") + coord_flip() + labs(y = "Coefficients",
        x = "") + theme_minimal() + theme(axis.text.y = element_blank()) +
        ylim(-3, 3)

    if (eps) {
        ggsave(paste0("graphs/coefs_", name, ".eps"), width = 4, height = 8)
    }
    if (png) {
        ggsave(paste0("graphs/coefs_", name, ".png"), width = 4, height = 8)
    }

    return(p)
}
