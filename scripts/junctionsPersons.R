# junctionsPersons.R 
# tests the junction frequency for each individual text
# based on scripts by Niklas Reinken, July 2021
# version 2, January 2023

# load required packages
if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
}

# load required functions
source("scripts/dataHandling.R")

# disable scientific notation
options(scipen = 0)

# load data
data <- data.loadData(whichColumns = c("person_ID", "junc_border"), removeWaZ = T,
    removeWordEnds = T, removeUpperCase = F, removeUnrecognisable = F)

# convert person_ID to factor
data$person_ID <- as.factor(data$person_ID)

# get percentages
prop.table(table(data$junc_border))

# calculate junction frequencies for each person
junction_freqs <- data %>%
    group_by(person_ID) %>%
    summarize(connected = round(prop.table(table(junc_border))[1], 3), not_connected = round(prop.table(table(junc_border))[2],
        3))

# Write junction frequencies to CSV
write_csv2(junction_freqs, "results/junctionRates_person.csv")

# add handwriting style (cursive, mixed, block)
breaks <- c(0, 0.2, 0.8, 1)
tags <- c("block", "mixed", "cursive")
junction_freqs$handwriting <- cut(junction_freqs$connected, breaks = breaks,
    include.lowest = TRUE, right = FALSE, labels = tags)

# Print summary of handwriting groups
summary(junction_freqs$handwriting)

# plot density of junction rates
ggplot(data = junction_freqs, aes(x = connected)) + geom_histogram(binwidth = 0.05,
    alpha = 0.5, fill = "#222222") + geom_density(color = "black") + labs(x = "Junction rate",
    y = "Number of texts") + theme_minimal()

# Save plot to file
ggsave("graphs/density_junctionRates.png", width = 5, height = 3)
ggsave("graphs/density_junctionRates.eps", width = 5, height = 3, device = cairo_ps)
