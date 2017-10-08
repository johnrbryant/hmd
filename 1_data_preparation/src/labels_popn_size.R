
library(methods)
library(dembase)
library(dplyr)

exposure_raw <- readRDS("out/exposure_raw.rds")
sample_description <- readRDS("out/sample_description.rds")

popn_size <- exposure_raw %>%
    left_join(sample_description, by = c("code" = "Code")) %>%
    filter(include) %>%
    filter(year == 2010) %>%
    group_by(country) %>%
    tally(count) %>%
    arrange(n)

labels <- popn_size$country

saveRDS(labels,
        file = "out/labels_popn_size.rds")
