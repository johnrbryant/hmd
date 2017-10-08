
library(methods)
library(dembase)
library(docopt)
library(dplyr)

'
Usage:
exposure.R [options]

Options:
--first_year [default: 1960]
' -> doc
opts <- docopt(doc)
first_year <- as.integer(opts$first_year)

exposure_raw <- readRDS("out/exposure_raw.rds")
sample_description <- readRDS("out/sample_description.rds")
labels_popn_size <- readRDS("out/labels_popn_size.rds")

age_breaks <- c(1, seq(5, 100, 5))

exposure <- exposure_raw %>%
    left_join(sample_description, by = c("code" = "Code")) %>%
    filter(include) %>%
    filter(year >= first_year) %>%
    mutate(country = factor(country, levels = labels_popn_size))

## use 'tapply' rather than 'xtabs' so that years with missing data are
## filled with NAs rather than 0s
exposure <- tapply(exposure[["count"]],
                   INDEX = exposure[c("age", "sex", "year", "country")],
                   FUN = sum) %>%
    Counts(dimscales = c(age = "Intervals", year = "Intervals")) %>%
    collapseIntervals(dimension = "age", breaks = age_breaks)

saveRDS(exposure,
        file = "out/exposure.rds")
