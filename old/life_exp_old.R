
library(methods)
library(dembase)
library(docopt)
library(dplyr)

'
Usage:
life_exp.R [options]

Options:
--first_year [default: 1960]
' -> doc
opts <- docopt(doc)
first_year <- as.integer(opts$first_year)

life_exp_raw <- readRDS("out/life_exp_raw.rds")
sample_description <- readRDS("out/sample_description.rds")
labels_popn_size <- readRDS("out/labels_popn_size.rds")

life_exp <- life_exp_raw %>%
    left_join(sample_description, by = c("code" = "Code")) %>%
    filter(include) %>%
    filter(year >= first_year) %>%
    mutate(country = factor(label, levels = labels_popn_size))

## use 'tapply' rather than 'xtabs' so that years with missing data are
## filled with NAs rather than 0s
life_exp <- tapply(life_exp[["value"]],
                   INDEX = life_exp[c("age", "sex", "year", "country")],
                   FUN = sum) %>%
    Counts(dimscales = c(year = "Intervals"))

saveRDS(life_exp,
        file = "out/life_exp.rds")
