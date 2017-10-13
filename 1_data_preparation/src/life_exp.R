
library(methods)
library(dembase)
library(demlife)
library(docopt)
library(dplyr)

'
Usage:
life_exp.R [options]

Options:
--first_year [default: 1950]
' -> doc
opts <- docopt(doc)
first_year <- as.integer(opts$first_year)

deaths <- readRDS("out/deaths.rds")
exposure <- readRDS("out/exposure.rds")

n_country <- length(dimnames(deaths)$country)
deaths_small <- deaths %>%
    slab(dimension = "country", elements = 1:3) %>%
    collapseIntervals(dimension = "age", old = c("90-94", "95-99", "100+"))
exposure_small <- exposure %>%
    slab(dimension = "country", elements = 1:3) %>%
    collapseIntervals(dimension = "age", old = c("90-94", "95-99", "100+"))
deaths_big <- deaths %>%
    slab(dimension = "country", elements = 4:n_country)
exposure_big <- exposure %>%
    slab(dimension = "country", elements = 4:n_country)

mx_small <- deaths_small / exposure_small
mx_big <- deaths_big / exposure_big
## 'LifeTable' cannot handle missing values, so have to
## handle by hand. Values are missing for entire years.
mx_is_missing_small <- is.na(mx_small)
mx_is_missing_big <- is.na(mx_big)
mx_small[mx_is_missing_small] <- 1
mx_big[mx_is_missing_big] <- 1
life_table_small <- LifeTable(mx_small)
life_table_big <- LifeTable(mx_big)
life_exp_small <- lifeTableFun(life_table_small, fun = "ex")
life_exp_big <- lifeTableFun(life_table_big, fun = "ex")
life_exp_small[mx_is_missing_small] <- NA
life_exp_big[mx_is_missing_big] <- NA
life_exp_big <- subarray(life_exp_big,
                         subarray = age <= 90)
life_exp <- dbind(life_exp_small, life_exp_big, along = "country")

saveRDS(life_exp,
        file = "out/life_exp.rds")
