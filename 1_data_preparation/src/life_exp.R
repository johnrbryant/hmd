
library(methods)
library(dembase)
library(demlife)
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

deaths <- readRDS("out/deaths.rds")
exposure <- readRDS("out/exposure.rds")

mx <- deaths / exposure
mx_is_missing <- is.na(mx)
mx[mx_is_missing] <- 1
life_table <- LifeTable(mx)
life_exp <- lifeTableFun(life_table, fun = "ex")
life_exp[mx_is_missing] <- NA
    

saveRDS(life_exp,
        file = "out/life_exp.rds")
