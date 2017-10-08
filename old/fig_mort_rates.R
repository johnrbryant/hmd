
library(methods)
library(demest)
library(docopt)
library(dplyr)
library(latticeExtra)

'
Usage:
fig_mort_rates.R [options]

Options:
--first_year [default: 1960]
--last_year [default: 2010]
' -> doc
opts <- docopt(doc)
first_year <- opts$first_year
last_year <- opts$last_year

deaths <- readRDS("out/deaths.rds") %>%
    subarray(year %in% c(first_year, last_year))

exposure <- readRDS("out/exposure.rds") %>%
    subarray(year %in% c(first_year, last_year))

rates <- deaths/exposure

series_names <- dimnames(rates)$series

p <- dplot(~ age | series * sex,
           data = rates,
           groups = year,
           subarray = series %in% series_names[1:20],
           midpoints = "age",
           xlab = "Age",
           ylab = "",
           scale = list(y = list(log = TRUE)),
           yscale.components = yscale.components.log10ticks,
           as.table = TRUE)
p <- useOuterStrips(p)


p <- p + as.layer(dplot(~ age | series,
                        data = rates,
                        groups = year,
                        subarray = sex == "Male",
                        midpoints = "age",
                        scale = list(y = list(log = TRUE))),
                  under = TRUE)



p <- p + layer(panel.abline(h = 0.2, col = "blue"), under = TRUE)
p <- p + layer(panel.abline(v = 2008, col = "blue"), under = TRUE)

file <- sprintf("out/fig_diff_life_exp_%s.pdf", tolower(SEX))
pdf(file = file,
    paper = "a4r",
    height = 0,
    width = 0)
plot(p)
dev.off()
