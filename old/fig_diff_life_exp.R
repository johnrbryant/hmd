
library(methods)
library(demest)
library(docopt)
library(dplyr)
library(latticeExtra)

'
Usage:
fig_incr_life_exp.R [options]

Options:
--first_year [default: 1990]
--sex [default: Female]
' -> doc
opts <- docopt(doc)
first_year <- as.integer(opts$first_year)
SEX <- opts$sex

diff_life_exp <- readRDS("out/diff_life_exp.rds") %>%
    subarray(year > first_year) %>%
    subarray(sex == SEX)

p <- dplot(~ year | series,
           data = diff_life_exp,
           midpoints = "year",
           xlab = "Year",
           ylab = "",
           col = "black",
           as.table = TRUE,
           ylim = c(-1, 1),
           main = SEX)
p <- p + layer(panel.abline(h = 0.2, col = "blue"), under = TRUE)
p <- p + layer(panel.abline(v = 2008, col = "blue"), under = TRUE)

file <- sprintf("out/fig_diff_life_exp_%s.pdf", tolower(SEX))
pdf(file = file,
    paper = "a4r",
    height = 0,
    width = 0)
plot(p)
dev.off()
