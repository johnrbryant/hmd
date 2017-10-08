
library(dembase)
library(dplyr)
library(latticeExtra)

first.year <- 1960

breaks <- c(0, 1, seq(5, 100, 5))
deaths <- readRDS("out/deaths.rds") %>%
    subarray(year > first.year - 1) %>%
    subarray(series == "USA") %>%
    collapseIntervals(dimension = "age", breaks = breaks) %>%
    setAgeMax(Inf)

exposure <- readRDS("out/exposure.rds") %>%
    subarray(year > first.year - 1) %>%
    subarray(series == "USA") %>%
    collapseIntervals(dimension = "age", breaks = breaks) %>%
    setAgeMax(Inf)

rate <- deaths/exposure

col <- rainbow(n = length(dimnames(rate)$age))

p <- dplot(~ year | sex,
           data = rate,
           groups = age,
           col = col,
           midpoints = "year",
           xlab = "Year",
           ylab = "",
           scale = list(y = list(log = TRUE)),
           yscale.components=yscale.components.log10ticks,
           key = list(text = dimnames(rate)["age"],
                      lines = list(col = col),
                      space = "top",
                      columns = 6))

pdf(file = "out/fig_usa_rates.pdf",
    height = 5,
    width = 9)
plot(p)
dev.off()
    
