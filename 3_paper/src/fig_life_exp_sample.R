
library(methods)
library(dembase)
library(dplyr)
library(latticeExtra)

life_exp <- readRDS("data/life_exp.rds")
palette <- readRDS("out/palette.rds")
sample_countries <- readRDS("out/sample_countries.rds")

data <- life_exp %>%
    subarray(country %in% sample_countries & age == "0" & sex == "Male") %>%
    as.data.frame(direction = "long", midpoints = "year")

p <- xyplot(value ~ year | country,
            data = data,
            xlab = "",
            ylab = "",
            layout = c(5, 1),
            scale = list(tck = 0.3),
            par.settings = list(fontsize = list(text = 8),
                                axis.line = list(col = "#808080"),
                                strip.background = list(col = palette$strip_col)),
            col = palette$blue,
            lwd = 1.5,
            type = "l")

pdf(file = "out/fig_life_exp_sample.pdf",
    height = 1.8,
    width = 4.9)
plot(p)
dev.off()

