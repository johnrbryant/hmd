
library(methods)
library(dembase)
library(latticeExtra)

deaths <- readRDS("out/deaths.rds")
exposure <- readRDS("out/exposure.rds")
palette <- readRDS("out/palette.rds")
sample_countries <- readRDS("out/sample_countries.rds")

rates <- deaths / exposure

sample_ages <- c(0, paste(seq(10, 85, 15), seq(14, 89, 15), sep = "-"), "100+")

col <- with(palette,
            c(rep(red, 3),
              rep(c(rep(light_blue, 4), rep(green, 4)), 3)))
col <- col[seq_along(sample_ages)]
col <- with(palette,
            c(red, rep(light_blue, times = length(sample_ages) - 1)))

p <- dplot(~ year | country,
           data = rates,
           subarray = country %in% sample_countries & age %in% sample_ages & sex == "Male",
           groups = age,
           midpoints = "year",
           xlab = "",
           ylab = "",
           layout = c(5, 1),
           scale = list(x = list(tck = 0.3),
                        y = list(log = TRUE)),
           par.settings = list(fontsize = list(text = 8),
                               axis.line = list(col = "#808080"),
                               strip.background = list(col = palette$strip_col)),
           yscale.components = yscale.components.log10ticks,
           col = col,
           lwd = 1.5,
           ylim = c(0.00004, 1.05))

pdf(file = "out/fig_mort_age_sample.pdf",
    height = 2.2,
    width = 5.2)
plot(p)
dev.off()


