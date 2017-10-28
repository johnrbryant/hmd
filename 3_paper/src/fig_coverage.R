
library(methods)
library(dembase)
library(docopt)
library(dplyr)
library(latticeExtra)

'
Usage:
fig_coverage.R [options]

Options:
--age [default: 0]
' -> doc

opts <- docopt(doc)
AGE <- opts$age


life_exp_obs <- readRDS("data/life_exp.rds") %>%
    subarray(age == AGE)
life_exp_pred_1980 <- readRDS("data/life_exp_1980_25.rds") %>%
    subarray(age == AGE)
life_exp_pred_1990 <- readRDS("data/life_exp_1990_25.rds") %>%
    subarray(age == AGE)

palette <- readRDS("out/palette.rds")

lower80 <- collapseIterations(life_exp_pred_1980, prob = 0.1, na.rm = TRUE)
lower90 <- collapseIterations(life_exp_pred_1990, prob = 0.1, na.rm = TRUE)
upper80 <- collapseIterations(life_exp_pred_1980, prob = 0.9, na.rm = TRUE)
upper90 <- collapseIterations(life_exp_pred_1990, prob = 0.9, na.rm = TRUE)
cover80 <- ((lower80 <= life_exp_obs) & (life_exp_obs <= upper80)) %>%
    as.data.frame.table(responseName = "value") %>%
    mutate(launch = 1980)
cover90 <- ((lower90 <= life_exp_obs) & (life_exp_obs <= upper90)) %>%
    as.data.frame.table(responseName = "value") %>%
    mutate(launch = 1990)
cover <- rbind(cover80, cover90) %>%
    mutate(year = as.integer(levels(year))[year],
           elapsed = year - launch) %>%
    group_by(elapsed, pooling) %>%
    summarize(value = mean(value, na.rm = TRUE))

col <- c(palette$blue, palette$green)

graphics.off()
pdf(file = "out/fig_coverage.pdf",
    height = 2.3,
    width = 4.9)
p <- xyplot(value ~ elapsed,
            data = cover,
            groups = pooling,
            type = "l",
            col = col,
            lwd = 1.5,
            xlab = "Year into forecast",
            ylab = "",
            scales = list(tck = 0.3,
                          y = list(relation = "free")),
            prepanel = function(y) list(ylim = c(0, max(y, na.rm = TRUE))),
            par.settings = list(fontsize = list(text = 8),
                                axis.line = list(col = "#808080"),
                                strip.background = list(col = palette$strip_col)),
            key = list(text = list(levels(cover$pooling)),
                       lines = list(col = col, lwd = 1.5)))
plot(p)
dev.off()
    
