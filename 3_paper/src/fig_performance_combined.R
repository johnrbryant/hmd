
library(methods)
library(dembase)
library(docopt)
library(dplyr)
library(latticeExtra)

'
Usage:
fig_performance_combined.R [options]

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

err_sq_80 <- ((collapseIterations(life_exp_pred_1980, median) - life_exp_obs)^2) %>%
    as.data.frame(direction = "long") %>%
    mutate(launch = 1980)
err_sq_90 <- ((collapseIterations(life_exp_pred_1990, median) - life_exp_obs)^2) %>%
    as.data.frame(direction = "long") %>%
    mutate(launch = 1990)
score80 <- life_exp_pred_1980 %>%
    intervalScore(truth = life_exp_obs, alpha = 0.2, na.rm = TRUE) %>%
    as.data.frame(direction = "long") %>%
    mutate(launch = 1980)
score90 <- life_exp_pred_1990 %>%
    intervalScore(truth = life_exp_obs, alpha = 0.2, na.rm = TRUE) %>%
    as.data.frame(direction = "long") %>%
    mutate(launch = 1990)
rmse <- rbind(err_sq_80, err_sq_90) %>%
    mutate(year = as.integer(levels(year))[year],
           elapsed = year - launch) %>%
    group_by(elapsed, pooling) %>%
    summarize(value = sqrt(mean(value, na.rm = TRUE))) %>%
    mutate(measure = "Root mean squared error")
score <- rbind(score80, score90) %>%
    mutate(year = as.integer(levels(year))[year],
           elapsed = year - launch) %>%
    group_by(elapsed, pooling) %>%
    summarize(value = mean(value, na.rm = TRUE)) %>%
    mutate(measure = "Interval score (means)")
performance <- rbind(rmse, score)

col <- c(palette$blue, palette$green)

graphics.off()
pdf(file = "out/fig_performance_combined.pdf",
    height = 2.3,
    width = 4.9)
p <- xyplot(value ~ elapsed | measure,
            data = performance,
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
            key = list(text = list(levels(score$pooling)),
                       lines = list(col = col, lwd = 1.5)))
plot(p)
dev.off()
    
