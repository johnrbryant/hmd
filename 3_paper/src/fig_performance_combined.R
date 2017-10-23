
## Load R packages. Note that 'Rscript' does not automatically load
## the 'methods' package, so we have to do it ourselves.

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
    mutate(model = paste(pooling, damping),
           model = factor(model,
                          levels = c("Individual Not damped",
                                     "Pooled Not damped",
                                     "Individual Damped",
                                     "Pooled Damped"),
                          labels = c("Individual, not damped",
                                     "Pooled, not damped",
                                     "Individual, damped",
                                     "Pooled, damped")),
           year = as.integer(levels(year))[year],
           elapsed = year - launch) %>%
    group_by(model, elapsed) %>%
    summarize(value = sqrt(mean(value, na.rm = TRUE))) %>%
    mutate(measure = "Root mean squared error")
score <- rbind(score80, score90) %>%
    mutate(model = paste(pooling, damping),
           model = factor(model,
                          levels = c("Individual Not damped",
                                     "Pooled Not damped",
                                     "Individual Damped",
                                     "Pooled Damped"),
                          labels = c("Individual, not damped",
                                     "Pooled, not damped",
                                     "Individual, damped",
                                     "Pooled, damped")),
           year = as.integer(levels(year))[year],
           elapsed = year - launch) %>%
    group_by(model, elapsed) %>%
    summarize(value = mean(value, na.rm = TRUE)) %>%
    mutate(measure = "Interval score (means)")
performance <- rbind(rmse, score)


col <- rep(c(palette$blue, palette$green), each = 2)
lty <- rep(c("dashed", "solid"), times = 2)

graphics.off()
pdf(file = "out/fig_performance_combined.pdf",
    height = 2.3,
    width = 4.9)
p <- xyplot(value ~ elapsed | measure,
            data = performance,
            type = "l",
            groups = model,
            col = col,
            lty = lty,
            lwd = 1.5,
            xlab = "Year into forecast",
            ylab = "",
            scales = list(tck = 0.3,
                          y = list(relation = "free")),
            prepanel = function(y) list(ylim = c(0, max(y, na.rm = TRUE))),
            par.settings = list(fontsize = list(text = 8),
                                axis.line = list(col = "#808080"),
                                strip.background = list(col = palette$strip_col)),
            key = list(text = list(levels(score$model)),
                       lines = list(col = col, lty = lty, lwd = 1.5),
                       columns = 2))
plot(p)
dev.off()

       



    
            
    
