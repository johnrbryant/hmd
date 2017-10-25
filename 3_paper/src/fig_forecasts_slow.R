
## Load R packages. Note that 'Rscript' does not automatically load
## the 'methods' package, so we have to do it ourselves.

library(methods)
library(dembase)
library(latticeExtra)
library(dplyr)

life_exp_pred_2010 <- readRDS("data/pool_damp_2010_25_life_exp.rds") %>%
    subarray(year == "2035" & age == "0")
life_exp_pred_2015 <- readRDS("data/pool_damp_2015_20_life_exp.rds") %>%
    subarray(year == "2035" & age == "0")

palette <- readRDS("out/palette.rds")

life_exp_2035 <- dbind("2010" = life_exp_pred_2010,
                       "2015" = life_exp_pred_2015,
                       along = "launch") %>%
    collapseIterations(prob = c(0.1, 0.5, 0.9)) %>%
    as.data.frame(direction = "long")

order <- (collapseIterations(life_exp_pred_2010, median)
    - collapseIterations(life_exp_pred_2015, median)) %>%
    collapseDimension(dimension = "sex", weights = 1) %>%
    sort()

life_exp_2035 <- life_exp_2035 %>%
    mutate(country = factor(country, levels = dimnames(order)$country)) %>%
    arrange(country)

n_countries <- length(dimnames(life_exp_pred_2010)$country)
eps <- 0.15
p <- xyplot(country ~ value | sex,
            data = life_exp_2035,
            layout = c(2, 1),
            panel = function(x, y, subscripts, ...) {
                is_low <- life_exp_2035$quantile[subscripts] == "10%"
                is_med <- life_exp_2035$quantile[subscripts] == "50%"
                is_high <- life_exp_2035$quantile[subscripts] == "90%"
                is_2010 <- life_exp_2035$launch[subscripts] == "2010"
                is_2015 <- life_exp_2035$launch[subscripts] == "2015"
                panel.segments(x0 = x[is_low & is_2010],
                               x1 = x[is_high & is_2010],
                               y0 = seq_len(n_countries) + eps,
                               y1 = seq_len(n_countries) + eps,
                               col = palette$green)
                panel.segments(x0 = x[is_low & is_2015],
                               x1 = x[is_high & is_2015],
                               y0 = seq_len(n_countries) - eps,
                               y1 = seq_len(n_countries) - eps,
                               col = palette$blue)
                panel.xyplot(x = x[is_med & is_2010],
                             y = seq_len(n_countries) + eps,
                             col = palette$green,
                             pch = 19,
                             cex = 0.7)
                panel.xyplot(x = x[is_med & is_2015],
                             y = seq_len(n_countries) - eps,
                             col = palette$blue,
                             pch = 19,
                             cex = 0.7)
            },
            scales = list(x = list(relation = "free")),
            xlab = "",
            ylab = "",
            between = list(x = 0.5),
            par.settings = list(fontsize = list(text = 10),
                                axis.line = list(col = "#808080"),
                                strip.background = list(col = palette$strip_col)))
p
             

file <- sprintf("out/mortality/mortality_lx_change_%s.pdf",
                SEX)
pdf(file = file,
    width = 6,
    height = 1.7)
plot(p)
dev.off()
p
   
            
    
