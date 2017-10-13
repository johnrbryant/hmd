
library(docopt)
library(dplyr)
library(dembase)
library(latticeExtra)

## Get name of model being compared

'
Usage:
fig_comp.R [options]

Options:
--model [default: third]
--yr_est [default: 1985]
' -> doc

opts <- docopt(doc)
model <- opts$model
yr_est <- as.integer(opts$yr_est)


life_exp_mod <- readRDS(sprintf("out/life_exp_comp_%s.rds", model))
life_exp_obs <- readRDS("data/life_exp.rds") %>%
    subarray(age == "0")

life_exp_mod_mse <- life_exp_mod %>%
    subarray(year > yr_est) %>%
    collapseIterations(FUN = median)

life_exp_obs_mse <- life_exp_obs %>%
    subarray(year > yr_est)

mse <- (life_exp_mod_mse - life_exp_obs_mse)^2

life_exp_mod_50 <- life_exp_mod %>%
    subarray(year > yr_est) %>%
    collapseIterations(prob = c(0.25, 0.75))
life_exp_mod_95 <- life_exp_mod %>%
    subarray(year > yr_est) %>%
    collapseIterations(prob = c(0.025, 0.975))
upper_50 <- slab(life_exp_mod_50,
                 dimension = "quantile",
                 elements = "75%")
upper_95 <- slab(life_exp_mod_95,
                 dimension = "quantile",
                 elements = "97.5%")
lower_50 <- slab(life_exp_mod_50,
                 dimension = "quantile",
                 elements = "25%")
lower_95 <- slab(life_exp_mod_95,
                 dimension = "quantile",
                 elements = "2.5%")
widths_50 <- collapseDimension(upper_50 - lower_50,
                               dimension = "country",
                               weights = 1)
widths_95 <- collapseDimension(upper_95 - lower_95,
                               dimension = "country",
                               weights = 1)
in_50 <- (1 * ((lower_50 <= life_exp_obs) & (life_exp_obs <= upper_50))) %>%
    Values(dimscales = c(year = "Intervals"))
in_95 <- (1 * ((lower_95 <= life_exp_obs) & (life_exp_obs <= upper_95))) %>%
    Values(dimscales = c(year = "Intervals"))
cover_50 <- collapseDimension(in_50,
                              dimension = "country",
                              weights = 1)
cover_95 <- collapseDimension(in_95,
                              dimension = "country",
                              weights = 1)

countries <- dimnames(life_exp_mod)$country
sexes <- dimnames(life_exp_obs)$sex

graphics.off()
pdf(file = sprintf("out/fig_comp_%s.pdf", model),
    paper = "a4")
for (COUNTRY in countries) {
    p <- dplot(~ year | sex * pooling,
               data = life_exp_mod,
               subarray = country == COUNTRY,
               midpoints = "year",
               ylim = c(50, 100),
               main = sprintf("Estimated, forecast, and observed life expectancy - %s", COUNTRY),
               overlay = list(values = subarray(life_exp_obs,
                                                subarray = country == COUNTRY),
                              col = "black"))
    p <- useOuterStrips(p)
    plot(p)
}
col <- "black"
lty <- c("dashed", "solid")
for (SEX in sexes) {
    p <- dplot(~ year | country,
               data = mse,
               subarray = sex == SEX,
               groups = pooling,
               midpoints = "year",
               main = sprintf("MSE - %ss", SEX),
               par.settings = list(fontsize = list(text = 8)),
               col = col,
               lty = lty,
               key = list(text = dimnames(life_exp_mod_mse)["pooling"],
                          lines = list(col = col, lty = lty)))
    plot(p)
}
p <- dplot(~ year | sex,
           data = widths_50,
           groups = pooling,
           midpoints = "year",
           main = "Mean width of 50 percent credible interval",
           par.settings = list(fontsize = list(text = 8)),
           col = col,
           lty = lty,
           key = list(text = dimnames(life_exp_mod_mse)["pooling"],
                      lines = list(col = col, lty = lty)))
plot(p)
p <- dplot(~ year | sex,
           data = widths_95,
           groups = pooling,
           midpoints = "year",
           main = "Mean width of 95 percent credible interval",
           par.settings = list(fontsize = list(text = 8)),
           col = col,
           lty = lty,
           key = list(text = dimnames(life_exp_mod_mse)["pooling"],
                      lines = list(col = col, lty = lty)))
plot(p)
p <- dplot(~ year | sex,
           data = cover_50,
           groups = pooling,
           midpoints = "year",
           main = "Coverage of 50 percent credible interval",
           par.settings = list(fontsize = list(text = 8)),
           col = col,
           lty = lty,
           key = list(text = dimnames(life_exp_mod_mse)["pooling"],
                      lines = list(col = col, lty = lty)))
plot(p)
p <- dplot(~ year | sex,
           data = cover_95,
           groups = pooling,
           midpoints = "year",
           main = "Coverage of 95 percent credible interval",
           par.settings = list(fontsize = list(text = 8)),
           col = col,
           lty = lty,
           key = list(text = dimnames(life_exp_mod_mse)["pooling"],
                      lines = list(col = col, lty = lty)))
plot(p)
dev.off()

