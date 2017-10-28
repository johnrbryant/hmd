
## Load R packages. Note that 'Rscript' does not automatically load
## the 'methods' package, so we have to do it ourselves.

library(methods)
library(dembase)
library(latticeExtra)
library(gridExtra)
library(dplyr)

life_exp_obs <- readRDS("data/life_exp.rds")
life_exp_pred_1980 <- readRDS("data/life_exp_1980_25.rds")
life_exp_pred_1990 <- readRDS("data/life_exp_1990_25.rds")

countries <- dimnames(life_exp_obs)$country
ages <- c(0, 65)
years <- c(1980, 1990)
grobs <- vector(mode = "list", length = 4)

direct <- dbind(Individual = life_exp_obs,
                Pooled = life_exp_obs,
                along = "pooling")

graphics.off()
pdf(file = "out/fig_forecasts_heldback.pdf",
    paper = "a4")
for (COUNTRY in countries) {
    for (i.age in seq_along(ages)) {
        AGE <- ages[i.age]
        for (i.yr in seq_along(years)) {
            YEAR <- years[i.yr]
            main <- sprintf("%s, age %s, %s",
                            COUNTRY, AGE, YEAR)
            xlim <- c(1950, 2015)
            ylim <- if (AGE == "0") c(60, 110) else c(5, 30)
            overlay <- as.layer(dplot(~ year | pooling + sex,
                                      data = direct,
                                      midpoints = "year",
                                      subarray = (country == COUNTRY)  & (age == AGE),
                                      col = "black"))
            max_year_with_data <- life_exp_obs %>%
                subarray(country == COUNTRY) %>%
                collapseDimension(margin = "year", weights = 1) %>%
                (function(x) max(as.integer(dimnames(x)$year[!is.na(x)])))
            p <- dplot(~ year | pooling + sex, 
                       data = if (YEAR == 1980) life_exp_pred_1980 else life_exp_pred_1990,
                       subarray = (country == COUNTRY) & (age == AGE) & (year <= max_year_with_data),
                       main = main,
                       ylim = ylim,
                       xlim = xlim,
                       xlab = "",
                       ylab = "",
                       col = "light blue",
                       prob = c(0.1, 0.5, 0.9),
                       midpoints = "year",
                       scales = list(tck = 0.4),
                       par.settings = list(fontsize = list(text = 7)),
                       na.rm = TRUE)
            p <- p + overlay
            p <- useOuterStrips(p)
            i.grob <- (i.age - 1) * 2 + i.yr
            grobs[[i.grob]] <- p
        }
    }
    m <- do.call(arrangeGrob, args = c(grobs, list(nrow = 2, ncol = 2)))
    plot(m)
}
dev.off()
    
            
    


