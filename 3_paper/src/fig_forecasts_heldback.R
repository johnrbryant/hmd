
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

life_exp_pred_1980[is.infinite(life_exp_pred_1980) | is.na(life_exp_pred_1980)] <- 80 ## TEMPORARY HACK!!!!
life_exp_pred_1990[is.infinite(life_exp_pred_1990) | is.na(life_exp_pred_1990)] <- 80 ## TEMPORARY HACK!!!!

countries <- dimnames(life_exp_obs)$country
sexes <- c("Female", "Male")
ages <- c(0, 65)
years <- c(1980, 1990)
grobs <- vector(mode = "list", length = 8)

graphics.off()
pdf(file = "out/fig_forecasts_heldback.pdf",
    paper = "a4")
for (COUNTRY in countries) {
    for (i.sex in seq_along(sexes)) {
        SEX <- sexes[i.sex]
        for (i.age in seq_along(ages)) {
            AGE <- ages[i.age]
            for (i.yr in seq_along(years)) {
                YEAR <- years[i.yr]
                main <- sprintf("%s, age %s, %ss, %s - HACK!!",
                                COUNTRY, AGE, SEX, YEAR)
                xlim <- c(1950, 2015)
                ylim <- if (AGE == "0") c(60, 110) else c(5, 30)
                overlay <- as.layer(dplot(~ year,
                                          data = life_exp_obs,
                                          midpoints = "year",
                                          subarray = (country == COUNTRY) & (sex == SEX) & (age == AGE),
                                          col = "black"))
                p <- dplot(~ year | pooling + damping, 
                           data = if (YEAR == 1980) life_exp_pred_1980 else life_exp_pred_1990,
                           subarray = (country == COUNTRY) & (sex == SEX) & (age == AGE),
                           main = main,
                           ylim = ylim,
                           xlim = xlim,
                           xlab = "",
                           ylab = "",
                           col = "light blue",
                           scales = list(tck = 0.4),
                           midpoints = "year",
                           par.settings = list(fontsize = list(text = 7)),
                           na.rm = TRUE)
                p <- p + overlay
                p <- useOuterStrips(p)
                i.grob <- (i.sex - 1) * 4 + (i.age - 1) * 2 + i.yr
                grobs[[i.grob]] <- p
            }
        }
    }
    m <- do.call(arrangeGrob, args = c(grobs, list(nrow = 4, ncol = 2)))
    plot(m)
}
dev.off()
    
            
    
