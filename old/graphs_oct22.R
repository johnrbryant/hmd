
## Load R packages. Note that 'Rscript' does not automatically load
## the 'methods' package, so we have to do it ourselves.

library(methods)
library(demest)
library(demlife)
library(dplyr)
library(latticeExtra)


deaths <- readRDS("data/deaths.rds")
life_exp <- readRDS("data/life_exp.rds")
countries <- dimnames(deaths)$country

models <- c("indiv_nodamp", "pool_nodamp", "indiv_damp", "pool_damp")
yr_ests <- c(1980, 1990)

graphics.off()
pdf(file = "out/forecasts_combined.pdf",
    paper = "a4r",
    width = 0,
    height = 0)
for (country in countries) {
    for (sex in c("Female", "Male")) {
        for (age in c(0, 65)) {
            args.mod <- vector(mode = "list", length = length(models))
            for (i.mod in seq_along(models)) {
                model <- models[i.mod]
                args.yr <- vector(mode = "list", length = length(yr_ests))
                for (i.yr in seq_along(yr_ests)) {
                    yr_est <- yr_ests[i.yr]
                    if (grepl("indiv", model)) {
                        filename_est <- sprintf("out/%s_%s_25_%s.est", model, yr_est, country)
                        filename_pred <- sprintf("out/%s_%s_25_%s.pred", model, yr_est, country)
                    }
                    else {
                        filename_est <- sprintf("out/%s_%s_25.est", model, yr_est)
                        filename_pred <- sprintf("out/%s_%s_25.pred", model, yr_est)
                    }
                    rates <- fetchBoth(filenameEst = filename_est,
                                       filenamePred = filename_pred,
                                       where = c("model", "likelihood", "rate"))
                    if (grepl("pool", model))
                        rates <- slab(rates, dimension = "country", elements = country)
                    rates <- addDimension(rates,
                                          name = "yr_est",
                                          labels = yr_est)
                    args.yr[[i.yr]] <- rates
                }
                rates <- dbind(args = args.yr, along = "yr_est")
                rates <- addDimension(rates,
                                      name = "model",
                                      labels = model)
                args.mod[[i.mod]] <- rates
            }
            rates <- dbind(args = args.mod, along = "model")
            rates <- slab(rates, dimension = "sex", elements = sex)
            lt <- LifeTable(rates)
            le <- lifeExpectancy(lt, age = age)
            values <- slab(slab(slab(life_exp, dimension = "country", elements = country),
                                dimension = "sex", elements = sex),
                           dimension = "age", elements = as.character(age))
            main <- sprintf("Life expectancy at age %s - %ss, %s",
                            age, sex, country)
            print(main)
            ylim <- if (age == "0") c(60, 110) else c(10, 30)
            p <- dplot(~ year | yr_est + model,
                       data = le,
                       main = main,
                       overlay = list(values = values, col = "black"),
                       ylim = ylim,
                       col = "light blue",
                       midpoints = "year")
            p <- useOuterStrips(p)
            plot(p)
        }
    }
}
dev.off()
    
            
    
