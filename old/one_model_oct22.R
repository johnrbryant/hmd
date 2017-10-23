
## Load R packages. Note that 'Rscript' does not automatically load
## the 'methods' package, so we have to do it ourselves.

library(methods)
library(demest)
library(docopt)
library(demlife)
library(dplyr)
library(latticeExtra)


## Read in options, using function 'docopt'

'
Usage:
one_model.R [options]

Options:
--model [default: pool_nodamp]
--seed [default: 0]
--yr_est [default: 2015]
--len_pred [default: 25]
--alpha [default: 0.2]
--n_burnin [default: 5]
--n_sim [default: 5]
--n_chain [default: 4]
--n_thin [default: 1]
' -> doc

opts <- docopt(doc)
model <- opts$model
seed <- as.integer(opts$seed)
yr_est <- as.integer(opts$yr_est)
len_pred <- as.integer(opts$len_pred)
alpha <- as.numeric(opts$alpha)
n_burnin <- as.integer(opts$n_burnin)
n_sim <- as.integer(opts$n_sim)
n_chain <- as.integer(opts$n_chain)
n_thin <- as.integer(opts$n_thin)


## Set random seed, using seed passed in via 'docopt'

set.seed(seed)


## Set up filenames, based on model, samp, yr_est, and len_pred

base <- sprintf("out/%s_%s_%s", model, yr_est, len_pred)
filename_life_exp <- sprintf("%s_life_exp.rds", base)
filename_forecasts <- sprintf("%s_forecasts.pdf", base)
filename_mse <- sprintf("%s_mse.pdf", base)
filename_score <- sprintf("%s_score.pdf", base)


## Subset deaths and exposure, to include required years

deaths <- readRDS("data/deaths.rds")
exposure <- readRDS("data/exposure.rds")
life_exp <- readRDS("data/life_exp.rds")
deaths_est <- deaths %>% subarray(year <= yr_est)
exposure_est <- exposure %>% subarray(year <= yr_est)
deaths_pred <- deaths %>% subarray(year > yr_est)
exposure_pred <- exposure %>% subarray(year > yr_est)


## model specifications

pool_nodamp <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
                                     age:sex + age:country + sex:country + age:year + sex:year + country:year +
                                     age:sex:country + age:country:year + sex:country:year),
                     age ~ DLM(damp = NULL,
                               error = Error(robust = TRUE)),
                     year ~ DLM(trend = Trend(scale = HalfT(scale = 0.1)),
                                damp = NULL),
                     age:sex ~ Zero(),
                     age:country ~ Zero(),
                     sex:country ~ Zero(),
                     age:year ~ Zero(),
                     sex:year ~ Zero(),
                     country:year ~ Zero(),
                     age:sex:country ~ DLM(trend = NULL,
                                           damp = NULL),               
                     age:country:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                            damp = NULL),
                     sex:country:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                            damp = NULL),
                     jump = 0.02)

indiv_nodamp <- Model(y ~ Poisson(mean ~ age + sex + year + 
                                      age:sex + age:year + sex:year),
                      age ~ DLM(damp = NULL,
                                error = Error(robust = TRUE)),
                      year ~ DLM(trend = Trend(scale = HalfT(scale = 0.1)),
                                 damp = NULL),
                      age:sex ~ DLM(trend = NULL,
                                    damp = NULL),               
                      age:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                     damp = NULL),
                      sex:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                     damp = NULL),
                      jump = 0.02)

pool_damp <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
                                   age:sex + age:country + sex:country + age:year + sex:year + country:year +
                                   age:sex:country + age:country:year + sex:country:year),
                   age ~ DLM(damp = NULL,
                             error = Error(robust = TRUE)),
                   year ~ DLM(trend = Trend(scale = HalfT(scale = 0.1)),
                              damp = NULL),
                   age:sex ~ Zero(),
                   age:country ~ Zero(),
                   sex:country ~ Zero(),
                   age:year ~ Zero(),
                   sex:year ~ Zero(),
                   country:year ~ Zero(),
                   age:sex:country ~ DLM(trend = NULL,
                                         damp = NULL),
                   age:country:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                          damp = Damp(min = 0.8, max = 1)),
                   sex:country:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                          damp = Damp(min = 0.8, max = 1)),
                   jump = 0.02)

indiv_damp <- Model(y ~ Poisson(mean ~ age + sex + year + 
                                    age:sex + age:year + sex:year),
                    age ~ DLM(damp = NULL,
                              error = Error(robust = TRUE)),
                    year ~ DLM(trend = Trend(scale = HalfT(scale = 0.1)),
                               damp = NULL),
                    age:sex ~ DLM(trend = NULL,
                                  damp = NULL),               
                    age:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                   damp = Damp(min = 0.8, max = 1)),
                    sex:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                   damp = Damp(min = 0.8, max = 1)),
                    jump = 0.02)

## Get the appropriate model

spec <- tryCatch(get(model),
                 error = function(e) e)
if (is(spec, "error"))
    stop(gettextf("invalid model name : %s",
                  spec$message))

is_indiv <- grepl("indiv", model)
countries <- dimnames(deaths)$country
n_countries <- length(countries)


## Estimation and prediction

if (is_indiv) {
    for (COUNTRY in countries) {
        filename_est <- sprintf("%s_%s.est", base, COUNTRY)
        filename_pred <- sprintf("%s_%s.pred", base, COUNTRY)
        outfile <- gsub(" ", "_", sprintf("%s_%s.log", base, COUNTRY))
        y <- subarray(deaths_est, country == COUNTRY)
        expose <- subarray(exposure_est, country == COUNTRY)
        estimateModel(spec,
                      y = y,
                      exposure = expose,
                      filename = filename_est,
                      nBurnin = n_burnin,
                      nSim = n_sim,
                      nChain = n_chain,
                      nThin = n_thin,
                      outfile = outfile)
        s <- fetchSummary(filename_est)
        print(s)
        predictModel(filenameEst = filename_est,
                     filenamePred = filename_pred,
                     n = len_pred)
    }
} else {
    filename_est <- sprintf("%s.est", base)
    filename_pred <- sprintf("%s.pred", base)
    outfile <- gsub(" ", "_", sprintf("%s.log", base))
    estimateModel(spec,
                  y = deaths_est,
                  exposure = exposure_est,
                  filename = filename_est,
                  nBurnin = n_burnin,
                  nSim = n_sim,
                  nChain = n_chain,
                  nThin = n_thin,
                  outfile = outfile)
    s <- fetchSummary(filename_est)
    print(s)
    predictModel(filenameEst = filename_est,
                 filenamePred = filename_pred,
                 n = len_pred)    
}


## Make predicted life expectancies - use age 90+ for 3 smallest countries and 100+ for rest


if (is_indiv) {
    rates_super_pred <- vector(mode = "list", length = n_countries)
    for (i in seq_len(n_countries)) {
        COUNTRY <- countries[i]
        filename_pred <- sprintf("%s_%s.pred", base, COUNTRY)
        val <- fetch(filename = filename_pred,
                     where = c("model", "likelihood", "rate"))
        val <- addDimension(val, name = "country", labels = COUNTRY)
        rates_super_pred[[i]] <- val
    }
    rates_super_pred <- dbind(args = rates_super_pred, along = "country")
} else {
    filename_pred <- sprintf("%s.pred", base)
    rates_super_pred <- fetch(filename = filename_pred,
                              where = c("model", "likelihood", "rate"))
}

expected_deaths_pred <- rates_super_pred * exposure_pred
deaths_pred <- suppressWarnings(rpois(n = length(expected_deaths_pred),
                                      lambda = expected_deaths_pred)) %>% # warns about NAs
    array(.,
          dim = dim(expected_deaths_pred),
          dimnames = dimnames(expected_deaths_pred)) %>%
    Counts(dimscales = c(year = "Intervals"))
deaths_pred_small <- deaths_pred %>%
    slab(dimension = "country", elements = 1:3) %>%
    collapseIntervals(dimension = "age", old = c("90-94", "95-99", "100+"))
deaths_pred_big <- deaths_pred %>%
    slab(dimension = "country", elements = 4:n_countries)
exposure_pred_small <- exposure_pred %>%
    slab(dimension = "country", elements = 1:3)
exposure_pred_big <- exposure_pred %>%
    slab(dimension = "country", elements = 4:n_countries)
rates_finite_pred_small <- (deaths_pred_small / exposure_pred_small)
rates_finite_pred_big <- (deaths_pred_big / exposure_pred_big)
is_missing_small <- is.na(rates_finite_pred_small) # missing for entire years; LifeTable can't handle
is_missing_big <- is.na(rates_finite_pred_big)
rates_finite_pred_small[is_missing_small] <- 999
rates_finite_pred_big[is_missing_big] <- 999
life_exp_pred_small <- rates_finite_pred_small %>%
    LifeTable() %>%
    lifeTableFun(fun = "ex")
life_exp_pred_big <- rates_finite_pred_big %>%
    LifeTable() %>%
    lifeTableFun(fun = "ex")
life_exp_pred_small[is_missing_small] <- NA
life_exp_pred_big[is_missing_big] <- NA
life_exp_pred_small <- life_exp_pred_small %>%
    subarray(age %in% c("0", "65"))
life_exp_pred_big <- life_exp_pred_big %>%
    subarray(age %in% c("0", "65"))
life_exp_pred <- dbind(life_exp_pred_small,
                       life_exp_pred_big,
                       along = "country")

saveRDS(life_exp_pred,
        file = filename_life_exp)


## Convergence

if (is_indiv) {
    for (i in seq_len(n_countries)) {
        COUNTRY <- countries[i]
        filename_est <- sprintf("%s_%s.est", base, COUNTRY)
        filename_converge <- sprintf("%s_%s_converge.pdf", base, COUNTRY)
        rate.mcmc <- fetchMCMC(filename_est,
                               where = c("model", "likelihood", "rate"))
        mean.mcmc <- fetchMCMC(filename_est,
                               where = c("model", "prior", "mean"))
        trend <- fetch(filename_est,
                       where = c("model", "hyper", "year", "trend")) +
            fetch(filename_est,
                  where = c("model", "hyper", "age:year", "trend")) + 
            fetch(filename_est,
                  where = c("model", "hyper", "sex:year", "trend"))
        trend.mcmc <- demest:::MCMCDemographic(trend, nChain = n_chain)
        graphics.off()
        pdf(filename_converge, paper = "a4r", width = 0, height = 0)
        plot(main = "Rates", x = 1, y = 1, axes = FALSE, type = "n", xlab = "", ylab = "")
        plot(rate.mcmc, smooth = FALSE, ask = FALSE)
        plot(main = "Mean", x = 1, y = 1, axes = FALSE, type = "n", xlab = "", ylab = "")
        plot(mean.mcmc, smooth = FALSE, ask = FALSE)
        plot(main = "Trend", x = 1, y = 1, axes = FALSE, type = "n", xlab = "", ylab = "")
        plot(trend.mcmc, smooth = FALSE, ask = FALSE)
        dev.off()
    }
} else {
    filename_est <- sprintf("%s.est", base)
    filename_converge <- sprintf("%s_converge.pdf", base)
    rate.mcmc <- fetchMCMC(filename_est,
                           where = c("model", "likelihood", "rate"))
    mean.mcmc <- fetchMCMC(filename_est,
                           where = c("model", "prior", "mean"))
    trend <- fetch(filename_est,
                   where = c("model", "hyper", "year", "trend")) +
        fetch(filename_est,
              where = c("model", "hyper", "age:country:year", "trend")) + 
        fetch(filename_est,
              where = c("model", "hyper", "sex:country:year", "trend"))
    trend.mcmc <- demest:::MCMCDemographic(trend, nChain = n_chain)
    graphics.off()
    pdf(filename_converge, paper = "a4r", width = 0, height = 0)
    plot(main = "Rates", x = 1, y = 1, axes = FALSE, type = "n", xlab = "", ylab = "")
    plot(rate.mcmc, smooth = FALSE, ask = FALSE)
    plot(main = "Mean", x = 1, y = 1, axes = FALSE, type = "n", xlab = "", ylab = "")
    plot(mean.mcmc, smooth = FALSE, ask = FALSE)
    plot(main = "Trend", x = 1, y = 1, axes = FALSE, type = "n", xlab = "", ylab = "")
    plot(trend.mcmc, smooth = FALSE, ask = FALSE)
    dev.off()
}


## Forecasts

graphics.off()
pdf(file = filename_forecasts,
    paper = "a4r",
    width = 0,
    height = 0)
for (SEX in c("Female", "Male")) {
    for (AGE in c("0", "65")) {
        data <- life_exp_pred %>%
            subarray(sex == SEX & age == AGE) %>%
            extrapolate(labels = seq(1950, yr_est), type = "missing")
        values <- life_exp %>%
            subarray(sex == SEX & age == AGE)
        p <- dplot(~ year | country,
                   data = data,
                   main = sprintf("Life expectancy at age %s - %ss (with %s percent credible intervals)",
                                  AGE, SEX, 100 * (1 - alpha)),
                   prob = c(alpha/2, 0.5, 1 - alpha/2),
                   lwd = 2,
                   na.rm = TRUE,
                   col = "salmon",
                   as.table = TRUE,
                   overlay = list(values = values,
                                  col = "black"))
        plot(p)
    }
}
dev.off()


## MSE

graphics.off()
pdf(file = filename_mse,
    paper = "a4r",
    width = 0,
    height = 0)
for (AGE in c("0", "65")) {
    values <- life_exp_pred %>%
        subarray(age == AGE) %>%
        collapseIterations(FUN = median)
    truth <- life_exp %>%
        subarray(age == AGE & year > yr_est)
    mse <- (values - truth)^2
    p <- dplot(~ year | country,
               data = mse,
               weights = 1,
               main = sprintf("MSE for life expectancy at age %s",
                              AGE),
               na.rm = TRUE,
               midpoints = "year",
               as.table = TRUE,
               col = "black")
    plot(p)
}
dev.off()


## Interval Score

graphics.off()
pdf(file = filename_score,
    paper = "a4r",
    width = 0,
    height = 0)
for (AGE in c("0", "65")) {
    values <- life_exp_pred %>%
        subarray(age == AGE) %>%
        collapseIterations(prob = c(alpha/2, 1-alpha/2), na.rm = TRUE)
    truth <- life_exp %>%
        subarray(age == AGE & year > yr_est)
    lower <- slab(values, dimension = "quantile", elements = 1L)
    upper <- slab(values, dimension = "quantile", elements = 2L)
    width <- upper - lower
    penalty.below.lower <- (2 / alpha) * (lower - truth) * (truth < lower)
    penalty.above.upper <- (2 / alpha) * (truth - upper) * (truth > upper)
    score <- width + penalty.below.lower + penalty.above.upper
    p <- dplot(~ year | country,
               data = score,
               weights = 1,
               main = sprintf("Interval score for life expectancy at age %s",
                              AGE),
               na.rm = TRUE,
               midpoints = "year",
               as.table = TRUE,
               col = "black")
    plot(p)
}
dev.off()
