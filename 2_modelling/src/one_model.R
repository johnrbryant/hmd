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
--model [default: pool]
--seed [default: 0]
--yr_est [default: 2015]
--len_pred [default: 20]
--alpha [default: 0.2]
--n_burnin [default: 10]
--n_sim [default: 10]
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
using_heldback_data <- max(as.integer(dimnames(deaths)$year)) >= (yr_est + len_pred)
if (using_heldback_data) {
    deaths_pred <- deaths %>% subarray(year > yr_est)
    exposure_pred <- exposure %>% subarray(year > yr_est)
}

countries <- dimnames(deaths)$country
n_countries <- length(countries)

## model specifications

DLM_main_effect <- DLM(level = Level(scale = HalfT(scale = 0.02)),
                       trend = Trend(scale = HalfT(scale = 0.02)),
                       error = Error(scale = HalfT(scale = 0.02)),
                       damp = NULL)
DLM_interaction <- DLM(level = Level(scale = HalfT(scale = 0.01)),
                       trend = Trend(scale = HalfT(scale = 0.01)),
                       error = Error(scale = HalfT(scale = 0.01)),
                       damp = Damp(min = 0.8, max = 1))


## Estimation and prediction

if (model == "pool") {
    spec <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
                                  age:sex + age:country + sex:country + age:year + sex:year + country:year +
                                  age:sex:country + age:country:year + sex:country:year),
                  age ~ DLM(damp = NULL,
                            covariates = Covariates(infant = TRUE),
                            error = Error(robust = TRUE)),
                  year ~ DLM_main_effect,
                  age:sex ~ Zero(),
                  age:country ~ Zero(),
                  sex:country ~ Zero(),
                  age:year ~ Zero(),
                  sex:year ~ Zero(),
                  country:year ~ Zero(),
                  age:sex:country ~ DLM_interaction,
                  age:country:year ~ DLM_interaction,
                  sex:country:year ~ DLM_interaction,
                  lower = 0.0001,
                  jump = 0.02)
    filename_est <- sprintf("%s.est", base)
    filename_pred <- sprintf("%s.pred", base)
    set.seed(seed)
    estimateModel(spec,
                  y = deaths_est,
                  exposure = exposure_est,
                  filename = filename_est,
                  nBurnin = n_burnin,
                  nSim = n_sim,
                  nChain = n_chain,
                  nThin = n_thin)
    s <- fetchSummary(filename_est)
    print(s)
    predictModel(filenameEst = filename_est,
                 filenamePred = filename_pred,
                 n = len_pred)    
} else {
    for (COUNTRY in countries) {
        jump <- switch(COUNTRY,
                       "Luxembourg" = 0.01,
                       "USA" = 0.05,
                       0.02)
        spec <- Model(y ~ Poisson(mean ~ age + sex + year + 
                                      age:sex + age:year + sex:year),
                      age ~ DLM(damp = NULL,
                                covariates = Covariates(infant = TRUE),
                                error = Error(robust = TRUE)),
                      year ~ DLM(trend = Trend(scale = HalfT(scale = 0.1)),
                                 damp = NULL),
                      age:sex ~ DLM(trend = NULL,
                                    damp = NULL),               
                      age:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                     damp = Damp(min = 0.8, max = 1)),
                      sex:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                     damp = Damp(min = 0.8, max = 1)),
                      lower = 0.0001,
                      jump = jump)
        filename_est <- sprintf("%s_%s.est", base, COUNTRY)
        filename_pred <- sprintf("%s_%s.pred", base, COUNTRY)
        y_country <- subarray(deaths_est, country == COUNTRY)
        exposure_country <- subarray(exposure_est, country == COUNTRY)
        set.seed(seed)
        estimateModel(spec,
                      y = y_country,
                      exposure = exposure_country,
                      filename = filename_est,
                      nBurnin = n_burnin,
                      nSim = n_sim,
                      nChain = n_chain,
                      nThin = n_thin)
        s <- fetchSummary(filename_est)
        print(s)
        predictModel(filenameEst = filename_est,
                     filenamePred = filename_pred,
                     n = len_pred)
    }
}

## Make predicted life expectancies
## Use age 90+ for 3 smallest countries and 100+ for rest
## If using held-back data, calculate finite-population
## quantities; otherwise calculate super-population quantities.

if (model == "pool") {
    filename_pred <- sprintf("%s.pred", base)
    rates_super_pred <- fetch(filename = filename_pred,
                              where = c("model", "likelihood", "rate"))
} else {
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
}

if (using_heldback_data) {
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
    rates_finite_pred_small[is_missing_small] <- 0.1
    rates_finite_pred_big[is_missing_big] <- 0.1
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
} else {
    life_exp_pred <- rates_super_pred %>%
        LifeTable() %>%
        lifeTableFun(fun = "ex") %>%
        subarray(age %in% c("0", "65"))
}

saveRDS(life_exp_pred,
        file = filename_life_exp)


## Convergence

if (model == "pool") {
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
} else {
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
}

