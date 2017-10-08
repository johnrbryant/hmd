
library(docopt)
library(dplyr)
library(demlife)

## Get options from command line

'
Usage:
life_exp_comp.R [options]

Options:
--model [default: third]
--seed [default: 0]
--yr_est [default: 1985]
--len_pred [default: 25]
--keep [default: FALSE]
--n_burnin [default: 5]
--n_sim [default: 5]
--n_chain [default: 4]
--n_thin [default: 1]
--n_subset [default: 8]
' -> doc

opts <- docopt(doc)
model <- opts$model
seed <- opts$seed
yr_est <- as.integer(opts$yr_est)
len_pred <- opts$len_pred
keep <- opts$keep
n_burnin <- opts$n_burnin
n_sim <- opts$n_sim
n_chain <- opts$n_chain
n_thin <- opts$n_thin
n_subset <- opts$n_subset


## Set up outfile

out_file <- sprintf("out/%s.txt", model)
if (file.exists(out_file))
    file.remove(out_file)


## Construct forecasts for 'all' and for each country

deaths <- readRDS("data/deaths.rds")
countries <- dimnames(deaths)$country
if (n_subset < length(countries))
    countries <- countries[seq_len(n_subset)] ## only use a subset of countries
samples <- c("all", countries)
for (samp in samples) {
    head <- "Rscript src/forecast_rates.R"
    options1 <- sprintf('--model %s --samp "%s" --seed %s --yr_est %s --len_pred %s --keep %s', # samp needs quotes because of spaces
                        model, samp, seed, yr_est, len_pred, keep)
    options2 <- sprintf("--n_burnin %s --n_sim %s --n_chain %s --n_thin %s",
                        n_burnin, n_sim, n_chain, n_thin)
    tail <- sprintf(">> %s", out_file)
    command <- paste(head, options1, options2, tail)
    system(command)
}    
    

## Collect super-population mortality rates for forecast period

rates_pooled <- sprintf("out/%s_all_%s_%s_rates.rds",
                        model, yr_est, len_pred) %>%
    readRDS()

rates_indiv <- vector(mode = "list", length = length(countries))
for (i in seq_along(countries)) {
    rates_indiv[[i]] <- sprintf("out/%s_%s_%s_%s_rates.rds",
                                model, countries[i], yr_est, len_pred) %>%
        readRDS() %>%
        addDimension(name = "country", labels = countries[i])
}
rates_indiv <- dbind(args = rates_indiv, along = "country")

rates <- dbind(pooled = rates_pooled,
               indiv = rates_indiv,
               along = "pooling")


## ## Randomly generate finite-population rates

## expose_finite <- dbind(pooled = exposure,
##                        indiv = exposure,
##                        along = "pooling") %>%
##     subarray(year > yr_est)
## deaths_expected <- rates * expose_finite
## deaths_obs <- array(rpois(n = length(deaths_expected),
##                           lambda = deaths_expected),
##                     dim = dim(deaths_expected),
##                     dimnames = dimnames(deaths_expected))
## deaths_obs <- Counts(deaths_obs,
##                      dimscales = c(year = "Intervals"))
## rates_finite <- deaths_obs / expose_finite


## Create and save life expectancies

life_exp <- rates %>%
    LifeTable() %>%
    lifeExpectancy()

file <- sprintf("out/life_exp_comp_%s.rds", model)
saveRDS(life_exp, file = file)


