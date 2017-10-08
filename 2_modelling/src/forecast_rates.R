
## Load R packages. Note that 'Rscript' does not automatically load
## the 'methods' package, so we have to do it ourselves.

library(methods)
library(demest)
library(docopt)


## Read in options, using function 'docopt'

'
Usage:
forecast_rates.R [options]

Options:
--model [default: third]
--samp [default: all]
--seed [default: 0]
--yr_est [default: 2015]
--len_pred [default: 25]
--keep [default: FALSE]
--n_burnin [default: 5]
--n_sim [default: 5]
--n_chain [default: 4]
--n_thin [default: 1]
' -> doc

opts <- docopt(doc)
model <- opts$model
samp <- opts$samp
seed <- as.integer(opts$seed)
yr_est <- as.integer(opts$yr_est)
len_pred <- as.integer(opts$len_pred)
keep <- as.logical(opts$keep)
n_burnin <- as.integer(opts$n_burnin)
n_sim <- as.integer(opts$n_sim)
n_chain <- as.integer(opts$n_chain)
n_thin <- as.integer(opts$n_thin)


## Set random seed, using seed passed in via 'docopt'

set.seed(seed)


## Set up filenames, based on model, samp, yr_est, and len_pred

base <- sprintf("out/%s_%s_%s_%s", model, samp, yr_est, len_pred)
filename_est <- sprintf("%s.est", base)
filename_pred <- sprintf("%s.pred", base)
outfile <- sub(" ", "_", sprintf("%s.log", base))
filename_rates <- sprintf("%s_rates.rds", base)


## Subset deaths and exposure, to include required years and, if necessary, samp

deaths <- readRDS("data/deaths.rds")
exposure <- readRDS("data/exposure.rds")
deaths <- subarray(deaths,
                   subarray = year <= yr_est)
exposure <- subarray(exposure,
                     subarray = year <= yr_est)
if (samp != "all") {
    deaths <- subarray(deaths,
                       subarray = country == samp)
    exposure <- subarray(exposure,
                         subarray = country == samp)
}


## 'third' model: all third-order effects

third_pool <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
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
                   jump = 0.07)

third_indiv <- Model(y ~ Poisson(mean ~ age + sex + year + 
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
                    jump = 0.07)


## 'fixsex' model: sex effects are constant over time

fixsex_pool <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
                                     age:sex + age:country + sex:country + age:year + sex:year + country:year +
                                     age:sex:country + age:country:year),
                     age ~ DLM(damp = NULL,
                               error = Error(robust = TRUE)),
                     year ~ DLM(trend = Trend(scale = HalfT(scale = 0.1)),
                                damp = NULL),
                     age:sex ~ Zero(),
                     age:country ~ Zero(),
                     sex:country ~ Exch(),
                     age:year ~ Zero(),
                     sex:year ~ Zero(),
                     country:year ~ Zero(),
                     age:sex:country ~ DLM(trend = NULL,
                                           damp = NULL),               
                     age:country:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                            damp = NULL),
                     jump = 0.07)

fixsex_indiv <- Model(y ~ Poisson(mean ~ age + sex + year + 
                                      age:sex + age:year),
                      age ~ DLM(damp = NULL,
                                error = Error(robust = TRUE)),
                      year ~ DLM(trend = Trend(scale = HalfT(scale = 0.1)),
                                 damp = NULL),
                      age:sex ~ DLM(trend = NULL,
                                    damp = NULL),
                      age:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                     damp = NULL),
                      jump = 0.07)


## 'fixagesex' model: age and sex effects are constant over time

fixagesex_pool <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
                                        age:sex + age:country + sex:country +
                                        age:sex:country),
                        age ~ DLM(damp = NULL,
                                  error = Error(robust = TRUE)),
                        year ~ DLM(trend = Trend(scale = HalfT(scale = 0.1)),
                                   damp = NULL),
                        age:sex ~ Zero(),
                        age:country ~ Zero(),
                        sex:country ~ Zero(),
                        age:sex:country ~ DLM(trend = NULL,
                                              damp = NULL),               
                        jump = 0.07)

fixagesex_indiv <- Model(y ~ Poisson(mean ~ age + sex + year + 
                                         age:sex),
                         age ~ DLM(damp = NULL,
                                   error = Error(robust = TRUE)),
                         year ~ DLM(trend = Trend(scale = HalfT(scale = 0.1)),
                                    damp = NULL),
                         age:sex ~ DLM(trend = NULL,
                                       damp = NULL),
                         jump = 0.07)


## 'inform': the same as 'third', but with more informative priors
## on variances for trend terms

inform_pool <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
                                   age:sex + age:country + sex:country + age:year + sex:year + country:year +
                                   age:sex:country + age:country:year + sex:country:year),
                   age ~ DLM(damp = NULL,
                             error = Error(robust = TRUE)),
                   year ~ DLM(trend = Trend(scale = HalfT(scale = 0.02)),
                              damp = NULL),
                   age:sex ~ Zero(),
                   age:country ~ Zero(),
                   sex:country ~ Zero(),
                   age:year ~ Zero(),
                   sex:year ~ Zero(),
                   country:year ~ Zero(),
                   age:sex:country ~ DLM(trend = NULL,
                                         damp = NULL),               
                   age:country:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.01)),
                                          damp = NULL),
                   sex:country:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.01)),
                                          damp = NULL),
                   jump = 0.07)

inform_indiv <- Model(y ~ Poisson(mean ~ age + sex + year + 
                                    age:sex + age:year + sex:year),
                    age ~ DLM(damp = NULL,
                              error = Error(robust = TRUE)),
                    year ~ DLM(trend = Trend(scale = HalfT(scale = 0.02)),
                               damp = NULL),
                    age:sex ~ DLM(trend = NULL,
                                  damp = NULL),               
                    age:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.01)),
                                   damp = NULL),
                    sex:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.01)),
                                   damp = NULL),
                    jump = 0.07)


## Get the appropriate model

model_type <- if (samp == "all") "pool" else "indiv"
model_name <- sprintf("%s_%s", model, model_type)
spec <- tryCatch(get(model_name),
                 error = function(e) e)
if (is(spec, "error"))
    stop(gettextf("invalid model name : %s",
                  model$message))


## Run model on historical data

stars <- paste0("\n\n\n\n", paste(rep("*", times = 79), collapse = ""), "\n\n")
cat(stars)
msg <- sprintf("model = '%s', samp = '%s', yr_est = '%s', len_pred = '%s' started at %s\n\n",
               model, samp, yr_est, len_pred, Sys.time())
cat(msg)

estimateModel(spec,
              y = deaths,
              exposure = exposure,
              filename = filename_est,
              nBurnin = n_burnin,
              nSim = n_sim,
              nChain = n_chain,
              nThin = n_thin,
              outfile = outfile)

s <- fetchSummary(filename_est)
print(s)


## Prediction

predictModel(filenameEst = filename_est,
             filenamePred = filename_pred,
             n = len_pred)


## Fetch combined historical and predicted mortality rates

rates <- fetchBoth(filenameEst = filename_est,
                   filenamePred = filename_pred,
                   where = c("model", "likelihood", "rate"))


## Save rates

saveRDS(rates,
        file = filename_rates)


## But, by default, delete model objects (which are large)

if (!keep) {
    file.remove(filename_est, filename_pred)
}

