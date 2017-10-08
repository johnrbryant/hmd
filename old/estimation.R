
library(demest)
library(docopt)

'
Usage:
estimation.R [options]

Options:
-v --variant [default: base]
-y --firstYear [default: 1900]
-b --nBurnin [default: 5]
-s --nSim [default: 5]
-c --nChain [default: 4]
-t --nThin [default: 1]
' -> doc
opts <- docopt(doc)
variant <- opts$variant
first.year <- as.integer(opts$firstYear)
n.burnin <- as.integer(opts$nBurnin)
n.sim <- as.integer(opts$nSim)
n.chain <- as.integer(opts$nChain)
n.thin <- as.integer(opts$nThin)

infant <- readRDS("out/infant.rds")
deaths <- readRDS("out/deaths.rds")
exposure <- readRDS("out/exposure.rds")

deaths <- subarray(deaths,
                   subarray = year > first.year - 1)
exposure <- subarray(exposure,
                     subarray = year > first.year - 1)

model <- Model(y ~ Poisson(mean ~ age * sex * year + age * sex * series),
               age ~ DLM(damp = NULL,
                         covariates = Covariates(mean ~ is.infant,
                                                 data = infant)),
               year ~ DLM(damp = NULL),
               age:year ~ Mix(),
               age:sex:year ~ Mix(),
               age:sex ~ Exch(),
               age:series ~ Exch(),
               age:sex:series ~ Exch())
               
filename <- sprintf("out/%s.est", variant)

estimateModel(model,
              y = deaths,
              exposure = exposure,
              filename = filename,
              nBurnin = n.burnin,
              nSim = n.sim,
              nChain = n.chain,
              nThin = n.thin)
s <- fetchSummary(filename)
print(s)
