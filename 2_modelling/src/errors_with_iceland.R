
library(methods)
library(demest)
library(dplyr)


set.seed(0)

deaths <- readRDS("data/deaths.rds")
exposure <- readRDS("data/exposure.rds")

model <- Model(y ~ Poisson(mean ~ age + sex + year + 
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
               jump = 1)

filename <- "out/iceland_error.est"
outfile <- "out/iceland_error.log"

y <- subarray(deaths, country == "Iceland" & year <= 1990)
expose <- subarray(exposure, country == "Iceland" & year <= 1990)

estimateModel(model,
              y = y,
              exposure = expose,
              filename = filename,
              nBurnin = 50000,
              nSim = 50000,
              nChain = 4,
              nThin = 200,
              outfile = outfile)
fetchSummary(filename)
