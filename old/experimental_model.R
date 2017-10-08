
library(demest)
library(dplyr)

first.year <- 1950

breaks <- c(0, 1, seq(5, 100, 5))
deaths <- readRDS("out/deaths.rds") %>%
    subarray(year > first.year - 1) %>%
    collapseIntervals(dimension = "age", breaks = breaks)
exposure <- readRDS("out/exposure.rds") %>%
    subarray(year > first.year - 1) %>%
    collapseIntervals(dimension = "age", breaks = breaks)
    
model <- Model(y ~ Poisson(mean ~ (age + sex + year + series)^2 + age:sex:year + age:sex:series),
               age ~ DLM(damp = NULL, 
                         covariates = Covariates(infant = TRUE)),
               year ~ DLM(damp = NULL),
               age:sex ~ Zero(),
               age:year ~ Zero(),
               age:series ~ Zero(),
               sex:year ~ Zero(),
               sex:series ~ Zero(),
               year:series ~ Mix(),
               age:sex:year ~ Mix(),
               age:sex:series ~ Mix())

filename <- "out/experimental_model.est"

estimateModel(model,
              y = deaths,
              exposure = exposure,
              filename = filename,
              nBurnin = 5000,
              nSim = 5000,
              nChain = 4,
              nThin = 20)
fetchSummary(filename)



filename.pred <- "out/experimental_model.pred"
predictModel(filenameEst = filename,
             filenamePred = filename.pred,
             n = 5)
