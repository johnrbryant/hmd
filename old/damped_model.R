
library(demest)
deaths <- readRDS("data/deaths.rds")
exposure <- readRDS("data/exposure.rds")


model <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
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
                                      damp = Damp(min = 0.8, max = 1, shape1 = 19, shape2 = 1)),
               sex:country:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                      damp = Damp(min = 0.8, max = 1, shape1 = 19, shape2 = 1)),
               jump = 0.06)


filename.est <- "out/damped.est"
filename.pred <- "out/damped.pred"

estimateModel(model,
              y = deaths,
              exposure = exposure,
              filename = filename.est,
              nBurnin = 10000,
              nSim = 10000,
              nChain = 4,
              nThin = 40,
              outfile = "damp.log")
s <- fetchSummary("out/damped.est")
print(s)

predictModel(filenameEst = filename.est,
             filenamePred = filename.pred,
             n = 25)

rates <- fetchBoth(filenameEst = filename.est,
                   filenamePred = filename.pred,
                   where = c("model", "likelihood", "rate"))

dplot(~ year | country * age,
      data = rates,
      subarray = country %in% c("Iceland", "New Zealand", "USA") & sex == "Female" & age %in% c("0", "20-24", "40-44", "60-64", "80-84"),
      scales = list(y = list(log = TRUE)))
      

