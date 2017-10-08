
library(methods)
library(dplyr)
library(demest)
library(docopt)

first_year <- 1960
deaths <- readRDS("out/deaths.rds") %>%
    subarray(year > first_year)
exposure <- readRDS("out/exposure.rds") %>%
    subarray(year > first_year)

deaths <- subarray(deaths, series == "USA" & age == "60-64")
exposure <- subarray(exposure, series == "USA" & age == "60-64")

model <- Model(y ~ Poisson(mean ~ year + sex),
               year ~ DLM(level = NULL, damp = NULL),
               jump = 0.01)

filename1 <- tempfile()
filename1.pred <- tempfile()
estimateModel(model,
              y = deaths,
              exposure = exposure,
              filename = filename1,
              nBurnin = 5000,
              nSim = 5000,
              nChain = 4,
              nThin = 5)
fetchSummary(filename1)
predictModel(filenameEst = filename1, filenamePred = filename1.pred, n = 25)


scale.lev <- fetchMCMC(filename1, c("mod", "hy", "ye", "scaleLev"))

rate <- fetchBoth(filename1, filename1.pred, c("mod", "lik", "rate"))
dplot( ~ year | sex, data = rate)

rate <- fetch(filename1, c("mod", "lik", "rate"))
dplot( ~ year | sex, data = rate)



direct <- deaths/exposure
diff.fem <- diff(log(direct[1,]), diff = 2)

## model <- Model(y ~ Poisson(mean ~ age * sex * year + age * sex * series + year * series),
##                age ~ DLM(damp = NULL,
##                          covariates = Covariates(infant = TRUE)),
##                year ~ DLM(damp = NULL),
##                age:sex ~ Zero(),
##                age:year ~ Zero(),
##                sex:year ~ Zero(),
##                age:sex:year ~ Mix(),
##                age:series ~ Zero(),
##                sex:series ~ Zero(),
##                age:sex:series ~ Mix(),
##                year:series ~ DLM(trend = NULL,
##                                  damp = NULL))

## model <- Model(y ~ Poisson(mean ~ (age + sex + series + year)^2 + age:sex:series + age:sex:year),
##                age ~ DLM(level = NULL,
##                          damp = NULL,
##                          covariates = Covariates(infant = TRUE)),
##                year ~ DLM(damp = NULL),
##                age:sex ~ Zero(),
##                age:series ~ Zero(),
##                age:year ~ Zero(),
##                sex:series ~ Zero(),
##                sex:year ~ Zero(),
##                series:year ~ Mix(),
##                age:sex:series ~ Mix(),
##                age:sex:year ~ Mix(),
##                jump = 0.07)


## model <- Model(y ~ Poisson(mean ~ (age + sex + series + year)^2 + age:sex:series + age:sex:year),
##                age ~ DLM(level = Level(scale = HalfT(scale = 5)),
##                          trend = NULL,
##                          damp = NULL,
##                          error = Error(scale = HalfT(scale = 0.000001, max = 0.000002))),
##                year ~ Exch(),
##                series ~ Exch(),
##                age:sex ~ Zero(),
##                age:series ~ Zero(),
##                age:year ~ Zero(),
##                sex:series ~ Zero(),
##                sex:year ~ Zero(),
##                series:year ~ Mix(),
##                age:sex:series ~ Exch(),
##                age:sex:year ~ Exch(),
##                jump = 0.07)


## model <- Model(y ~ Poisson(mean ~ (age + sex + series + year)^2 + age:sex:series + age:sex:year),
##                age ~ DLM(level = Level(scale = HalfT(scale = 5)),
##                          trend = NULL,
##                          damp = NULL,
##                          error = Error(scale = HalfT(scale = 0.000001, max = 0.000002))),
##                year ~ DLM(level = NULL,
##                           damp = NULL),
##                series ~ Exch(),
##                age:sex ~ Zero(),
##                age:series ~ Zero(),
##                age:year ~ Zero(),
##                sex:series ~ Zero(),
##                sex:year ~ Zero(),
##                series:year ~ Mix(),
##                age:sex:series ~ Exch(),
##                age:sex:year ~ Mix(),
##                jump = 0.07)


model <- Model(y ~ Poisson(mean ~ (age + sex + series + year)^3),
               age ~ DLM(level = NULL, damp = NULL),
               year ~ DLM(level = NULL, damp = NULL),
               age:sex ~ DLM(level = NULL, damp = NULL),
               age:series ~ Zero(),
               age:year ~ DLM(),
               sex:series ~ Zero(),
               sex:year ~ DLM(),
               series:year ~ DLM(),
               age:sex:series ~ Mix(),
               age:sex:year ~ Mix(),
               age:series:year ~ Mix(),
               sex:series:year ~ Mix(),
               jump = 0.07)



filename <- "out/model_base.est"
Sys.time()
estimateModel(model,
              y = deaths,
              exposure = exposure,
              filename = filename,
              nBurnin = n_burnin,
              nSim = n_sim,
              nChain = n_chain,
              nThin = n_thin)
Sys.time()
s <- fetchSummary(filename)
print(s)
for (i in 1:20) {
    print(Sys.time())
    continueEstimation(filename,
                       nBurnin = 200,
                       nSim = 200)
    print(Sys.time())
    s <- fetchSummary(filename)
    print(s)
}
for (i in 1:4) {
    print(Sys.time())
    continueEstimation(filename,
                       nBurnin = 0,
                       nSim = 200)
    print(Sys.time())
    s <- fetchSummary(filename)
    print(s)
}


