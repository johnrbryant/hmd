
library(methods)
library(demest)
library(dplyr)
library(demlife)


n_burnin <- 20000
n_sim <- 20000
n_chain <- 4
n_thin <- 80

deaths <- readRDS("data/deaths.rds") %>%
    subarray(year <= 1985)
exposure <- readRDS("data/exposure.rds") %>%
    subarray(year <= 1985)

pool <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
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

damp <- Model(y ~ Poisson(mean ~ age + sex + country + year + 
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
                                     damp = Damp(shape1 = 19, shape2 = 1, min = 0.8)),
              sex:country:year ~ DLM(trend = Trend(scale = HalfT(scale = 0.05)),
                                     damp = Damp(shape1 = 19, shape2 = 1, min = 0.8)),
              jump = 0.03)

filename_pool_est <- "out/pool.est"
filename_pool_pred <- "out/pool.pred"
filename_damp_est <- "out/damp.est"
filename_damp_pred <- "out/damp.pred"

estimateModel(pool,
              y = deaths,
              exposure = exposure,
              filename = filename_pool_est,
              nBurnin = n_burnin,
              nSim = n_sim,
              nChain = n_chain,
              nThin = n_thin)
s_pool <- fetchSummary(filename_pool_est)
print(s_pool)

estimateModel(damp,
              y = deaths,
              exposure = exposure,
              filename = filename_damp_est,
              nBurnin = n_burnin,
              nSim = n_sim,
              nChain = n_chain,
              nThin = n_thin)
s_damp <- fetchSummary(filename_damp_est)
print(s_damp)

predictModel(filenameEst = filename_pool_est,
             filenamePred = filename_pool_pred,
             n = 25)

predictModel(filenameEst = filename_damp_est,
             filenamePred = filename_damp_pred,
             n = 25)

rates_pool <- fetch(filename = filename_pool_pred,
                     where = c("model", "likelihood", "rate"))
rates_damp <- fetch(filename = filename_damp_pred,
                     where = c("model", "likelihood", "rate"))
lt_pool <- LifeTable(rates_pool)
lt_damp <- LifeTable(rates_damp)
le_pool <- lifeExpectancy(lt_pool)
le_damp <- lifeExpectancy(lt_damp)

le_true <- readRDS("data/life_exp.rds") %>%
    subarray(year > 1985) %>%
    subarray(age == "0")


le <- dbind(pool = collapseIterations(le_pool, median),
            damp = collapseIterations(le_damp, median),
            true = le_true, along = "le")
col <- c("red", "blue", "black")
dplot(~ year | country,
      data = le,
      groups = le,
      weights = 1,
      midpoints = "year",
      col = col,
      main = "Life expectancies",
      key = list(text = dimnames(le)["le"],
                 lines = list(col = col)))


mse_pool <- (collapseIterations(le_pool, median) - le_true)^2
mse_damp <- (collapseIterations(le_damp, median) - le_true)^2
mse <- dbind(mse_pool, mse_damp, along = "mse")
col <- c("red", "blue")
dplot(~ year | country,
      data = mse,
      groups = mse,
      weights = 1,
      midpoints = "year",
      col = col,
      main = "MSE",
      key = list(text = dimnames(mse)["mse"],
                 lines = list(col = col)))


score_pool <- intervalScore(values = le_pool,
                            truth = le_true,
                            alpha = 0.2)
score_damp <- intervalScore(values = le_damp,
                            truth = le_true,
                            alpha = 0.2)
score <- dbind(score_pool, score_damp, along = "score")
col <- c("red", "blue")
dplot(~ year | country,
      data = score,
      groups = score,
      weights = 1,
      midpoints = "year",
      col = col,
      main = "Interval score with 80% prediction intervals",
      key = list(text = dimnames(score)["score"],
                 lines = list(col = col)))

width_pool <- collapseIterations(le_pool, prob = 0.9) - collapseIterations(le_pool, prob = 0.1)
width_damp <- collapseIterations(le_damp, prob = 0.9) - collapseIterations(le_damp, prob = 0.1)
width <- dbind(width_pool, width_damp, along = "width")
col <- c("red", "blue")
dplot(~ year | country,
      data = width,
      groups = width,
      weights = 1,
      midpoints = "year",
      col = col,
      main = "Width of 80% prediction intervals",
      key = list(text = dimnames(width)["width"],
                 lines = list(col = col)))

