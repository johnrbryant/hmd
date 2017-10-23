
library(methods)
library(dembase)
library(docopt)

'
Usage:
life_exp.R [options]

Options:
--yr_est [default: 2015]
--len_pred [default: 25]
' -> doc

opts <- docopt(doc)
yr_est <- as.integer(opts$yr_est)
len_pred <- as.integer(opts$len_pred)

indiv_nodamp <- readRDS(sprintf("out/indiv_nodamp_%s_%s_life_exp.rds", yr_est, len_pred))
pool_nodamp <- readRDS(sprintf("out/pool_nodamp_%s_%s_life_exp.rds", yr_est, len_pred))
indiv_damp <- readRDS(sprintf("out/indiv_damp_%s_%s_life_exp.rds", yr_est, len_pred))
pool_damp <- readRDS(sprintf("out/pool_damp_%s_%s_life_exp.rds", yr_est, len_pred))

nodamp <- dbind(Individual = indiv_nodamp,
                Pooled = pool_nodamp,
                along = "pooling")
damp <- dbind(Individual = indiv_damp,
              Pooled = pool_damp,
              along = "pooling")

life_exp <- dbind("Not damped" = nodamp,
                  Damped = damp,
                  along = "damping")

file <- sprintf("out/life_exp_%s_%s.rds", yr_est, len_pred)
saveRDS(life_exp, file = file)
