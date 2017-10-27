
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

indiv <- readRDS(sprintf("out/indiv_%s_%s_life_exp.rds", yr_est, len_pred))
pool <- readRDS(sprintf("out/pool_%s_%s_life_exp.rds", yr_est, len_pred))

life_exp <- dbind(Individual = indiv,
                  Pooled = pool,
                  along = "pooling")

file <- sprintf("out/life_exp_%s_%s.rds", yr_est, len_pred)
saveRDS(life_exp, file = file)
