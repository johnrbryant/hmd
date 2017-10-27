

SEED = 0 # random seed
ALPHA = 0.2 # governs size of credible interval
N_BURNIN = 1000 # number of iterations for burnin
N_SIM = 1000 # number of iterations for production
N_CHAIN = 4 # number of independent chains
N_THIN = 10 # thinning interval

.PHONY: all
all: out/pool_2010_25_life_exp.rds \
     out/pool_2015_20_life_exp.rds \
     out/life_exp_1980_25.rds \
     out/life_exp_1990_25.rds


## slow-down forecasts

out/pool_2010_25_life_exp.rds : src/one_model.R \
                                     data/deaths.rds \
                                     data/exposure.rds \
                                     data/life_exp.rds
	Rscript $< --model pool \
                     --seed $(SEED) --yr_est 2010 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)

out/pool_2015_20_life_exp.rds : src/one_model.R \
                                     data/deaths.rds \
                                     data/exposure.rds \
                                     data/life_exp.rds
	Rscript $< --model pool \
                     --seed $(SEED) --yr_est 2015 --len_pred 20 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


## consolidated

out/life_exp_1980_25.rds : src/life_exp.R \
                           out/pool_1980_25_life_exp.rds \
                           out/indiv_1980_25_life_exp.rds
	Rscript $< --yr_est 1980  --len_pred 25


out/life_exp_1990_25.rds : src/life_exp.R \
                           out/pool_1990_25_life_exp.rds \
                           out/indiv_1990_25_life_exp.rds
	Rscript $< --yr_est 1990  --len_pred 25


# 1980

out/pool_1980_25_life_exp.rds : src/one_model.R \
                                     data/deaths.rds \
                                     data/exposure.rds \
                                     data/life_exp.rds
	Rscript $< --model pool \
                     --seed $(SEED) --yr_est 1980 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


out/indiv_1980_25_life_exp.rds : src/one_model.R \
                                 data/deaths.rds \
                                 data/exposure.rds \
                                 data/life_exp.rds
	Rscript $< --model indiv \
                     --seed $(SEED) --yr_est 1980 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


# 1990

out/pool_1990_25_life_exp.rds : src/one_model.R \
                                data/deaths.rds \
                                data/exposure.rds \
                                data/life_exp.rds
	Rscript $< --model pool \
                     --seed $(SEED) --yr_est 1990 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


out/indiv_1990_25_life_exp.rds : src/one_model.R \
                                 data/deaths.rds \
data/exposure.rds \
                                      data/life_exp.rds
	Rscript $< --model indiv \
                     --seed $(SEED) --yr_est 1990 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


## clean

.PHONY: clean
clean:
	rm -rf out
	mkdir out

