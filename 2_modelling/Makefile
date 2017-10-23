

SEED = 0 # random seed
ALPHA = 0.2 # governs size of credible interval
N_BURNIN = 2500 # number of iterations for burnin
N_SIM = 2500 # number of iterations for production
N_CHAIN = 4 # number of independent chains
N_THIN = 20 # thinning interval

.PHONY: all
all: out/pool_damp_2010_25_life_exp.rds \
     out/pool_damp_2015_20_life_exp.rds \
     out/life_exp_1980_25.rds \
     out/life_exp_1990_25.rds


## slow-down forecasts

out/pool_damp_2010_25_life_exp.rds : src/one_model.R \
                                     data/deaths.rds \
                                     data/exposure.rds \
                                     data/life_exp.rds
	Rscript $< --model pool_damp \
                     --seed $(SEED) --yr_est 2010 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)

out/pool_damp_2015_20_life_exp.rds : src/one_model.R \
                                     data/deaths.rds \
                                     data/exposure.rds \
                                     data/life_exp.rds
	Rscript $< --model pool_damp \
                     --seed $(SEED) --yr_est 2015 --len_pred 20 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


## consolidated

out/life_exp_1980_25.rds : src/life_exp.R \
                           out/pool_nodamp_1980_25_life_exp.rds \
                           out/indiv_nodamp_1980_25_life_exp.rds \
                           out/pool_damp_1980_25_life_exp.rds \
                           out/indiv_damp_1980_25_life_exp.rds
	Rscript $< --yr_est 1980  --len_pred 25


out/life_exp_1990_25.rds : src/life_exp.R \
                           out/pool_nodamp_1990_25_life_exp.rds \
                           out/indiv_nodamp_1990_25_life_exp.rds \
                           out/pool_damp_1990_25_life_exp.rds \
                           out/indiv_damp_1990_25_life_exp.rds
	Rscript $< --yr_est 1990  --len_pred 25


# 1980

out/pool_nodamp_1980_25_life_exp.rds : src/one_model.R \
                                       data/deaths.rds \
                                       data/exposure.rds \
                                       data/life_exp.rds
	Rscript $< --model pool_nodamp \
                     --seed $(SEED) --yr_est 1980 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


out/indiv_nodamp_1980_25_life_exp.rds : src/one_model.R \
                                        data/deaths.rds \
                                        data/exposure.rds \
                                        data/life_exp.rds
	Rscript $< --model indiv_nodamp \
                     --seed $(SEED) --yr_est 1980 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)

out/pool_damp_1980_25_life_exp.rds : src/one_model.R \
                                     data/deaths.rds \
                                     data/exposure.rds \
                                     data/life_exp.rds
	Rscript $< --model pool_damp \
                     --seed $(SEED) --yr_est 1980 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


out/indiv_damp_1980_25_life_exp.rds : src/one_model.R \
                                      data/deaths.rds \
                                      data/exposure.rds \
                                      data/life_exp.rds
	Rscript $< --model indiv_damp \
                     --seed $(SEED) --yr_est 1980 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


# 1990

out/pool_nodamp_1990_25_life_exp.rds : src/one_model.R \
                                       data/deaths.rds \
                                       data/exposure.rds \
                                       data/life_exp.rds
	Rscript $< --model pool_nodamp \
                     --seed $(SEED) --yr_est 1990 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


out/indiv_nodamp_1990_25_life_exp.rds : src/one_model.R \
                                        data/deaths.rds \
                                        data/exposure.rds \
                                        data/life_exp.rds
	Rscript $< --model indiv_nodamp \
                     --seed $(SEED) --yr_est 1990 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)

out/pool_damp_1990_25_life_exp.rds : src/one_model.R \
                                     data/deaths.rds \
                                     data/exposure.rds \
                                     data/life_exp.rds
	Rscript $< --model pool_damp \
                     --seed $(SEED) --yr_est 1990 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


out/indiv_damp_1990_25_life_exp.rds : src/one_model.R \
                                      data/deaths.rds \
                                      data/exposure.rds \
                                      data/life_exp.rds
	Rscript $< --model indiv_damp \
                     --seed $(SEED) --yr_est 1990 --len_pred 25 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)





.PHONY: clean
clean:
	rm -rf out
	mkdir out

