

SEED = 0 # random seed
ALPHA = 0.2 # governs size of credible interval
N_BURNIN = 5 # number of iterations for burnin
N_SIM = 5 # number of iterations for production
N_CHAIN = 4 # number of independent chains
N_THIN = 1 # thinning interval

.PHONY: all
all: out/pool_nodamp_1995_20_life_exp.rds \
     out/indiv_nodamp_1995_20_life_exp.rds \

out/pool_nodamp_1995_20_life_exp.rds : src/one_model.R \
                                                data/deaths.rds \
                                                data/exposure.rds \
                                                data/life_exp.rds
	Rscript $< --model pool_nodamp \
                     --seed $(SEED) --yr_est 1995 --len_pred 20 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


out/indiv_nodamp_1995_20_life_exp.rds : src/one_model.R \
                                        data/deaths.rds \
                                        data/exposure.rds \
                                        data/life_exp.rds
	Rscript $< --model indiv_nodamp \
                     --seed $(SEED) --yr_est 1995 --len_pred 20 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)

out/pool_damp_1995_20_life_exp.rds : src/one_model.R \
                                                data/deaths.rds \
                                                data/exposure.rds \
                                                data/life_exp.rds
	Rscript $< --model pool_damp \
                     --seed $(SEED) --yr_est 1995 --len_pred 20 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


out/indiv_damp_1995_20_life_exp.rds : src/one_model.R \
                                        data/deaths.rds \
                                        data/exposure.rds \
                                        data/life_exp.rds
	Rscript $< --model indiv_damp \
                     --seed $(SEED) --yr_est 1995 --len_pred 20 --alpha $(ALPHA) \
                     --n_burnin $(N_BURNIN) --n_sim $(N_SIM) --n_chain $(N_CHAIN) --n_thin $(N_THIN)


.PHONY: clean
clean:
	rm -rf out
	mkdir out

