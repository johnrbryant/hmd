

SEED = 0 # random seed

## Variables used in comparison of models

KEEP_COMP = TRUE # whether to keep *.est and *.pred files
YR_EST_COMP = 2000 # final year of data used
LEN_PRED_COMP = 10 # length of prediction interval
N_BURNIN_COMP = 2000 # number of iterations for burnin
N_SIM_COMP = 2000 # number of iterations for production
N_CHAIN_COMP = 4 # number of independent chains
N_THIN_COMP = 10 # thinning interval


life_exp_comp = out/life_exp_comp_third.rds \
                out/life_exp_comp_fixsex.rds \
                out/life_exp_comp_fixagesex.rds \
                out/life_exp_comp_inform.rds


fig_comp = out/fig_comp_third.pdf \
           out/fig_comp_fixsex.pdf \
           out/fig_comp_fixagesex.pdf \
           out/fig_comp_inform.pdf


.PHONY: all
all: $(life_exp_comp) \
     $(fig_comp)


$(fig_comp) : src/fig_comp.R \
              $(life_exp_comp) \
              data/life_exp.rds
	Rscript $<  --model $(subst .pdf,,$(subst out/fig_comp_,,$@)) --yr_est $(YR_EST_COMP)


$(life_exp_comp) : src/life_exp_comp.R \
                   src/forecast_rates.R \
                   data/deaths.rds \
                   data/exposure.rds
	Rscript $< --model $(subst .rds,,$(subst out/life_exp_comp_,,$@)) \
                     --seed $(SEED) --yr_est $(YR_EST_COMP) --len_pred $(LEN_PRED_COMP) --keep $(KEEP_COMP) \
                     --n_burnin $(N_BURNIN_COMP) --n_sim $(N_SIM_COMP) --n_chain $(N_CHAIN_COMP) --n_thin $(N_THIN_COMP)


.PHONY: clean
clean:
	rm -rf out
	mkdir out

