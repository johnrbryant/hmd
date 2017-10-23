
.PHONY: all
all : bmort.pdf \
      out/fig_forecasts_heldback.pdf \
      out/fig_performance_indiv.pdf


## Paper

bmort.pdf : bmort.tex \
            bmort.bib \
            out/fig_life_exp_sample.pdf \
            out/fig_mort_age_sample.pdf \
            out/fig_performance_combined.pdf
	pdflatex -interaction=batchmode bmort
	bibtex -terse bmort
	pdflatex -interaction=batchmode bmort
	pdflatex -interaction=batchmode bmort


bmort.tex : bmort.Rnw
	Rscript -e "knitr::knit('$<')"


out/fig_performance_combined.pdf : src/fig_performance_combined.R \
                                   data/life_exp.rds \
                                   data/life_exp_1980_25.rds \
                                   data/life_exp_1990_25.rds \
                                   out/palette.rds
	Rscript $<


out/fig_life_exp_sample.pdf : src/fig_life_exp_sample.R \
                              data/life_exp.rds \
                              out/sample_countries.rds \
                              out/palette.rds
	Rscript $<


out/fig_mort_age_sample.pdf : src/fig_mort_age_sample.R \
                              data/deaths.rds \
                              data/exposure.rds \
                              out/sample_countries.rds \
                              out/palette.rds
	Rscript $<


## Supporting Figures

out/fig_forecasts_heldback.pdf : src/fig_forecasts_heldback.R \
                                 data/life_exp.rds \
                                 data/life_exp_1980_25.rds \
                                 data/life_exp_1990_25.rds
	Rscript $<


out/fig_performance_indiv.pdf : src/fig_performance_indiv.R \
                                data/life_exp.rds \
                                data/life_exp_1980_25.rds \
                                data/life_exp_1990_25.rds
	Rscript $<


## Shared Parameters

out/palette.rds : src/palette.R
	Rscript $<


out/sample_countries.rds : src/sample_countries.R
	Rscript $<


.PHONY: clean
clean:
	rm -f *.aux *.log *.toc *.blg *.bbl *.synctex.gz *.idx *.lof *.lot *.tex *.pdf
	rm -rf out
	mkdir out

.PHONY: cleantex
cleantex:
	rm -f *.aux *.log *.toc *.blg *.bbl *.synctex.gz *.idx *.lof *.lot *.tex *.pdf
