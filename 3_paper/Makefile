
FIRST_YEAR = 1960

.PHONY: all
all : bmort.pdf \
      supplementary.pdf

.PHONY: data
data : out/life_exp.rds \
       out/deaths.rds \
       out/exposure.rds

bmort.pdf : bmort.tex \
            bmort.bib \
            out/fig_life_exp_sample.pdf \
            out/fig_mort_age_sample.pdf
	pdflatex -interaction=batchmode bmort
	bibtex -terse bmort
	pdflatex -interaction=batchmode bmort
	pdflatex -interaction=batchmode bmort

supplementary.pdf : supplementary.tex                    
	pdflatex -interaction=batchmode supplementary
	pdflatex -interaction=batchmode supplementary
	pdflatex -interaction=batchmode supplementary

bmort.tex : bmort.Rnw
	Rscript -e "knitr::knit('$<')"

supplementary.tex : supplementary.Rnw                    
	Rscript -e "knitr::knit('$<')"

supplementary.Rnw : out/sample_description.rds

out/model_three_way_base.pred : src/model_three_way_base.R \
                               out/model_three_way.est
	Rscript $< --n_year 25

out/model_three_way.est : src/model_three_way.R \
                          out/deaths.rds \
                          out/exposure.rds
	Rscript $< --n_burnin 50 --n_sim 50 --n_chain 4 --n_thin 5

out/model_two_way_dlm.est : src/model_two_way_dlm.R \
                            out/deaths.rds \
                            out/exposure.rds
	Rscript $< --n_burnin 5 --n_sim 5 --n_chain 4 --n_thin 1

out/model_two_way.est : src/model_two_way.R \
                        out/deaths.rds \
                        out/exposure.rds
	Rscript $< --n_burnin 2000 --n_sim 2000 --n_chain 4 --n_thin 10

out/fig_mort_age_sample.pdf : src/fig_mort_age_sample.R \
                              out/deaths.rds \
                              out/exposure.rds \
                              out/sample_countries.rds \
                              out/palette.rds
	Rscript $<

out/fig_life_exp_sample.pdf : src/fig_life_exp_sample.R \
                              out/life_exp.rds
	Rscript $<

out/fig_life_exp.pdf : src/fig_life_exp.R \
                       out/life_exp.rds
	Rscript $<

## Small sample of countries to be used in figures in main text.
out/sample_countries.rds : src/sample_countries.R
	Rscript $<

## Colour palette to be used in figures
out/palette.rds : src/palette.R
	Rscript $<

## Values object with data for series where Included is "Yes" in 'sample_description'.
out/life_exp.rds : src/life_exp.R \
                   out/sample_description.rds \
                   out/life_exp_raw.rds \
                   out/labels_popn_size.rds
	Rscript $<  --first_year $(FIRST_YEAR)

## Counts object with data for series where Included is "Yes" in 'sample_description'.
## Non-integer values rounded to integer.
out/deaths.rds : src/deaths.R \
                 out/sample_description.rds \
                 out/deaths_raw.rds \
                 out/labels_popn_size.rds
	Rscript $<  --first_year $(FIRST_YEAR)

## Counts object with data for series where Included is "Yes" in 'sample_description'.
out/exposure.rds : src/exposure.R \
                   out/sample_description.rds \
                   out/exposure_raw.rds \
                   out/labels_popn_size.rds
	Rscript $<  --first_year $(FIRST_YEAR)

out/labels_popn_size.rds : src/labels_popn_size.R \
                           out/exposure_raw.rds
	Rscript $<

## data.frame used as table in supplementary materials,
## and also to define sample series
out/sample_description.rds : src/sample_description.R \
                             data/hmd_country_codes.csv \
                             out/deaths_raw.rds
	Rscript $<

## data.frame with data for all series
out/life_exp_raw.rds : src/life_exp_raw.R \
                       data/hmd_country_codes.csv
	Rscript $<

## data.frame with data for all series. 15% of values are non-integer.
out/deaths_raw.rds : src/deaths_raw.R \
                     data/hmd_country_codes.csv
	Rscript $<

## data.frame with data for all series
out/exposure_raw.rds : src/exposure_raw.R \
                       data/hmd_country_codes.csv
	Rscript $<


.PHONY: clean
clean:
	rm -f *.aux *.log *.toc *.blg *.bbl *.synctex.gz *.idx *.lof *.lot *.tex *.pdf
	rm -rf out
	mkdir out

.PHONY: cleantex
cleantex:
	rm -f *.aux *.log *.toc *.blg *.bbl *.synctex.gz *.idx *.lof *.lot *.tex *.pdf
