
FIRST_YEAR = 1950

.PHONY: all
data : out/life_exp.rds \
       out/deaths.rds \
       out/exposure.rds \
       out/sample_description.rds


## Values object with data for series where Included is "Yes" in 'sample_description'.
## Calculated using 'deaths' and 'exposure' objects, plus functions from package
## 'demlife', rather than taken directly from Human Mortality Database.
out/life_exp.rds : src/life_exp.R \
                   out/deaths.rds \
                   out/exposure.rds 
	Rscript $<

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
	rm -rf out
	mkdir out
