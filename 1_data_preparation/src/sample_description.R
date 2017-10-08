
library(readr)
library(dplyr)

country_codes <- read_csv("data/hmd_country_codes.csv")
deaths_raw <- readRDS("out/deaths_raw.rds")

first <- tapply(deaths_raw$year, deaths_raw$code, min)
last <- tapply(deaths_raw$year, deaths_raw$code, max)

sample_description <- country_codes %>%
    mutate(first = first[match(code, names(first))],
           last = last[match(code, names(last))]) %>%
    mutate(include = factor(include, levels = c("TRUE", "FALSE"), labels = c("Yes", "No"))) %>%
    mutate(label = ifelse(is.na(label), "-", label)) %>%
    select(Series = series, Code = code, First = first, Last = last, Included = include, Label = label) %>%
    arrange(desc(Last))

saveRDS(sample_description,
        file = "out/sample_description.rds")

    
