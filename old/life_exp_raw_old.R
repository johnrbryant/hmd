
library(tidyverse)

country_codes <- read_csv("data/hmd_country_codes.csv")

files_female <- file.path("data/fltper_5x1", dir("data/fltper_5x1"))
files_male <- file.path("data/mltper_5x1", dir("data/mltper_5x1"))

read_file <- function(file) {
    ans <- read.table(file, skip = 2, header = TRUE, na.string = ".")
    basename <- basename(file)
    code <- strsplit(basename, split = ".", fixed = TRUE)[[1L]][1L]
    ans$code <- code
    ans
}

life_exp_raw_female <- lapply(files_female, read_file) %>%
    bind_rows %>%
    as_data_frame() %>%
    mutate(sex = "Female") %>%
    select(code, age = Age, sex, year = Year, value = ex)

life_exp_raw_male <- lapply(files_male, read_file) %>%
    bind_rows %>%
    as_data_frame() %>%
    mutate(sex = "Male") %>%
    select(code, age = Age, sex, year = Year, value = ex)

life_exp_raw <- bind_rows(life_exp_raw_female, life_exp_raw_male) %>%
    left_join(y = country_codes, by = "code") %>%
    mutate(age = as.character(age)) %>%
    separate(age, into = c("age", "delete"), remove = TRUE, convert = TRUE, fill = "right") %>%
    select(-delete)

saveRDS(life_exp_raw,
        file = "out/life_exp_raw.rds")
    
