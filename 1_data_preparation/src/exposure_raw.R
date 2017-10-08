
library(tidyverse)

country_codes <- read_csv("data/hmd_country_codes.csv")

dirname <- "data/Exposures_1x1"

read_file <- function(filename) {
    file <- file.path(dirname, filename)
    ans <- read.table(file, skip = 2, header = TRUE, na.string = ".")
    code <- strsplit(filename, split = ".", fixed = TRUE)[[1L]][1L]
    ans$code <- code
    ans
}

exposure_raw <- lapply(dir(dirname), read_file) %>%
    bind_rows %>%
    as_data_frame() %>%
    select(code, year = Year, age = Age, Female, Male) %>%
    gather(key = "sex", value = "count", -code, -year, -age) %>%
    left_join(y = country_codes, by = "code") %>%
    select(country = label, code, age, sex, year, count, include)

saveRDS(exposure_raw,
        file = "out/exposure_raw.rds")
    
