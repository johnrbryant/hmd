
library(dembase)
library(dplyr)

first.year <- 1950

summarise <- function(x)
    c(equal0 = 100 * mean(x == 0),
      lt10 = 100 * mean(x < 10),
      lt30 = 100 * mean(x < 30),
      median = median(x),
      max = max(x))

deaths <- readRDS("out/deaths.rds") %>%
    subarray(year > first.year - 1) %>%
    as.numeric %>%
    na.omit %>%
    summarise

exposure <- readRDS("out/exposure.rds") %>%
    subarray(year > first.year - 1) %>%
    as.numeric %>%
    na.omit %>%
    summarise

