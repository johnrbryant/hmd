

library(demest)
library(dplyr)

first.year <- 1960

breaks <- c(0, 1, seq(5, 100, 5))
deaths <- readRDS("out/deaths.rds") %>%
    subarray(year > first.year - 1) %>%
    collapseIntervals(dimension = "age", breaks = breaks)
exposure <- readRDS("out/exposure.rds") %>%
    subarray(year > first.year - 1) %>%
    collapseIntervals(dimension = "age", breaks = breaks)


summary <- function(x) {
    x <- as.numeric(x)
    pc.missing = mean(is.na(x))
    x <- na.omit(x)
    c(pc.missing = pc.missing,
      pc.equal.0 = 100 * mean(x == 0L),
      pc.lt.10 = 100 * mean(x < 10),
      pc.lt.30 = 100 * mean(x < 30),
      median = median(x),
      max = max(x))
}

round(summary(deaths), 1)
round(summary(exposure), 1)
