
library(methods)
library(dembase)
library(dplyr)

life_exp <- readRDS("out/life_exp.rds") %>%
    as.array()

diff_life_exp <- life_exp %>%
    as.array() %>%
    apply(., c("sex", "series"), diff)
names(dimnames(diff_life_exp))[1] <- "year"
diff_life_exp <- Values(diff_life_exp, dimscales = c(year = "Intervals"))

saveRDS(diff_life_exp,
        file = "out/diff_life_exp.rds")
