
library(methods)
library(dembase)
library(dplyr)
library(latticeExtra)

life_exp <- readRDS("out/life_exp.rds") %>%
    as.data.frame(direction = "long", midpoints = "year")

av_life_exp <- life_exp %>%
    group_by(sex, year) %>%
    summarise(count = mean(count, na.rm = TRUE))

p <- xyplot(count ~ year | country,
            data = life_exp,
            groups = sex,
            xlab = "Year",
            type = "l",
            lwd = 2,
            ylab = "",
            col = c("dark red", "dark blue"),
            as.table = TRUE,
            par.settings = list(fontsize = list(text = 9)))
p <- p + as.layer(xyplot(count ~ year,
                         data = av_life_exp,
                         groups = sex,
                         col = "grey20",
                         type = "l"),
                  under = TRUE)

pdf(file = "out/fig_life_exp.pdf",
    paper = "a4r",
    height = 0,
    width = 0)
plot(p)
dev.off()
