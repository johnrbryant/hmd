
library(methods)
library(dembase)
library(dplyr)
library(gridExtra)
library(latticeExtra)


life_exp_obs <- readRDS("data/life_exp.rds")
life_exp_pred_1980 <- readRDS("data/life_exp_1980_25.rds")
life_exp_pred_1990 <- readRDS("data/life_exp_1990_25.rds")

palette <- readRDS("out/palette.rds")

rmse80 <- sqrt((collapseIterations(life_exp_pred_1980, median) - life_exp_obs)^2) %>%
    as.data.frame(direction = "long") %>%
    mutate(launch = 1980)
rmse90 <- sqrt((collapseIterations(life_exp_pred_1990, median) - life_exp_obs)^2) %>%
    as.data.frame(direction = "long") %>%
    mutate(launch = 1990)
score80 <- life_exp_pred_1980 %>%
    intervalScore(truth = life_exp_obs, alpha = 0.2, na.rm = TRUE) %>%
    as.data.frame(direction = "long") %>%
    mutate(launch = 1980)
score90 <- life_exp_pred_1990 %>%
    intervalScore(truth = life_exp_obs, alpha = 0.2, na.rm = TRUE) %>%
    as.data.frame(direction = "long") %>%
    mutate(launch = 1990)
rmse <- rbind(rmse80, rmse90) %>%
    mutate(sex_age = paste(sex, age),
           year = as.integer(levels(year))[year])
score <- rbind(score80, score90) %>%
    mutate(sex_age = paste(sex, age),
           year = as.integer(levels(year))[year])

col <- c(palette$blue, palette$green)
countries <- dimnames(life_exp_obs)$country

graphics.off()
pdf(file = "out/fig_performance_indiv.pdf",
    paper = "a4")
for (COUNTRY in countries) {
    p_rmse <- xyplot(value ~ year | sex_age + factor(launch),
                     data = rmse,
                     type = "l",
                     subset = country == COUNTRY,
                     main = paste("RMSE", COUNTRY, sep = " - "),
                     groups = pooling,
                     col = col,
                     lwd = 1.5,
                     key = list(text = list(levels(score$pooling)),
                                lines = list(col = col, lwd = 1.5)))
    p_score <- xyplot(value ~ year | sex_age + factor(launch),
                      data = score,
                      type = "l",
                      subset = country == COUNTRY,
                      main = paste("Interval Score", COUNTRY, sep = " - "),
                      groups = pooling,
                      col = col,
                      lwd = 1.5,
                      par.settings = list(fontsize = list(text = 7)),
                      key = list(text = list(levels(score$pooling)),
                                 lines = list(col = col, lwd = 1.5)))
    p_rmse <- useOuterStrips(p_rmse)
    p_score <- useOuterStrips(p_score)
    m <- arrangeGrob(p_rmse, p_score, nrow = 2)
    plot(m)
}
dev.off()

       



    
            
    
