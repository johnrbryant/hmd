
library(latticeExtra)

countries <- c("Sweden", "France Total Population", "Belgium", "England and Wales Total Population", "Norway", "Netherlands")
deaths <- readRDS("out/deaths.raw.rds")
exposure <- readRDS("out/exposure.raw.rds")
deaths <- subset(deaths,
                 subset = series %in% countries)
exposure <- subset(exposure,
                   subset = series %in% countries)
deaths$series <- factor(deaths$series)
exposure$series <- factor(exposure$series)
deaths <- subset(deaths, year >= 1850)
exposure <- subset(exposure, year >= 1850)
deaths$age <- cut(deaths$age, breaks = c(seq(0, 100, 10), Inf), right = FALSE)
exposure$age <- cut(exposure$age, breaks = c(seq(0, 100, 10), Inf), right = FALSE)
deaths <- xtabs(count ~ year + age + sex + series,
                data = deaths)
exposure <- xtabs(count ~ year + age + sex + series,
                  data = exposure)
rate <- deaths / exposure
relative <- apply(rate, 2:4, function(x) x/x[1])
relative <- as.data.frame.table(relative,
                                responseName = "value")
relative$year <- as.integer(levels(relative$year))[relative$year]
p <- xyplot(value ~ year | series * age,
            data = relative,
            groups = sex,
            col = c("black", "dark grey"),
            ylim = c(0, 2),
            as.table = TRUE,
            type = "l") + layer(panel.abline(h = 1, col = "grey")) + layer(panel.abline(v = seq(1850, 2000, 50), col = "grey"))
useOuterStrips(p)


xyplot(value ~ year | age,
       data = relative,
       subset = sex == "Female",
       type = "l")


p <- xyplot(log(value) ~ age | factor(year),


            data = rate,
            subset = sex == "Female",
            type = "l",
            col = "black", 
            lwd = 1,
            par.settings = list(fontsize = list(text = 8)),
            scale = list(y = "free"),
            as.table = FALSE)

rate <- rate[order(rate[c("year", "age")]),]

col <- rainbow(n = 112)
col[seq(11, 111, 10)] <- "#000000"
xyplot(log(value) ~ year,
       data = rate,
       groups = age,
            subset = sex == "Female",
            type = "l",
            col = col,
            lwd = 1,
            par.settings = list(fontsize = list(text = 8)),
            as.table = FALSE)



col <- rainbow(n = length(unique(rate$year)))
xyplot(log(value) ~ age,
       data = rate,
       groups = year,
            subset = sex == "Female",
            type = "l",
            col = col,
            lwd = 1,
            par.settings = list(fontsize = list(text = 8)),
            as.table = FALSE)

change <- rate
change$age <- cut(change$age, breaks = seq(0, 120, 10))
change <- xtabs(value ~ year + age + sex,
                data = change)
change <- apply(log(change), 2:3, diff)
names(dimnames(change))[1] <- "year"
change <- as.data.frame.table(change,
                              stringsAsFactors = FALSE,
                              responseName = "value")
change$year <- as.integer(change$year)
change <- change[order(change$age, change$year),]

xyplot(value ~ year | factor(age),
       data = change,
       groups = sex,
       col = "black",
       type = "smooth",
       ylim = c(-0.1, 0.1)) + layer(panel.abline(h = 0))


col = rainbow(n = length(unique(change$age)))
xyplot(value ~ year,
       data = change,
       groups = age,
       subset = sex == "Female",
       col = col,
       ylim = c(-0.1, 0.1),
       type = "l") + layer(panel.abline(h = 0))



            layout = c(3, 4, 12),
            



key = list(text = list(levels(log.death.rates$age)),
                       lines = list(col = col, lwd = 2),
                       columns = 4))


mx.folder <- "data/Mx_1x1"
names <- dir(mx.folder)

all <- lapply(names, getData)
all <- do.call(rbind, all)

levels.age <- seq(0, 110, 10)
log.death.rates <- subset(all,
                          age %in% levels.age)
log.death.rates$age <- factor(log.death.rates$age,
                              levels = levels.age)
col <- rainbow(n = length(levels.age))
p <- xyplot(value ~ year | sex * series,
            data = log.death.rates,
            groups = age,
            type = "l",
            col = col,
            lwd = 1,
            par.settings = list(fontsize = list(text = 8)),
            scale = list(y = list(log = TRUE)),
            as.true = FALSE,
            layout = c(3, 4, 12),
            key = list(text = list(levels(log.death.rates$age)),
                       lines = list(col = col, lwd = 2),
                       columns = 4))
graphics.off()
pdf(file = "log.death.rates.pdf",
    paper = "a4")
plot(p)
dev.off()

change <- xtabs(value ~ year + age + sex + series,
                data = all,
                subset = year %in% seq(1924, 2014, 10))
change <- apply(log(change), 2:4, diff)
names(dimnames(change))[1] <- "year"
change <- as.data.frame.table(change,
                              stringsAsFactors = FALSE,
                              responseName = "value")
change[c("year", "age")] <- lapply(change[c("year", "age")], as.integer)

levels.age <- seq(0, 100, 20)
change <- subset(change,
                 age %in% levels.age)
change$age <- factor(change$age,
                     levels = levels.age)
col <- rainbow(n = length(levels.age))
p <- xyplot(100 * value ~ year | sex * series,
            data = change,
            groups = age,
            type = "l",
            par.settings = list(fontsize = list(text = 8)),
            subset = age %in% seq(0, 110, 20),
            layout = c(3, 4, 12),
            lwd = 1.5,
            key = list(text = list(levels(change$age)),
                       lines = list(col = col, lwd = 2),
                       columns = 3))
graphics.off()
pdf(file = "decade.pc.change.pdf",
    paper = "a4")
plot(p)
dev.off()


levels.year <- seq(1924, 2014, 10)
age.profiles <- subset(all,
                       year %in% levels.year)
age.profiles$year <- factor(age.profiles$year,
                            levels = levels.year)
col <- rainbow(n = length(levels.year))
p <- xyplot(value ~ age | sex * series,
            data = age.profiles,
            groups = year,
            type = "l",
            col = col,
            lwd = 1,
            par.settings = list(fontsize = list(text = 8)),
            scale = list(y = list(log = TRUE)),
            as.true = FALSE,
            layout = c(3, 4, 12),
            key = list(text = list(levels(age.profiles$year)),
                       lines = list(col = col, lwd = 2),
                       columns = 4))
graphics.off()
pdf(file = "age.profiles.pdf",
    paper = "a4")
plot(p)
dev.off()
