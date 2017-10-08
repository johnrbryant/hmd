
library(latticeExtra)

n.iter <- 1000

age.sex.level <- fetch(filename, c("mod", "hy", "age:sex", "level"))
age.sex.level <- array(age.sex.level, dim = c(23, 2, n.iter))
alpha0.age.sex <- age.sex.level[1,,]
age.sex.level <- age.sex.level[-1,,]


sex <- fetch(filename, c("mod", "pr", "sex"), norm = F)
series <- fetch(filename, c("mod", "pr", "series"), norm = F)
intercept <- fetch(filename, c("mod", "pr", "(In"))
age.sex <- fetch(filename, c("mod", "pr", "age:sex"), norm = F)

sex.av <- 0.5 * subarray(sex, sex == "Female") + 0.5 * subarray(sex, sex == "Male")
sex.av.rep <- rep(sex.av, each = 2)
sex.tilde <- sex - sex.av.rep

intercept.tilde <- intercept + sex.av

alpha0.age.sex.av <- 0.5 * alpha0.age.sex[1, ] + 0.5 * alpha0.age.sex[2, ]
age.sex.level.tilde <- age.sex.level - rep(alpha0.age.sex.av, each = 22 * 2)
dimnames(age.sex.level.tilde) <- dimnames(age.sex)
age.sex.level.tilde <- Values(age.sex.level.tilde)
age.sex.level.tilde.mcmc <- demest:::MCMCDemographic(age.sex.level.tilde, nChain = 4)

intercept.tilde <- intercept.tilde + alpha0.age.sex.av

age.series.year <- fetch(filename, c("mod", "pr", "age:series:year"), norm = FALSE)
age.series.year.level <- fetch(filename, c("mod", "hy", "age:series:year", "level"))
age.series.year.level <- array(age.series.year.level, dim = c(22, 24, 57, n.iter))
alpha0.age.series.year.level <- age.series.year.level[,,1,]
age.series.year.level <- age.series.year.level[,,-1,]
dimnames(age.series.year.level) <- dimnames(age.series.year)
age.series.year.level <- Values(age.series.year.level, dimscale = c(year = "Intervals"))
alpha0.series <- apply(alpha0.age.series.year.level, 2:3, mean)
dimnames(alpha0.series) <- dimnames(series)
alpha0.series <- Values(alpha0.series)
age.series.year.level.tilde <- age.series.year.level - alpha0.series
age.series.year.level.tilde.mcmc <- demest:::MCMCDemographic(age.series.year.level.tilde, nChain = 4)

series.tilde <- series + alpha0.series
series.av <- apply(series, 2, mean)
series.tilde <- series.tilde - rep(series.av, each = 24)

intercept.tilde <- intercept.tilde + series.av
intercept.mcmc <- demest:::MCMCDemographic(intercept.tilde, nChain = 4)

year <- fetch(filename, c("mod", "pr", "year"), norm = F)
year.trend <- fetch(filename, c("mod", "hy", "year", "trend"))
year.trend <- matrix(year.trend, nr = 57)
year.trend <- year.trend[-1, ]
dimnames(year.trend) <- dimnames(year)
year.trend <- Values(year.trend, dimscales = c(year = "Intervals"))
age.series.year.trend <- fetch(filename, c("mod", "hy", "age:series:year", "trend"))
age.series.year.trend <- array(age.series.year.trend, dim = c(22, 24, 57, n.iter))
age.series.year.trend.0 <- age.series.year.trend[,,1,]
age.series.year.trend <- age.series.year.trend[,,-1,]
dimnames(age.series.year.trend) <- dimnames(age.series.year)
age.series.year.trend <- Values(age.series.year.trend, dimscales = c(year = "Intervals"))
year.av <- apply(age.series.year.trend.0, 3, mean)
age.series.year.trend.tilde <- age.series.year.trend - rep(year.av, each = 22*24*56)
year.trend.tilde <- year.trend + year.av



dplot(~ year | age, data = age.series.year.trend, subarray = series == "New Zealand")

dplot(~ year, data = year.trend.tilde)

age.series.year.trend.tilde.mcmc <- demest:::MCMCDemographic(age.series.year.trend.tilde, nCh = 4)
year.trend.tilde.mcmc <- demest:::MCMCDemographic(year.trend.tilde, nCh = 4)


p <- dplot(~ age | year * series, 
           data = comb, 
           subarray = series %in% c("Iceland", "New Zealand", "Spain", "Japan", "USA") & year %in% as.character(c(1960, 1970, 1980, 1990, 2000, 2010, 2015)))
p <- p + layer(panel.abline(h = 0))
useOuterStrips(p)

p <- dplot(~ year | age * series, 
           data = comb, 
           subarray = series %in% c("Iceland", "New Zealand", "Spain", "Japan", "USA") & age %in% c("0", "15-19", "30-34", "45-49", "60-64", "75-79", "100+"))
p <- p + layer(panel.abline(h = 0))
useOuterStrips(p)


dplot(~ year | series, data = comb, subarray =  age == "80-84")

comb <- year.trend.tilde + age.series.year.trend.tilde
