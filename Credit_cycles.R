#Here, we are oing to look at the correlation between business cycles and credit cycles sing data on the aggregate financial sector.
library(lattice)
library(stats)
library(tseries)
library(Hmisc)
library(reshape)
library(mFilter)
#Cleaning up
rm(list = ls())
# Annual daata from FDIC Insured Banks from 1934-2014 
#Setting the working directory
setwd("~/Research/Credit_Cycles")
#Reading the Historical Data Set on
data_set <- read.csv("Credit_Cycle_data_r.csv", header = TRUE)
summary(data_set)

#Lets look at the data
attach(data_set)
hist(gdp_g, breaks =50)
hist(cpi_g, breaks =50)
hist(net_income_g, breaks =50)
hist(net_interest_income_g, breaks =50)
hist(non_interest_income_g, breaks =50)
hist(loan_leases_net_g, breaks = 50)
hist(total_deposits_g, breaks = 50)

detach(data_set)

#Lets look at the time series
attach(data_set)
plot(year, gdp_g,col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "GDP, CPI and Net Income",sub = "Annual growth (%)", xlab = "Year", ylab = "% growth")
lines(year, net_interest_income_g,col = "blue", type = "b", cex = 0.5, lty = 2, pch=1)
lines(year, loan_leases_net_g,col = "black", type = "b", cex = 0.5, lty = 2,pch=2 )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("topleft", c("ROA", "ROE", "MMF"), lty = c(2,2,2), col = c("red", "blue", "black"), pch = c(0,1,2), cex = 0.75)
detach(data_set)

#We need to filter the data......
#Cleaning up again
rm(list = ls())
data_set_levels <- read.csv("Credit_Cycle_data_levels_r.csv", header = TRUE)
summary(data_set_levels)
data_set_levels <- data_set_levels[order(data_set_levels$year), ]

#We are going to filter the data first
attach(data_set_levels)
gdp_hp = hpfilter(gdp_l, type = "lambda", freq = 6.25)$trend
cpi_hp = hpfilter(cpi_l, type = "lambda", freq = 6.25)$trend
net_interest_income_hp = hpfilter(net_interest_income_l, type = "lambda", freq = 6.25)$trend
non_interest_income_hp = hpfilter(non_interest_income_l, type = "lambda", freq = 6.25)$trend
loan_leases_net_hp = hpfilter(loan_leases_net_l, type = "lambda", freq = 6.25)$trend
total_deposits_hp = hpfilter(total_deposits_l, type = "lambda", freq = 6.25)$trend
detach(data_set_levels)

attach(data_set_levels)
data_set_levels <- cbind(data_set_levels, gdp_hp, cpi_hp, net_interest_income_hp, non_interest_income_hp, loan_leases_net_hp, total_deposits_hp)
detach(data_set_levels)

#Need to create a new data set. 
data_set_g = data.frame(data_set_levels$year[2:81])
names(data_set_g)[1] <- c("year")
gdp_g <- c(1:80)
cpi_g <- c(1:80)
net_interest_income_g <- c(1:80)
non_interest_income_g <- c(1:80)
loan_leases_net_g <- c(1:80)
total_deposits_g <- c(1:80)

#Creating the growth variable

for(i in 1:80){
  gdp_g[i] = (data_set_levels$gdp_hp[i+1]/data_set_levels$gdp_hp[i])*100-100
  cpi_g[i] = (data_set_levels$cpi_hp[i+1]/data_set_levels$cpi_hp[i])*100-100
  net_interest_income_g[i] = (data_set_levels$net_interest_income_hp[i+1]/data_set_levels$net_interest_income_hp[i])*100-100
  non_interest_income_g[i] = (data_set_levels$non_interest_income_hp[i+1]/data_set_levels$non_interest_income_hp[i])*100-100
  loan_leases_net_g[i] = (data_set_levels$loan_leases_net_hp[i+1]/data_set_levels$loan_leases_net_hp[i])*100-100
  total_deposits_g[i] = (data_set_levels$total_deposits_hp[i+1]/data_set_levels$total_deposits_hp[i])*100-100
}

#creating the database

data_set_g = cbind(data_set_g, gdp_g, cpi_g, net_interest_income_g, non_interest_income_g, loan_leases_net_g, total_deposits_g)
#cleaning up the data
rm(data_set_levels, gdp_g,cpi_g,net_interest_income_g, non_interest_income_g, loan_leases_net_g, total_deposits_g, gdp_hp,cpi_hp,net_interest_income_hp, non_interest_income_hp, loan_leases_net_hp, total_deposits_hp,i)

#Lets looo=k at the data now
attach(data_set_g)
plot(year, gdp_g,col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "GDP and Net Interest Income ",sub = "Annual growth (%), hp filtered", xlab = "Year", ylab = "% growth", ylim = c(-2,13))
lines(year, net_interest_income_g,col = "blue", type = "b", cex = 0.5, lty = 2, pch=1)
lines(year, loan_leases_net_g,col = "black", type = "b", cex = 0.5, lty = 2,pch=2 )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("topright", c("GDP", "NII"), lty = c(2,2), col = c("red", "blue"), pch = c(0,1), cex = 0.75)
detach(data_set_g)

attach(data_set_g)
plot(net_interest_income_g,gdp_g, col = "red", cex = 0.8, main = "GDP vs Net Interest Income ",sub = "Growth(%)", xlab = "Net Interest Income", ylab = "GDP" )
abline(lm(gdp_g~net_interest_income_g))
summary(lm(gdp_g~net_interest_income_g))
detach(data_set_g)

attach(data_set_g)
plot(year, gdp_g,col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "GDP and Total Credit ",sub = "Annual growth (%), hp filtered", xlab = "Year", ylab = "% growth", ylim = c(-2,13))
lines(year, loan_leases_net_g,col = "black", type = "b", cex = 0.5, lty = 2,pch=2 )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("topright", c("GDP", "Credit"), lty = c(2,2), col = c("red", "black"), pch = c(0,2), cex = 0.75)
detach(data_set_g)

attach(data_set_g)
plot(loan_leases_net_g,gdp_g, col = "red", cex = 0.8, main = "GDP vs Credit ",sub = "Growth(%)", xlab = "Credit", ylab = "GDP" )
abline(lm(gdp_g~loan_leases_net_g))
summary(lm(gdp_g~loan_leases_net_g))
detach(data_set_g)
