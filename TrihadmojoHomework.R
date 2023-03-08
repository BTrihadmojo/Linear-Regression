#SOC401 Homework by Bambang Trihadmojo


#Part A
#creating variable Y with 6 observations
Y <-  c(12, 15, 9, 13, 5, 19)

#Calculating mean of Y
Y.mean <- mean(Y)
print(Y.mean)

#Calculating variance of Y
Y.var <- var(Y)
print(Y.var)

#Calculating standard deviation of Y
Y.sd <- sd(Y)
print(Y.sd)

#Part B
install.packages("Hmisc")
library(dplyr)
library(tidyverse)
library(Hmisc)

setwd("")

#Importing data

m1 <- read.csv("m1.csv", header = T)
m2 <- read.csv("m2.csv", header = T)
hepb <- read.csv("hepb.csv", header = T)
rotac <- read.csv("rotac.csv", header = T)
pcv <- read.csv("pcv.csv", header = T)
dtp <- read.csv("dtp.csv", header=T)
hib <- read.csv("hib.csv", header = T)
wgi <- read.csv("wgi.csv", header = T)
gdp <- read.csv("gdp.csv", header = T)

#Converting from str to numeric
wgi[3:8] <- sapply(wgi[3:8],as.numeric)
tambah <- function(x) {  
  ( x + 2.5)}
wgi[3:8] <- sapply(wgi[3:8],tambah)

#Combining variables
vaccine <- m1 %>% 
  left_join(m2, by = "Country") %>% 
  left_join(hepb, by = "Country") %>% 
  left_join(rotac, by = "Country") %>% 
  left_join(pcv, by = "Country") %>% 
  left_join(dtp, by = "Country") %>% 
  left_join(hib, by = "Country")

#Standardizing country name
vaccine$Country[vaccine$Country=="Czechia"] <- "Czech Republic"
vaccine$Country[vaccine$Country=="Côte d'Ivoire"] <- "Ivory Coast"

#Creating dataset
dataset <- vaccine %>% 
  left_join(wgi, by = "Country") %>% 
  left_join(gdp, by = "Code")

write.csv(dataset, "dataset.csv")

#Describing variables
describe <- str(dataset)
write.csv("describe.csv", describe)

#Checking missing value
summary(dataset)
summary(as.numeric(dataset$VoA))
miscountry <- dataset[is.na(dataset$Country.Name),]

missum <- sapply(dataset, function(x)sum(is.na(x)))
View(missum)
write.csv(missum, "missum.csv")

#Calculating means and SD
meansd <- sapply(dataset, function(mn) list(means=mean(mn,na.rm=TRUE),sd=sd(mn,na.rm = TRUE)))
meansd
write.csv(meansd, "meansd.csv")


#Creating scatterplot of M1(y) and PS(x)
plot(dataset$PS,dataset$M1)

#Linear Regression on select variables
fit1 <- lm(M1~PS, data = dataset)
summary(fit1)


