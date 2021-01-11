# This R script can be used to replicate the GAM analysis
# for the sensitivity analysis that trims at the 5th and 95th percentile of prices
# for the paper "Climate Policy and Transition Risk in the Housing Market"
# by Konstantinos Ferentinos & Alex Gibberd & Benjamin Guin.
# The code was developed by Konstantinos Ferentinos.

library(dplyr)
library(plyr)
library(readr)
library(lubridate)
library(data.table)
library(psych)
library(ggplot2)
library(ggpubr)
library(mgcv)
library(mgcViz)

# In order to make the R code portable,
# whenever I intend to import or save data in a CSV format
# I define a variable with the name 'my_path' early in each R script 
# that stores the path to each CSV file that is used in the code.
# That way each user of the code can easily change the path at will,
# thus improving its reproducibility.
my_path<-'data\\'

data<-fread(paste(my_path, '5pc_psm_data.csv', sep='\\'), header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$Date<-ymd(data$Date)
class(data$Date)
all(data$Date < "2018-04-01")

cols <- colnames(data)[c(15,18,19,22,24,25)]

data[cols] <- lapply(data[cols], factor)

# A continuous time covariate called 'Time' is constructed,
# by calculating the time difference in days between each row's date
# and the first transaction date of the matched dataset.
data$Time<-as.numeric(difftime(data$Date, min(data$Date), units = "days"))
data<-select(data, -c(Distance, Class, PScores, distance, weights))
head(data)

levels(data$EPC_LEVEL)<-c('Above E', 'Below E')
data$EPC_LEVEL <- relevel(data$EPC_LEVEL, ref="Below E")

# GAM with ordered-factor-smooth interactions

# We change EPC_LEVEL to an ordered factor
data$EPC_ordered <- as.ordered(data$EPC_LEVEL)
contrasts(data$EPC_ordered) <- 'contr.treatment'

fit <- gam(Price ~ EPC_ordered + NUTS118NM + s(Time, bs="cr") + s(Time, by=EPC_ordered, bs="cr") +
              Group1 + PROPERTY_TYPE + TOTAL_FLOOR_AREA + CONSTRUCTION_AGE_BAND + TENURE +
              NUMBER_HABITABLE_ROOMS_grouped, data=data)
summary(fit)
