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

## Examining Pre-intervention Price Trends via Generalized Additive Models ##

data<-fread('D:\\5pc_psm_data.csv', header = T, 
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
