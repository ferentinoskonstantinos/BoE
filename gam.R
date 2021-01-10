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
library(scales)
library(cowplot)

## Examining Pre-intervention Price Trends via Generalized Additive Models ##

data<-fread('D:\\psm_data_main.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$Date<-ymd(data$Date)
class(data$Date)
all(data$Date < "2018-04-01")

cols <- colnames(data)[c(15,18,19,22,24:26)]

data[cols] <- lapply(data[cols], factor)

# A continuous time covariate called 'Time' is constructed,
# by calculating the time difference in days between each row's date
# and the first transaction date of the matched dataset.
data$Time<-as.numeric(difftime(data$Date, min(data$Date), units = "days"))
data<-select(data, -c(Distance, Class, PScores, distance, weights))
head(data)

levels(data$EPC_LEVEL)<-c('Above E', 'Below E')
data$EPC_LEVEL <- relevel(data$EPC_LEVEL, ref="Below E")


# GAM with factor-smooth interactions
fit1 <- gam(Price ~ EPC_LEVEL + s(Time, by = EPC_LEVEL, bs="cr"), data = data)
summary(fit1)


# GAM with ordered-factor-smooth interactions

# We change EPC_LEVEL to an ordered factor
data$EPC_ordered <- as.ordered(data$EPC_LEVEL)
contrasts(data$EPC_ordered) <- 'contr.treatment'

fit2 <- gam(Price ~ EPC_ordered + s(Time, bs="cr") + s(Time, by=EPC_ordered, bs="cr"), data=data)
summary(fit2)

# This is the GAM that is reported in the paper.
fit3 <- gam(Price ~ EPC_ordered + NUTS118NM + s(Time, bs="cr") + s(Time, by=EPC_ordered, bs="cr") +
              Group1 + PROPERTY_TYPE + TOTAL_FLOOR_AREA + CONSTRUCTION_AGE_BAND + TENURE +
              NUMBER_HABITABLE_ROOMS_grouped, data=data)
summary(fit3)

b <- getViz(fit3)

# Estimated smooth trend for properties with EPC rating below E 
plot(sm(b, 1)) + l_fitLine(colour = "black", size = 0.8) +
  l_ciLine(level = 0.95, colour = "black", linetype = 2, size = 0.8) +
  scale_x_continuous(name="Date",
                     labels=function(x)year(x+ymd("2015-01-02"))) +
  scale_y_continuous(name="Reference Smooth") +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20),
        legend.text=element_text(size=20))

# Estimated smooth difference trend between 'Below E' and 'Above E'
plot(sm(b, 2)) + l_fitLine(colour = "black", size = 0.8) +
  l_ciLine(level = 0.95, colour = "black", linetype = 2, size = 0.8) +
  scale_x_continuous(name="Date",
                     labels=function(x)year(x+ymd("2015-01-02"))) +
  scale_y_continuous(name="Difference Smooth") +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20),
        legend.text=element_text(size=20))
