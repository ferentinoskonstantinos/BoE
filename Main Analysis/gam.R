# This R script can be used to replicate the GAM analysis
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
library(scales)
library(cowplot)

# The DiD analysis is valid only under the assumption that the outcomes in treatment 
# and control group follow the same time trend in the absence of the intervention.
# Yet, the standard approach does not go beyond visual inspections 
# and testing for differences in linear pre-intervention trends.
# Assuming a linear form for the outcome trend, restricts one from fitting a wider range 
# of possible shapes of the trend, that might be closer to its true functional form.
# As the standard approach for evaluating this assumption seems restrictive, 
# I present a novel assessment of the assumption, 
# using the GAM with factor-smooth interactions framework, 
# which formalizes the test for the statistical significance of the difference 
# in pre-intervention non-linear price trends between the treatment and control group.

# In order to make the R code portable,
# whenever I intend to import or save data in a CSV format
# I define a variable with the name 'my_path' early in each R script 
# that stores the path to each CSV file that is used in the code.
# That way each user of the code can easily change the path at will,
# thus improving its reproducibility.
my_path<-'data\\'

data<-fread(paste(my_path, 'psm_data_main.csv', sep='\\'), header = T, 
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

# GAM with ordered-factor-smooth interactions

# We treat EPC_LEVEL as an ordered factor
# in order to fit a smooth function for treated properties, 
# and then another smooth that models the difference between treated 
# and non-treated properties.
data$EPC_ordered <- as.ordered(data$EPC_LEVEL)
contrasts(data$EPC_ordered) <- 'contr.treatment'

# We replicate Table 7 of the paper.
fit <- gam(Price ~ EPC_ordered + NUTS118NM + s(Time, bs="cr") + s(Time, by=EPC_ordered, bs="cr") +
              Group1 + PROPERTY_TYPE + TOTAL_FLOOR_AREA + CONSTRUCTION_AGE_BAND + TENURE +
              NUMBER_HABITABLE_ROOMS_grouped, data=data)
summary(fit)

# We replicate Figure 5 of the paper.
b <- getViz(fit)

# Fitted price trend for properties with EPC rating below E 
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

# Estimated smooth for the difference between 'Below E' and 'Above E'
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
