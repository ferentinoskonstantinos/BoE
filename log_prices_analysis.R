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
library(lmtest)
library(plm)
library(moments)

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

# GAM with ordered-factor-smooth interactions

# We change EPC_LEVEL to an ordered factor
data$EPC_ordered <- as.ordered(data$EPC_LEVEL)
contrasts(data$EPC_ordered) <- 'contr.treatment'

fit <- gam(log(Price) ~ EPC_ordered + NUTS118NM + s(Time, bs="cr") + s(Time, by=EPC_ordered, bs="cr") +
              Group1 + PROPERTY_TYPE + TOTAL_FLOOR_AREA + CONSTRUCTION_AGE_BAND + TENURE +
              NUMBER_HABITABLE_ROOMS_grouped, data=data)
summary(fit)

rm(data, fit, cols)


## Estimation of the Intervention Effect via the Difference-in-Difference Model ##

data<-fread('D:\\processed_final_data.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$Date<-ymd(data$Date)
class(data$Date)

cols <- colnames(data)[c(15,18,19,22,24,25)]

data[cols] <- lapply(data[cols], factor)

data$NUMBER_HABITABLE_ROOMS_grouped<-factor(cut(data$NUMBER_HABITABLE_ROOMS, 
                                                breaks=quantile(data$NUMBER_HABITABLE_ROOMS),
                                                include.lowest=TRUE))
levels(data$NUMBER_HABITABLE_ROOMS_grouped)[2:3]<-"[4,5]"
table(data$NUMBER_HABITABLE_ROOMS_grouped)

rm(cols)

levels(data$CONSTRUCTION_AGE_BAND)[2:3]<-"1930-1966"
levels(data$CONSTRUCTION_AGE_BAND)[3:6]<-"1967-1995"
levels(data$CONSTRUCTION_AGE_BAND)[4:5]<-"1996-2006"


# We upload the PSM-derived matched dataset.
psm_data<-fread('D:\\psm_data_main.csv', header = T, 
                data.table=FALSE)
head(psm_data)
dim(psm_data)

psm_data$Date<-ymd(psm_data$Date)
class(psm_data$Date)
all(psm_data$Date < "2018-04-01")

cols <- colnames(psm_data)[c(15,18,19,22,24,25)]

psm_data[cols] <- lapply(psm_data[cols], factor)

rm(cols)

levels(psm_data$EPC_LEVEL)<-c('Above E', 'Below E')
head(psm_data)
psm_data<-select(psm_data, -c(Distance, Class, PScores, distance, weights))

# In order to regain the panel data structure that is necessary 
# for fitting the fixed effects DiD regression model, 
# the PSM-produced matched dataset of pre-intervention properties 
# is combined with their corresponding post-intervention transactions.
res<-inner_join(psm_data, data %>% filter(Date >= ymd("2018-04-01")), 
                by=colnames(psm_data)[c(7,2:4)])
nrow(res)

head(res)
any(unique(res$ID.x)%in%unique(res$ID.y))
unique_ID<-c(unique(res$ID.x), unique(res$ID.y))
length(unique_ID)

head(data)
did_data<-filter(data, ID %in% unique_ID)
nrow(did_data)
head(did_data)

rm(data, psm_data, res, unique_ID)

# We add variable 'Post', that takes the value 1 
# if the transaction date is post-intervention, and 0 otherwise.
did_data$Post<-as.factor(ifelse(did_data$Date >= ymd("2018-04-01"), "Post", "Pre"))
did_data$Post<-relevel(did_data$Post, "Pre")
contrasts(did_data$Post)

# We create a unique property identifier,
# by concatenating the variables POSTCODE, PAON, SAON, and Street.
did_data$property<-do.call(paste, c(did_data[c(7,2:4)], sep=" "))
did_data$property<-as.factor(did_data$property)
length(levels(did_data$property))
head(did_data)

# In order to create a balanced panel, 
# we keep for each property with multiple post-intervention transactions, 
# the post-intervention observation with the most recent transaction date.
did_data<-mutate(did_data, Distance = abs(as.numeric(difftime(ymd("2018-04-01"), Date, 
                                                              unit="days"))))
head(did_data)

res<-did_data %>% group_by(property, Post) %>% slice(which.max(Distance))
res<-as.data.frame(res)
head(res)
nrow(res)

any(table(did_data$property, did_data$Post)!=1)
any(table(res$property, res$Post)!=1)

rm(did_data)

# We construct variable 'D', that takes the value 1 
# for a post-intervention treated property, and 0 otherwise.
res<-mutate(res, D = ifelse(EPC_LEVEL=='Below E' & Post=='Post', 1, 0))
head(filter(res, D==1))

head(res)

# We transform the 'res' data frame into a panel data frame.
d<-pdata.frame(res, index = c("property", "Post"), drop.index = FALSE)
head(d)

# We fit the DiD model on the full balanced panel.
did_reg <- plm(log(Price) ~ Post + D, data = d, model = "within")
summary(did_reg)

coeftest(did_reg, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

