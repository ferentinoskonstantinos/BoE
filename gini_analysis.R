library(dplyr)
library(plyr)
library(readr)
library(lubridate)
library(data.table)
library(psych)
library(ggplot2)
library(ggpubr)
library(reldist)

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

data_prior<-res %>% filter(Date < ymd("2018-04-01"))
head(data_prior)

data_post<-res %>% filter(Date >= ymd("2018-04-01"))
head(data_post)

# We calculate the Gini coefficients
# using as data the full balanced panel of 40,128 transactions.
round(gini(data_prior$Price)*100, 2)
round(gini(data_post$Price)*100, 2)
round(gini(filter(data_post, EPC_LEVEL=="Below E")$Price+9000)*100, 2)

# We construct non-parametric 95% bootstrap confidence intervals
# for each Gini coefficient.
B <- 1000
parBS1 <- NULL
parBS2 <- NULL
parBS3 <- NULL

for(i in 1:B){
  set.seed(i)
  dataBS1 <- sample(data_prior$Price, length(data_prior$Price), replace=T)
  dataBS2 <- sample(data_post$Price, length(data_post$Price), replace=T)
  dataBS3 <- sample(filter(data_post, EPC_LEVEL=="Below E")$Price, 
                    length(filter(data_post, EPC_LEVEL=="Below E")$Price), replace=T)
  
  parBS1[i] <- gini(dataBS1)
  parBS2[i] <- gini(dataBS2)
  parBS3[i] <- gini(dataBS3+9000)
}

c(round(quantile(parBS1,0.025), 4), round(quantile(parBS1,0.975), 4))
c(round(quantile(parBS2,0.025), 4), round(quantile(parBS2,0.975), 4))
c(round(quantile(parBS3,0.025), 4), round(quantile(parBS3,0.975), 4))
