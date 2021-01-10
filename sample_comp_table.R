library(dplyr)
library(plyr)
library(readr)
library(lubridate)
library(data.table)
library(psych)
library(ggplot2)
library(ggpubr)
library(Information)
library(tableone)
library(knitr)

data1<-fread('D:\\repeated_sales.csv', header = T, 
             data.table=FALSE)
head(data1)
dim(data1)

data1$INSPECTION_DATE<-ymd(data1$INSPECTION_DATE)
class(data1$INSPECTION_DATE)

cols <- colnames(data1)[c(7,10,11,13,15,16)]

data1[cols] <- lapply(data1[cols], factor)

data1$NUMBER_HABITABLE_ROOMS_grouped<-factor(cut(data1$NUMBER_HABITABLE_ROOMS, 
                                                 breaks=quantile(data1$NUMBER_HABITABLE_ROOMS),
                                                 include.lowest=TRUE))
levels(data1$NUMBER_HABITABLE_ROOMS_grouped)[1]<-"less than 4"
levels(data1$NUMBER_HABITABLE_ROOMS_grouped)[2:3]<-"4 to 5"
levels(data1$NUMBER_HABITABLE_ROOMS_grouped)[3]<-"more than 5"
table(data1$NUMBER_HABITABLE_ROOMS_grouped)

levels(data1$CONSTRUCTION_AGE_BAND)[2:3]<-"1930-1966"
levels(data1$CONSTRUCTION_AGE_BAND)[3:6]<-"1967-1995"
levels(data1$CONSTRUCTION_AGE_BAND)[4:5]<-"1996-2006"

rm(cols)

data2<-fread('D:\\random_sample.csv', header = T, 
             data.table=FALSE)
head(data2)
dim(data2)

data2$INSPECTION_DATE<-ymd(data2$INSPECTION_DATE)
class(data2$INSPECTION_DATE)

cols <- colnames(data2)[c(7,10,11,13,15,16)]

data2[cols] <- lapply(data2[cols], factor)

data2$NUMBER_HABITABLE_ROOMS_grouped<-factor(cut(data2$NUMBER_HABITABLE_ROOMS, 
                                                 breaks=quantile(data2$NUMBER_HABITABLE_ROOMS),
                                                 include.lowest=TRUE))
levels(data2$NUMBER_HABITABLE_ROOMS_grouped)[1]<-"less than 4"
levels(data2$NUMBER_HABITABLE_ROOMS_grouped)[2:3]<-"4 to 5"
levels(data2$NUMBER_HABITABLE_ROOMS_grouped)[3]<-"more than 5"
table(data2$NUMBER_HABITABLE_ROOMS_grouped)

levels(data2$CONSTRUCTION_AGE_BAND)[2:3]<-"1930-1966"
levels(data2$CONSTRUCTION_AGE_BAND)[3:6]<-"1967-1995"
levels(data2$CONSTRUCTION_AGE_BAND)[4:5]<-"1996-2006"

colnames(data1)
colnames(data2)

l<-list(data1, data2)
epc_data<-rbindlist(l)
epc_data<-as.data.frame(epc_data)
head(epc_data)

# Summary of balance for the two datasets
xvars <- colnames(epc_data)[c(7,8,10,11,15,16,18)]
table1 <- CreateTableOne(vars = xvars, strata = "Dataset", data = epc_data, test = FALSE)
print(table1, smd = TRUE)
