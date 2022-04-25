library(dplyr)
library(plyr)
library(readr)
library(lubridate)
library(data.table)
library(psych)
library(ggplot2)
library(ggpubr)
library(scales)
library(cowplot)
library(lmtest)
library(plm)
library(moments)
library(mltools)
library(broom)

my_path1 <- 'processed_final_data.csv'
my_path2 <- 'psm_data_main.csv'

## Estimation of the Intervention Effect via a differenced model ##

data<-fread(my_path1, header = T, 
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
psm_data<-fread(my_path2, header = T, 
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

levels(did_data$TENURE)[2:3] <- "rental"
table(did_data$TENURE)

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

head(res)

pre_date<-res %>% filter(Date < ymd("2018-04-01")) %>% select(Price, Date, EPC_LEVEL,
                                                              CURRENT_ENERGY_RATING,
                                                              NUTS118NM, property, TENURE)

post_date<-res %>% filter(Date >= ymd("2018-04-01")) %>% select(Price, Date, EPC_LEVEL,
                                                                CURRENT_ENERGY_RATING,
                                                                NUTS118NM, property, TENURE)

diff_data<-inner_join(pre_date, post_date, by="property")

diff_data$first_diff<-diff_data$Price.y-diff_data$Price.x
head(diff_data)

ChangeOwnerToRented<-diff_data %>% filter(TENURE.x == "owner-occupied" & TENURE.y == "rental") %>% select(property)
ChangeRentedToOwner<-diff_data %>% filter(TENURE.x == "rental" & TENURE.y == "owner-occupied") %>% select(property)
RentedToRented<-diff_data %>% filter(TENURE.x == "rental" & TENURE.y == "rental") %>% select(property)

diff_data<-diff_data %>% select(property, first_diff, Date.y, EPC_LEVEL.y, NUTS118NM.y)

colnames(diff_data)[3:5] <- gsub(".y", "", colnames(diff_data)[3:5])

diff_data$ChangeOwnerToRented<-ifelse(diff_data$property %in% ChangeOwnerToRented$property, 1, 0)
diff_data$ChangeRentedToOwner<-ifelse(diff_data$property %in% ChangeRentedToOwner$property, 1, 0)
diff_data$RentedToRented<-ifelse(diff_data$property %in% RentedToRented$property, 1, 0)
diff_data$Post_2019<-ifelse(diff_data$Date >= ymd("2019-04-01"), 1, 0)

diff_data<-cbind(diff_data, as.data.frame(one_hot(as.data.table(diff_data$NUTS118NM))))
diff_data<-cbind(diff_data, as.data.frame(one_hot(as.data.table(diff_data$EPC_LEVEL))))
head(diff_data)

colnames(diff_data)[10:21] <- gsub(" ", "_", colnames(diff_data)[10:21])
colnames(diff_data)[10:21] <- gsub("\\(", "", colnames(diff_data)[10:21])
colnames(diff_data)[10:21] <- gsub(")", "", colnames(diff_data)[10:21])
colnames(diff_data)[21] <- "Below_E_Post_Intervention"

# DID model: region FE 
Table_2_regions_noconstant <- lm(first_diff ~ Below_E_Post_Intervention + V1_East_Midlands_England +
                 V1_East_of_England + V1_London + V1_North_East_England +
                 V1_North_West_England + V1_South_East_England +
                 V1_South_West_England + V1_Wales + V1_West_Midlands_England +
                 V1_Yorkshire_and_The_Humber -1
               , data = diff_data)
summary(Table_2_regions_noconstant)

# DID model: region FE and post 2019 
Table_3_post2019 <- lm(first_diff ~ Below_E_Post_Intervention 
                    + Post_2019*Below_E_Post_Intervention + Post_2019
                    + V1_East_Midlands_England +
                      V1_East_of_England + V1_London + V1_North_East_England +
                      V1_North_West_England + V1_South_East_England +
                      V1_South_West_England + V1_Wales + V1_West_Midlands_England +
                      V1_Yorkshire_and_The_Humber -1
                    , data = diff_data)
summary(Table_3_post2019)

# DID model: region FE and London 
Table_4_London <- lm(first_diff ~ Below_E_Post_Intervention 
                    + V1_London*Below_E_Post_Intervention 
                    + V1_East_Midlands_England +
                      V1_East_of_England + V1_London + V1_North_East_England +
                      V1_North_West_England + V1_South_East_England +
                      V1_South_West_England + V1_Wales + V1_West_Midlands_England +
                      V1_Yorkshire_and_The_Humber 
                    , data = diff_data)
summary(Table_4_London)

# DID model: region FE and change in tenure 
Table_5_Tenure <- lm(first_diff ~ Below_E_Post_Intervention 
                    +  ChangeOwnerToRented*Below_E_Post_Intervention +
                      ChangeRentedToOwner*Below_E_Post_Intervention +
                      RentedToRented*Below_E_Post_Intervention
                    +  ChangeOwnerToRented + ChangeRentedToOwner + RentedToRented
                    + V1_East_Midlands_England +
                      V1_East_of_England + V1_London + V1_North_East_England +
                      V1_North_West_England + V1_South_East_England +
                      V1_South_West_England + V1_Wales + V1_West_Midlands_England +
                      V1_Yorkshire_and_The_Humber -1
                    , data = diff_data)
summary(Table_5_Tenure)