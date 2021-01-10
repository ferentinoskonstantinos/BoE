library(dplyr)
library(plyr)
library(readr)
library(lubridate)
library(data.table)
library(psych)
library(ggplot2)
library(ggpubr)

## Data Processing ##

data<-fread('D:\\unique_epc_data.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$INSPECTION_DATE<-ymd(data$INSPECTION_DATE)
class(data$INSPECTION_DATE)

# We create a new categorical variable called EPC LEVEL, 
# with two categories: EPC rating above E and EPC rating below E.
data$EPC_LEVEL<-ifelse(data$CURRENT_ENERGY_RATING>'E', 'Below E', 'Above E')
head(select(data, c(CURRENT_ENERGY_RATING, EPC_LEVEL)))

# Next, the postcode of each property in the dataset 
# is matched with the corresponding 2011 OAC code, which ranges from 1a1 to 8d3.
codes<-fread('D:\\ONSPD_MAY_2020_UK.csv', header = T, 
             data.table=FALSE)
head(codes)
dim(codes)

codes<-select(codes, c(pcd, oac11))

codes$pcd<-gsub(" ","",codes$pcd)
data$pcd<-gsub(" ","",data$POSTCODE)

res<-inner_join(data, codes, by='pcd')
head(res)

res<-select(res, -pcd)

# We save the data frame with the added feature of the 2011 OAC codes.
#fwrite(res, 'D:\\unique_data_merged_epc.csv')

rm(codes, data, res)

# We add the Supergroup cluster names as a column in the dataset.
data<-fread('D:\\unique_data_merged_epc.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$INSPECTION_DATE<-ymd(data$INSPECTION_DATE)
class(data$INSPECTION_DATE)

oac11_data<-fread('D:\\Output_Area_Classification__December_2011__in_the_United_Kingdom.csv', header = T, 
                  data.table=FALSE)
head(oac11_data)
dim(oac11_data)

oac11_data$Group1<-trimws(sub(".*:", "", oac11_data$Supergroup))
oac11_data$oac11<-trimws(sub(":.*", "", oac11_data$Subgroup))

oac11_data<-select(oac11_data, c(Group1, oac11))

# The dataset that holds the three tiers of OAC clusters 
# and can be sourced from the Open Geography Portal, 
# is used to match each 2011 OAC code in the EPC dataset 
# with the 8 Supergroup cluster names.
res<-inner_join(data, oac11_data, by='oac11')
head(res)

# As a next step, a new nominal feature named NUTS118NM is added, 
# which corresponds to the NUTS Level 1 regions of the UK, 
# by matching the LOCAL_AUTHORITY variable of our combined dataset 
# with the corresponding NUTS Level 1 name.
colnames(res)[16]<-'LAU118CD'

codes<-fread('D:\\LAU218_LAU118_NUTS318_NUTS218_NUTS118_UK_LU.csv', header = T, 
             data.table=FALSE)
head(codes)
dim(codes)
codes<-select(codes, -c(LAU218CD, LAU218NM))
test<-distinct(codes, .keep_all = T)
head(test)
length(names(table(codes$LAU118CD)))
length(test$LAU118CD)

test<-select(test, c(LAU118CD, NUTS118NM))
head(test)

data1<-join(res, test, by='LAU118CD')
head(data1)

rm(codes,data,oac11_data,res,test)

# We proceed with removing missing values.
cols <- colnames(data1)[c(8,11,14,15)]

data1[cols] <- lapply(data1[cols], factor)

levels(data1$CONSTRUCTION_AGE_BAND)[c(1,13,14)]<-NA
levels(data1$CONSTRUCTION_AGE_BAND)[1:11]<-trimws(sub(".*:", "", 
                                                      levels(data1$CONSTRUCTION_AGE_BAND)[1:11]))

table(data1$CONSTRUCTION_AGE_BAND)

levels(data1$TENURE)[c(1,2,6)]<-NA

levels(data1$CURRENT_ENERGY_RATING)[8]<-NA

sum(is.na(data1$NUMBER_HABITABLE_ROOMS))
sum(is.na(data1$NUTS118NM))

data1<-data1[complete.cases(data1),]
nrow(data1)
head(data1)

#fwrite(data1, 'D:\\unique_modified_data.csv')

rm(data1, cols)

# Processing the sample of repeated property sales employed in the main analysis.
data<-fread('D:\\processed_final_data.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$Date<-ymd(data$Date)
class(data$Date)

data_cs<-data %>% distinct(POSTCODE, PAON, SAON, Street, .keep_all = T) 
data_cs<-select(data_cs, c(LMK_KEY, POSTCODE, INSPECTION_DATE, 
                           CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, 
                           ADDRESS, PROPERTY_TYPE, TOTAL_FLOOR_AREA, 
                           NUMBER_HABITABLE_ROOMS, CONSTRUCTION_AGE_BAND, 
                           TENURE, LAU118CD, EPC_LEVEL, oac11,
                           Group1, NUTS118NM))
dim(data_cs)
head(data_cs)

data_cs<-mutate(data_cs, Dataset=rep("Repeated_Sales", nrow(data_cs)))

##fwrite(data_cs, 'D:\\repeated_sales.csv')

rm(data, data_cs)

# Processing the random sample of the population of all properties
data<-fread('D:\\unique_modified_data.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$INSPECTION_DATE<-ymd(data$INSPECTION_DATE)
class(data$INSPECTION_DATE)

data<-mutate(data, Dataset=rep("Random_Sample", nrow(data)))

data<-select(data, -c(ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN))

##fwrite(data, 'D:\\random_sample.csv')
