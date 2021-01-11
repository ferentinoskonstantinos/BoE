# This R script can be used to replicate the data sourcing process
# for the paper "Climate Policy and Transition Risk in the Housing Market"
# by Konstantinos Ferentinos & Alex Gibberd & Benjamin Guin.
# The code was developed by Konstantinos Ferentinos.

library(dplyr)
library(plyr)
library(readr)
library(lubridate)
library(data.table)

## Data on property transactions ##

# After the yearly CSV files for the period 2015-2019,
# are downloaded from the HM Land Registry website
# https://data.gov.uk/dataset/314f77b3-e702-4545-8bcb-9ef8262ea0fd/archived-price-paid-data-1995-to-2017,
# I proceed to collate them into a single dataset.

# In order to make the R code portable,
# whenever I intend to import or save data in a CSV format
# I define a variable with the name 'my_path' early in each R script 
# that stores the path to each CSV file that is used in the code.
# That way each user of the code can easily change the path at will,
# thus improving its reproducibility.
my_path<-'data\\'

df_15<-fread(paste(my_path, 'pp-2015.csv', sep='\\'), header=F,
             data.table=FALSE)
colnames(df_15)<-c('ID', 'Price', 'Date', 'Postcode', 'Property', 'Age', 'Duration',
                   'PAON', 'SAON', 'Street', 'Locality', 'Town', 'District', 
                   'County', 'PPD_type', 'Record_Status')

df_16<-fread(paste(my_path, 'pp-2016.csv', sep='\\'), header=F,
             data.table=FALSE)
colnames(df_16)<-c('ID', 'Price', 'Date', 'Postcode', 'Property', 'Age', 'Duration',
                   'PAON', 'SAON', 'Street', 'Locality', 'Town', 'District', 
                   'County', 'PPD_type', 'Record_Status')

df_17_1<-fread(paste(my_path, 'pp-2017-part1.csv', sep='\\'), header=F,
               data.table=FALSE)
colnames(df_17_1)<-c('ID', 'Price', 'Date', 'Postcode', 'Property', 'Age', 'Duration',
                     'PAON', 'SAON', 'Street', 'Locality', 'Town', 'District', 
                     'County', 'PPD_type', 'Record_Status')

df_17_2<-fread(paste(my_path, 'pp-2017-part2.csv', sep='\\'), header=F,
               data.table=FALSE)
colnames(df_17_2)<-c('ID', 'Price', 'Date', 'Postcode', 'Property', 'Age', 'Duration',
                     'PAON', 'SAON', 'Street', 'Locality', 'Town', 'District', 
                     'County', 'PPD_type', 'Record_Status')

df_18_1<-fread(paste(my_path, 'pp-2018-part1.csv', sep='\\'), header=F,
               data.table=FALSE)
colnames(df_18_1)<-c('ID', 'Price', 'Date', 'Postcode', 'Property', 'Age', 'Duration',
                     'PAON', 'SAON', 'Street', 'Locality', 'Town', 'District', 
                     'County', 'PPD_type', 'Record_Status')

df_18_2<-fread(paste(my_path, 'pp-2018-part2.csv', sep='\\'), header=F,
               data.table=FALSE)
colnames(df_18_2)<-c('ID', 'Price', 'Date', 'Postcode', 'Property', 'Age', 'Duration',
                     'PAON', 'SAON', 'Street', 'Locality', 'Town', 'District', 
                     'County', 'PPD_type', 'Record_Status')

df_19<-fread(paste(my_path, 'pp-2019.csv', sep='\\'), header=F,
             data.table=FALSE)
colnames(df_19)<-c('ID', 'Price', 'Date', 'Postcode', 'Property', 'Age', 'Duration',
                   'PAON', 'SAON', 'Street', 'Locality', 'Town', 'District', 
                   'County', 'PPD_type', 'Record_Status')

# Total sample size
sum(nrow(df_15), nrow(df_16), nrow(df_17_1), nrow(df_17_2), nrow(df_18_1), nrow(df_18_2), nrow(df_19))

# Merging all the 7 data frames into a single dataset
data1<-rbind(df_15, df_16, df_17_1, df_17_2, df_18_1, df_18_2, df_19)

rm(df_15, df_16, df_17_1, df_17_2, df_18_1, df_18_2, df_19)

head(data1)

# Checking for duplicate values
sum(duplicated(data1$ID))

nrow(data1)

# Transforming the dates that are stored in character vectors to Date
data1$Date<-ymd(ymd_hm(data1$Date))

head(data1)

# We identify the number of properties with multiple transactions 
# before and after the date that the 2018 MEES came into force 
# (i.e. the properties which have at least one sale before 1 April 2018 
# and at least one sale after 1 April 2018), using columns
# PAON, SAON, Street, Town, and Postcode.
test<-select(data1, c(ID, PAON, SAON, Street, Town, Postcode, Date))
head(test)
res<-inner_join(test %>% filter(Date < "2018-04-01"), test %>% filter(Date >= "2018-04-01"), 
                by=colnames(test)[c(2:6)])
nrow(res)
nrow(distinct(res[,c(2:6)]))

# The data frame 'res', contains all rows from the filtered price dataset
# with transactions before 1 April 2018, 
# where there are matching values in the filtered price dataset
# with transactions after 1 April 2018, and all columns from both filtered datasets.
head(res)

# In order to extract and focus only on the properties 
# which have had a house sale both before and after the intervention date,
# we keep the rows with IDs that are in the matched dataset.
unique_ID<-c(unique(res$ID.x), unique(res$ID.y))

price_data<-filter(data1, ID %in% unique_ID)
nrow(price_data)
head(price_data)

# We save the price data frame as the price_data.csv file
fwrite(price_data, paste(my_path, 'price_data.csv', sep='\\'))


## EPC Dataset ##

price_data<-fread(paste(my_path, 'price_data.csv', sep='\\'), header = T, 
                  data.table=FALSE)
head(price_data)

# In order to solve the problem of the large EPC register 
# (consisting of 340 csv files of total size 16.1 GB), 
# we upload the files in R in batches of 68 files per data frame 
# (resulting in five data frames),
# keeping only those rows corresponding to the Postcodes found in the price dataset,
# and with dates prior to 2020, since the project focuses on the period 2015-2019.
# We then save the five data frames as five separate csv files.

# After we download the full register, that consists of the 340 data files, 
# one for each local authority district to which the postcodes of the registered properties 
# have been assigned, containing all the up to date EPC data from 2008 to the current date,
# and are sourced from https://epc.opendatacommunities.org/,
# we produce a list containing the names of files in the directory
# that holds the 340 energy performance certificates.
file_names<-list.files(paste(my_path, 'EPC', sep='\\'), full.names = TRUE)

file_names<-paste(file_names, 'certificates.csv', sep='\\')

'%notin%'<-Negate('%in%')

df<-NULL
names1<-file_names[1:69]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020 & POSTCODE%in%price_data$Postcode)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'epc_data1.csv', sep='\\'))

rm(df,data1,names1)


df<-NULL
names1<-file_names[70:137]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020 & POSTCODE%in%price_data$Postcode)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'epc_data2.csv', sep='\\'))

rm(df,data1,names1)


df<-NULL
names1<-file_names[138:205]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020 & POSTCODE%in%price_data$Postcode)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'epc_data3.csv', sep='\\'))

rm(df,data1,names1)


df<-NULL
names1<-file_names[206:273]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020 & POSTCODE%in%price_data$Postcode)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'epc_data4.csv', sep='\\'))

rm(df,data1,names1)


df<-NULL
names1<-file_names[274:340]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020 & POSTCODE%in%price_data$Postcode)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'epc_data5.csv', sep='\\'))


# We then upload the five different csv files, and for each data frame 
# we keep a subset of the columns.
epc_1<-fread(paste(my_path, 'epc_data1.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_1<-select(epc_1, c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
                       INSPECTION_DATE, LODGEMENT_DATE, TRANSACTION_TYPE,
                       CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, ADDRESS, PROPERTY_TYPE,
                       BUILT_FORM, ENVIRONMENT_IMPACT_CURRENT, ENERGY_CONSUMPTION_CURRENT, 
                       CO2_EMISSIONS_CURRENT, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
                       MAIN_FUEL, CONSTRUCTION_AGE_BAND, TENURE, 
                       LOCAL_AUTHORITY, CONSTITUENCY))

# We make the Address string uppercase and remove the commas 
# so as to have consistency of the ADDRESS string between the price dataset 
# and the EPC dataset.
epc_1$ADDRESS<-gsub(",","", toupper(epc_1$ADDRESS))

epc_2<-fread(paste(my_path, 'epc_data2.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_2<-select(epc_2, c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
                       INSPECTION_DATE, LODGEMENT_DATE, TRANSACTION_TYPE,
                       CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, ADDRESS, PROPERTY_TYPE,
                       BUILT_FORM, ENVIRONMENT_IMPACT_CURRENT, ENERGY_CONSUMPTION_CURRENT, 
                       CO2_EMISSIONS_CURRENT, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
                       MAIN_FUEL, CONSTRUCTION_AGE_BAND, TENURE, 
                       LOCAL_AUTHORITY, CONSTITUENCY))
epc_2$ADDRESS<-gsub(",","", toupper(epc_2$ADDRESS))

epc_3<-fread(paste(my_path, 'epc_data3.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_3<-select(epc_3, c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
                       INSPECTION_DATE, LODGEMENT_DATE, TRANSACTION_TYPE,
                       CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, ADDRESS, PROPERTY_TYPE,
                       BUILT_FORM, ENVIRONMENT_IMPACT_CURRENT, ENERGY_CONSUMPTION_CURRENT, 
                       CO2_EMISSIONS_CURRENT, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
                       MAIN_FUEL, CONSTRUCTION_AGE_BAND, TENURE, 
                       LOCAL_AUTHORITY, CONSTITUENCY))
epc_3$ADDRESS<-gsub(",","", toupper(epc_3$ADDRESS))

epc_4<-fread(paste(my_path, 'epc_data4.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_4<-select(epc_4, c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
                       INSPECTION_DATE, LODGEMENT_DATE, TRANSACTION_TYPE,
                       CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, ADDRESS, PROPERTY_TYPE,
                       BUILT_FORM, ENVIRONMENT_IMPACT_CURRENT, ENERGY_CONSUMPTION_CURRENT, 
                       CO2_EMISSIONS_CURRENT, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
                       MAIN_FUEL, CONSTRUCTION_AGE_BAND, TENURE, 
                       LOCAL_AUTHORITY, CONSTITUENCY))
epc_4$ADDRESS<-gsub(",","", toupper(epc_4$ADDRESS))

epc_5<-fread(paste(my_path, 'epc_data5.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_5<-select(epc_5, c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
                       INSPECTION_DATE, LODGEMENT_DATE, TRANSACTION_TYPE,
                       CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, ADDRESS, PROPERTY_TYPE,
                       BUILT_FORM, ENVIRONMENT_IMPACT_CURRENT, ENERGY_CONSUMPTION_CURRENT, 
                       CO2_EMISSIONS_CURRENT, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
                       MAIN_FUEL, CONSTRUCTION_AGE_BAND, TENURE, 
                       LOCAL_AUTHORITY, CONSTITUENCY))
epc_5$ADDRESS<-gsub(",","", toupper(epc_5$ADDRESS))

# We merge the five csv files into a single EPC dataset,
# and save it as the epc_data.csv file.
l<-list(epc_1, epc_2, epc_3, epc_4, epc_5)
epc_data<-rbindlist(l)
fwrite(epc_data, paste(my_path, 'epc_data.csv', sep='\\'))

