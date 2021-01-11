# This R script can be used to replicate the results of Table 11
# for the paper "Climate Policy and Transition Risk in the Housing Market"
# by Konstantinos Ferentinos & Alex Gibberd & Benjamin Guin.
# The code was developed by Konstantinos Ferentinos.

library(dplyr)
library(plyr)
library(readr)
library(lubridate)
library(data.table)

# In order to make the R code portable,
# whenever I intend to import or save data in a CSV format
# I define a variable with the name 'my_path' early in each R script 
# that stores the path to each CSV file that is used in the code.
# That way each user of the code can easily change the path at will,
# thus improving its reproducibility.
my_path<-'data\\'

# First, we produce a list containing the names of files in the directory
# that holds the 340 energy performance certificates,
# and we take a random sample of the population of all properties.
file_names<-list.files(paste(my_path, 'EPC', sep='\\'), full.names = TRUE)

file_names<-paste(file_names, 'certificates.csv', sep='\\')

'%notin%'<-Negate('%in%')

df<-NULL
names1<-file_names[1:69]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020) %>%
    select(c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
             INSPECTION_DATE, CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, 
             ADDRESS, PROPERTY_TYPE, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
             CONSTRUCTION_AGE_BAND, TENURE, LOCAL_AUTHORITY))
  set.seed(1000)
  df[[i]]<-distinct(df[[i]], ADDRESS, POSTCODE, .keep_all = TRUE)
  
  set.seed(1000)
  df[[i]]<-sample_frac(df[[i]], 0.035)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'unique_epc_data1.csv', sep='\\'))

rm(df,data1,names1)


df<-NULL
names1<-file_names[70:137]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020) %>%
    select(c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
             INSPECTION_DATE, CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, 
             ADDRESS, PROPERTY_TYPE, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
             CONSTRUCTION_AGE_BAND, TENURE, LOCAL_AUTHORITY))
  set.seed(1000)
  df[[i]]<-distinct(df[[i]], ADDRESS, POSTCODE, .keep_all = TRUE)
  
  set.seed(1000)
  df[[i]]<-sample_frac(df[[i]], 0.035)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'unique_epc_data2.csv', sep='\\'))

rm(df,data1,names1)


df<-NULL
names1<-file_names[138:205]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020) %>%
    select(c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
             INSPECTION_DATE, CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, 
             ADDRESS, PROPERTY_TYPE, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
             CONSTRUCTION_AGE_BAND, TENURE, LOCAL_AUTHORITY))
  set.seed(1000)
  df[[i]]<-distinct(df[[i]], ADDRESS, POSTCODE, .keep_all = TRUE)
  
  set.seed(1000)
  df[[i]]<-sample_frac(df[[i]], 0.035)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'unique_epc_data3.csv', sep='\\'))

rm(df,data1,names1)


df<-NULL
names1<-file_names[206:273]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020) %>%
    select(c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
             INSPECTION_DATE, CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, 
             ADDRESS, PROPERTY_TYPE, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
             CONSTRUCTION_AGE_BAND, TENURE, LOCAL_AUTHORITY))
  set.seed(1000)
  df[[i]]<-distinct(df[[i]], ADDRESS, POSTCODE, .keep_all = TRUE)
  
  set.seed(1000)
  df[[i]]<-sample_frac(df[[i]], 0.035)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'unique_epc_data4.csv', sep='\\'))

rm(df,data1,names1)


df<-NULL
names1<-file_names[274:340]
for(i in 1:length(names1)){
  df[[i]]<-fread(names1[i], header = T, data.table=FALSE)
  df[[i]]<-filter(df[[i]], year(ymd(INSPECTION_DATE))%notin%2020) %>%
    select(c(LMK_KEY, ADDRESS1, ADDRESS2, ADDRESS3, POSTTOWN, POSTCODE, 
             INSPECTION_DATE, CURRENT_ENERGY_RATING, CURRENT_ENERGY_EFFICIENCY, 
             ADDRESS, PROPERTY_TYPE, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
             CONSTRUCTION_AGE_BAND, TENURE, LOCAL_AUTHORITY))
  set.seed(1000)
  df[[i]]<-distinct(df[[i]], ADDRESS, POSTCODE, .keep_all = TRUE)
  
  set.seed(1000)
  df[[i]]<-sample_frac(df[[i]], 0.035)
}

data1 <- rbindlist(df)
fwrite(data1, paste(my_path, 'unique_epc_data5.csv', sep='\\'))


# We then upload the five different csv files.
epc_1<-fread(paste(my_path, 'unique_epc_data1.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_1$ADDRESS<-gsub(",","", toupper(epc_1$ADDRESS))

epc_2<-fread(paste(my_path, 'unique_epc_data2.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_2$ADDRESS<-gsub(",","", toupper(epc_2$ADDRESS))

epc_3<-fread(paste(my_path, 'unique_epc_data3.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_3$ADDRESS<-gsub(",","", toupper(epc_3$ADDRESS))

epc_4<-fread(paste(my_path, 'unique_epc_data4.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_4$ADDRESS<-gsub(",","", toupper(epc_4$ADDRESS))

epc_5<-fread(paste(my_path, 'unique_epc_data5.csv', sep='\\'), header = T, 
             data.table=FALSE)
epc_5$ADDRESS<-gsub(",","", toupper(epc_5$ADDRESS))

# We merge the five csv files into a single EPC dataset.
l<-list(epc_1, epc_2, epc_3, epc_4, epc_5)
epc_data<-rbindlist(l)
fwrite(epc_data, paste(my_path, 'unique_epc_data.csv', sep='\\'))
