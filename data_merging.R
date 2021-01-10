library(dplyr)
library(plyr)
library(readr)
library(lubridate)
library(data.table)

## Data Merging ##

price_data<-fread('D:\\Data Files\\Main Analysis Data Files\\price_data.csv', header = T, 
                  data.table=FALSE)
head(price_data)

epc_data<-fread('D:\\Data Files\\Main Analysis Data Files\\epc_data.csv', header = T, 
                data.table=FALSE)
head(epc_data)

# We check whether there are any duplicate values in the EPC dataset.
sum(duplicated(epc_data$LMK_KEY))

epc_test<-select(epc_data, c(LMK_KEY, POSTTOWN, POSTCODE, INSPECTION_DATE, 
                             TRANSACTION_TYPE, CURRENT_ENERGY_RATING,
                             CURRENT_ENERGY_EFFICIENCY, ADDRESS, ADDRESS1, PROPERTY_TYPE,
                             BUILT_FORM, ENVIRONMENT_IMPACT_CURRENT, ENERGY_CONSUMPTION_CURRENT, 
                             CO2_EMISSIONS_CURRENT, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, 
                             MAIN_FUEL, CONSTRUCTION_AGE_BAND, TENURE, LOCAL_AUTHORITY))
head(epc_test)
dim(epc_test)

point_ref<-select(price_data, c(ID, PAON, SAON, Street, Locality, Town, 
                                Postcode, Date, Price, District, County))
head(point_ref)

rm(price_data, epc_data)

# We fix the Date variables of the two data frames.
epc_test$INSPECTION_DATE<-ymd(epc_test$INSPECTION_DATE)
point_ref$Date<-ymd(point_ref$Date)
class(epc_test$INSPECTION_DATE)
class(point_ref$Date)

colnames(point_ref)[7]<-'POSTCODE'

# In order to work around the problem of mis-matches in the ADDRESS string, 
# we create two addresses identifiers.
# The first is ADDRESS which is the full address string 
# with PAON, SAON, Street, and Locality.
# This variable was constructed to be the same (or approximately the same) with the
# ADDRESS variable of the EPC dataset
point_ref$ADDRESS <- do.call(paste, c(point_ref[c(3,2,4,5)], sep=" "))

# We remove from the concatenated address string of the price dataset 
# any leading and trailing whitespaces, as well as commas.
point_ref$ADDRESS<-trimws(point_ref$ADDRESS)
point_ref$ADDRESS<-gsub(",", "", point_ref$ADDRESS)

# We remove dots from the existing concatenated address string of the EPC dataset.
epc_test$ADDRESS<-gsub(".", "", epc_test$ADDRESS, fixed = TRUE)

# The second address identifier is PRIMARY_ADDRESS
# which is only the first line of the address,
# with PAON, SAON, and Street.
# This variable was constructed to be the same (or approximately the same) 
# with the ADDRESS1 variable of the EPC dataset.
point_ref$PRIMARY_ADDRESS <- do.call(paste, c(point_ref[c(3,2,4)], sep=" "))
point_ref$PRIMARY_ADDRESS<-trimws(point_ref$PRIMARY_ADDRESS)
point_ref$PRIMARY_ADDRESS<-gsub(",", "", point_ref$PRIMARY_ADDRESS)

epc_test$ADDRESS1 <- gsub(",","", toupper(epc_test$ADDRESS1))
epc_test$ADDRESS1<-trimws(epc_test$ADDRESS1)
epc_test$ADDRESS1<-gsub(".", "", epc_test$ADDRESS1, fixed = TRUE)

# The column ADDRESS1 of the EPC dataset,
# is renamed to PRIMARY_ADDRESS.
colnames(epc_test)[9]<-'PRIMARY_ADDRESS'

head(point_ref)
head(epc_test)

# Inner join of the two data frames for the same POSTCODE and ADDRESS, 
# without taking into account the date.
res<-inner_join(point_ref, epc_test, 
                by=c('POSTCODE', 'ADDRESS'))
head(res)
dim(res)

# For some properties, the only existing EPC rating 
# is registered in a later date than the transaction date.
# In order to address these issues, we select for each transaction 
# the closest prior registered EPC rating, and when there is no prior, 
# we use the closest registered EPC rating.
res<-mutate(res, Distance = abs(as.numeric(difftime(Date, INSPECTION_DATE, unit="days"))),
            Prior = ifelse(as.numeric(difftime(Date, INSPECTION_DATE, unit="days"))>=0, 
                           'Yes', 'No'))

length(unique(res$ID))

# We save all the transactions that had both prior 
# and posterior registered EPC ratings.
df.g<-res %>% group_by(ID) %>% filter(any(Prior=='No') & any(Prior=='Yes'))

# Then we save all the transactions that have homogeneous registered EPC ratings 
# (i.e. either all prior registered EPC ratings or all posterior registered EPC ratings).
df.h<-res %>% group_by(ID) %>% filter(all(Prior=='No') | all(Prior=='Yes'))

# For the transactions that had both prior and posterior registered EPC ratings, 
# I kept only the rows with the prior registered ones, 
# and for each transaction extracted with the slice() function the closest one.
df.g<-df.g %>% filter(Prior=='Yes') %>% slice(which.min(Distance))

# As for the transactions that had either all prior registered EPC ratings 
# or all posterior registered EPC ratings,
# I just took the closest registered EPC rating.
df.h<-df.h %>% slice(which.min(Distance))

# Then we bind the rows of the two data frames.
l <- list(df.g, df.h)
df<-rbindlist(l)
df<-as.data.frame(df)

head(df)

# We then remove the properties that had duplicated sales values
# i.e. multiple rows of a transaction of the same property
# at the same date with the same price, albeit with a different ID number due to a
# transcription error in the original Price Paid Data that were sourced from the HM
# Land Registry website.
# We also remove the properties with INVALID! values in their EPC ratings.
'%notin%'<-Negate('%in%')

unique_df<-df %>% distinct(POSTCODE, ADDRESS, Date, .keep_all = T)
dupes<-select(filter(df, ID%notin%(unique_df$ID)), c(ID, POSTCODE, ADDRESS, Date))

# An example of a property with a duplicated transaction value.
filter(df, ADDRESS=='3 ARCHER STREET')
filter(unique_df, ADDRESS=='3 ARCHER STREET')
filter(dupes, ADDRESS=='3 ARCHER STREET')

df<-df %>% filter(POSTCODE%notin%dupes$POSTCODE & ADDRESS%notin%dupes$ADDRESS)
df<-df %>% filter(ADDRESS!=unique(df$ADDRESS[df$CURRENT_ENERGY_RATING=='INVALID!']))

head(df)
dim(df)

rm(res, df.g, df.h, dupes, l, unique_df)

# I followed the same procedure for 'res1', 
# which is the result of the inner_join() function 
# between the price data frame and the EPC data frame, 
# in terms of POSTCODE and PRIMARY ADDRESS, 
# after extracting from the price data frame, 
# the transactions that did not have any matches 
# in the first step of the strategy.
head(point_ref)
head(epc_test)

point_ref1<-filter(point_ref, ID%notin%df$ID)
head(point_ref1)

res1<-inner_join(point_ref1, epc_test, 
                 by=c('POSTCODE', 'PRIMARY_ADDRESS'))
head(res1)
dim(res1)
res1<-mutate(res1, Distance = abs(as.numeric(difftime(Date, INSPECTION_DATE, unit="days"))),
             Prior = ifelse(as.numeric(difftime(Date, INSPECTION_DATE, unit="days"))>=0, 
                            'Yes', 'No'))

length(unique(res1$ID))

missing1<-res1 %>% group_by(ID) %>% filter(any(Prior=='No') & any(Prior=='Yes'))

missing2<-res1 %>% group_by(ID) %>% filter(all(Prior=='No') | all(Prior=='Yes'))

missing1<-missing1 %>% filter(Prior=='Yes') %>% slice(which.min(Distance))
missing2<-missing2 %>% slice(which.min(Distance))

l1 <- list(missing1, missing2)
df1<-rbindlist(l1)
df1<-as.data.frame(df1)

unique_df1<-df1 %>% distinct(POSTCODE, PRIMARY_ADDRESS, Date, .keep_all = T)
dupes<-select(filter(df1, ID%notin%(unique_df1$ID)), c(ID, POSTCODE, PRIMARY_ADDRESS, Date))

df1<-df1 %>% filter(POSTCODE%notin%dupes$POSTCODE & PRIMARY_ADDRESS%notin%dupes$PRIMARY_ADDRESS)
df1<-df1 %>% filter(PRIMARY_ADDRESS!=unique(df1$PRIMARY_ADDRESS[df1$CURRENT_ENERGY_RATING=='INVALID!']))

head(df1)
dim(df1)

# We check for any common transactions between the two data frames.
any(df$ID%in%df1$ID)

# In order to merge the two data frames into a single one,
# the columns order and names must be the same.
colnames(df)
colnames(df1)

colnames(df1)[20]<-'ADDRESS'
df<-select(df, -c(PRIMARY_ADDRESS.x, PRIMARY_ADDRESS.y, Distance))
df1<-select(df1, -c(ADDRESS.x, PRIMARY_ADDRESS, Distance))

head(df)
head(df1)
df1<-df1[,c(1:11,18,12:17,19:30)]

rm(dupes, l1, missing1, missing2, point_ref1, res1, unique_df1)

# We merge the two resulting data frames into a single one.
l <- list(df, df1)
data<-rbindlist(l)
data<-as.data.frame(data)
sum(duplicated(data$ID))

head(data)
dim(data)

#fwrite(data, 'D:\\initial_data.csv')

