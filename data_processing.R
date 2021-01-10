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

## Data Processing ##

data<-fread('D:\\initial_data.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$INSPECTION_DATE<-ymd(data$INSPECTION_DATE)
data$Date<-ymd(data$Date)
class(data$INSPECTION_DATE)
class(data$Date)

# We check for any irregularities in the merged data frame, 
# regarding the existence of properties which do not have sales both before 
# and after the intervention date, due to the merging strategy.
test<-select(data, c(ID, PAON, SAON, Street, Town, POSTCODE, Date))
head(test)

# In order to check for this issue, 
# I split the data in two separate data frames: one with transactions before 01/04/2018 
# and the other with transactions after 01/04/2018.
res<-inner_join(test %>% filter(Date < "2018-04-01"), test %>% filter(Date >= "2018-04-01"), 
                by=colnames(test)[c(2:6)])
nrow(res)
nrow(distinct(res[,c(2:6)]))

head(res)

# After taking their inner join by ADDRESS and POSTCODE, 
# I use the Transaction unique identifier code to extract from the full data frame 
# only those rows with sales before and after the intervention date 01/04/2018.
unique_ID<-c(unique(res$ID.x), unique(res$ID.y))

data<-filter(data, ID %in% unique_ID)
nrow(data)

# We create a new categorical variable called EPC LEVEL, 
# with two categories: EPC rating above E and EPC rating below E.
data$EPC_LEVEL<-ifelse(data$CURRENT_ENERGY_RATING>'E', 'Below E', 'Above E')
head(select(data, c(CURRENT_ENERGY_RATING, EPC_LEVEL)))

rm(res, test, unique_ID)

# Next, the postcode of each property in the merged dataset 
# is matched with the corresponding 2011 OAC code, which ranges from 1a1 to 8d3.
# This was achieved, by downloading the ONS Postcode Directory (ONSPD) for the
# United Kingdom in its May 2020 format, 
# from https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-may-2020,
# which relates both current and terminated UK postcodes 
# to a range of current area geographies, among them the 2011 OAC codes.
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

# We save the data frame with the added feature of the 2011 OAC codes,
# as the 'initial_data1.csv' file.
#fwrite(res, 'D:\\initial_data1.csv')

rm(codes, data, res)

# We start with checking for outliers in the Price variable.
data<-fread('D:\\initial_data1.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

describe(data$Price)

data$Date<-ymd(data$Date)
class(data$Date)

colnames(data)

# We remove the columns that will not be used in the project.
data<-select(data, -c(District, County, POSTTOWN, TRANSACTION_TYPE,
                      ENVIRONMENT_IMPACT_CURRENT, ENERGY_CONSUMPTION_CURRENT, 
                      CO2_EMISSIONS_CURRENT, MAIN_FUEL, BUILT_FORM))

quantile(data$Price, 0.01)
quantile(data$Price, 0.99)

# The data are trimmed using the 1st and 99th percentiles of the Price variable.
trimmed<-filter(data, Price>quantile(data$Price, 0.01) & Price<quantile(data$Price, 0.99))
head(trimmed)

# A number of the remaining properties do not have sales both before 
# and after the intervention date, so we re-calculate the number of properties 
# with at least two transactions: one before 1 April 2018, and one after 1 April 2018.
test<-select(trimmed, c(ID, PAON, SAON, Street, Town, POSTCODE, Date))
head(test)
res<-inner_join(test %>% filter(Date < "2018-04-01"), test %>% filter(Date >= "2018-04-01"), 
                by=colnames(test)[c(2:6)])
nrow(res)
nrow(distinct(res[,c(2:6)]))

head(res)

unique_ID<-c(unique(res$ID.x), unique(res$ID.y))

trimmed_data<-filter(trimmed, ID %in% unique_ID)
nrow(trimmed_data)
head(trimmed_data)

# We save the trimmed dataset as a csv file.
#fwrite(trimmed_data, 'D:\\trimmed_data.csv')

# The trimming of the data based on the percentiles of the Price variable, 
# has mitigated the issue of the outliers.
describe(data$Price)
describe(trimmed_data$Price)

ggplot(trimmed_data, aes(x=Price/1000)) + 
  geom_histogram(aes(y=..density..), col="black", fill="white", bins=15)+
  geom_density(alpha=.4, fill="#17406A") +
  labs(x="Price (Thousands)", y="Density") +
  scale_x_continuous(label=dollar_format(prefix = "\u00A3", big.mark = ",")) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20),
        legend.text=element_text(size=20))

rm(data, res, test, trimmed, trimmed_data, unique_ID)

# We add the Supergroup cluster names as a column in the trimmed dataset.
data<-fread('D:\\trimmed_data.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$Date<-ymd(data$Date)
class(data$Date)

# Output Area Classifications in their December 2011 format 
# were sourced from the Open Geography Portal
# https://geoportal.statistics.gov.uk/datasets/be648a095a9745998f6961e5ba54e01c_0.
oac11_data<-fread('D:\\Output_Area_Classification__December_2011__in_the_United_Kingdom.csv', header = T, 
                  data.table=FALSE)
head(oac11_data)
dim(oac11_data)

oac11_data$Group1<-trimws(sub(".*:", "", oac11_data$Supergroup))
oac11_data$oac11<-trimws(sub(":.*", "", oac11_data$Subgroup))

oac11_data<-select(oac11_data, c(Group1, oac11))

# The dataset that holds the three tiers of OAC clusters 
# and can be sourced from the Open Geography Portal, 
# is used to match each 2011 OAC code in the trimmed dataset 
# with the 8 Supergroup cluster names.
res<-inner_join(data, oac11_data, by='oac11')
head(res)

# As a next step, a new nominal feature named NUTS118NM is added, 
# which corresponds to the NUTS Level 1 regions of the UK, 
# by matching the LOCAL_AUTHORITY variable of our combined dataset 
# with the corresponding NUTS Level 1 name.
colnames(res)[20]<-'LAU118CD'

# We use a CSV file found in the CSV Collection of the ONSPD in
# its May 2020 format, that relates the LAU codes and names to the NUTS codes and names, 
# in their 2018 format.
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
cols <- colnames(data1)[c(18,19)]

data1[cols] <- lapply(data1[cols], factor)

levels(data1$CONSTRUCTION_AGE_BAND)[c(1,13,14)]<-NA
levels(data1$CONSTRUCTION_AGE_BAND)[1:11]<-trimws(sub(".*:", "", 
                                                      levels(data1$CONSTRUCTION_AGE_BAND)[1:11]))

table(data1$CONSTRUCTION_AGE_BAND)

levels(data1$TENURE)[c(1,2,6)]<-NA

sum(is.na(data1$NUMBER_HABITABLE_ROOMS))

sum(is.na(data1$NUTS118NM))

sum(is.na(data1$TOTAL_FLOOR_AREA))

data1<-data1[complete.cases(data1),]
nrow(data1)
head(data1)

# After removing the rows with missing values, 
# a number of the remaining properties do not have sales both before 
# and after the intervention date.
# So, we re-calculate the number of properties with at least two transactions: 
# one before 1 April 2018, and one after 1 April 2018.
test<-select(data1, c(ID, PAON, SAON, Street, Town, POSTCODE, Date))
head(test)
res<-inner_join(test %>% filter(Date < "2018-04-01"), test %>% filter(Date >= "2018-04-01"), 
                by=colnames(test)[c(2:6)])
nrow(res)
nrow(distinct(res[,c(2:6)]))

head(res)

unique_ID<-c(unique(res$ID.x), unique(res$ID.y))

data1<-filter(data1, ID %in% unique_ID)
nrow(data1)
head(data1)

# We save the final version of our merged panel dataset,
# as a csv file.
#fwrite(data1, 'D:\\processed_final_data.csv')
