library(dplyr)
library(plyr)
library(readr)
library(lubridate)
library(data.table)
library(psych)
library(ggplot2)
library(ggpubr)

## Data Processing ##

# We proceed with checking for outliers in the Price variable.
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

quantile(data$Price, 0.05)
quantile(data$Price, 0.95)

# The data are trimmed using the 5th and 95th percentiles of the Price variable.
trimmed<-filter(data, Price>quantile(data$Price, 0.05) & Price<quantile(data$Price, 0.95))
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
#fwrite(trimmed_data, 'D:\\5pc_trimmed_data.csv')

# The trimming of the data based on the percentiles of the Price variable, 
# has mitigated the issue of the outliers.
describe(data$Price)
describe(trimmed_data$Price)

ggplot(trimmed_data, aes(x=Price)) + 
  geom_histogram(aes(y=..density..), col="red", fill="grey", bins=15)+
  geom_density(alpha=.2, fill="red") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20),
        legend.text=element_text(size=20))

rm(data, res, test, trimmed, trimmed_data, unique_ID)

# We add the Supergroup cluster names as a column in the trimmed dataset.
data<-fread('D:\\5pc_trimmed_data.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$Date<-ymd(data$Date)
class(data$Date)

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
data1$NUMBER_HABITABLE_ROOMS[743]

data1$NUTS118NM[970]
sum(is.na(data1$NUTS118NM))

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
#fwrite(data1, 'D:\\5pc_modified_data.csv')
