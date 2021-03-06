# This R script can be used to replicate the DiD analysis
# for the paper "Climate Policy and Transition Risk in the Housing Market"
# by Konstantinos Ferentinos & Alex Gibberd & Benjamin Guin.
# The code was developed by Konstantinos Ferentinos.

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
library(reldist)

# In order to make the R code portable,
# whenever I intend to import or save data in a CSV format
# I define a variable with the name 'my_path' early in each R script 
# that stores the path to each CSV file that is used in the code.
# That way each user of the code can easily change the path at will,
# thus improving its reproducibility.
my_path<-'data\\'

data<-fread(paste(my_path, 'processed_final_data.csv', sep='\\'), header = T, 
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
psm_data<-fread(paste(my_path, 'psm_data_main.csv', sep='\\'), header = T, 
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

# Density plot for Price variable,
# as shown in Figure 4b of the paper.
ggplot(filter(did_data, 
              Date < "2018-04-01"), aes(x=Price/1000, fill=EPC_LEVEL)) +
  geom_density(alpha=0.4) +
  scale_fill_manual(name=NULL, labels=c("EPC Above E", "EPC Below E"),
                    values=c("#FFFFFF", "#17406A")) +
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
        legend.text=element_text(size=20),
        legend.position="bottom",
        legend.background = element_rect(linetype="solid", colour ="black"))

# Density plot for Price variable,
# as shown in Figure 4d of the paper.
ggplot(filter(did_data, 
              Date >= "2018-04-01"), aes(x=Price/1000, fill=EPC_LEVEL)) +
  geom_density(alpha=0.4) +
  scale_fill_manual(name=NULL, labels=c("EPC Above E", "EPC Below E"),
                    values=c("#FFFFFF", "#17406A")) +
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
        legend.text=element_text(size=20),
        legend.position="bottom",
        legend.background = element_rect(linetype="solid", colour ="black"))

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

# We fit the DiD model on the full balanced panel,
# and replicate column (1) of Table 8 of the paper.
did_reg <- plm(Price ~ Post + D, data = d, model = "within")
summary(did_reg)

# We replicate column (1) of Table 15 of the paper.
coeftest(did_reg, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# We focus on the sub-sample of properties 
# that have been private rented at any point before or after 1 April 2018.
rented<-select(filter(res, TENURE=='rental (private)'), 
               property)
head(rented)

res_rented<-filter(res, property%in%rented$property)
head(res_rented)
nrow(res_rented)

d_rented<-pdata.frame(res_rented, index = c("property", "Post"), drop.index = FALSE)
head(d_rented)

# We fit the DiD model on the sub-sample of properties 
# that have been private rented at least once,
# and replicate column (2) of Table 8 of the paper.
did_rented <- plm(Price ~ Post + D, data = d_rented, model = "within")
summary(did_rented)

# We replicate column (2) of Table 15 of the paper.
coeftest(did_rented, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))

# We replicate the QQ-plot in Figure 9a of the paper.
options(scipen = 999)
df <- data.frame(y = residuals(did_reg))
ggplot(df, aes(sample = y)) + 
  stat_qq() + stat_qq_line(size = 0.85) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size=20),
        legend.text=element_text(size=20))

# We calculate the skewness and kurtosis values for the residuals
# of the DiD model on the full balanced panel.
round(skewness(residuals(did_reg)), 3)
round(kurtosis(residuals(did_reg)), 3)

# We replicate Table 10 of the paper.
data_prior<-res %>% filter(Date < ymd("2018-04-01"))
head(data_prior)

data_post<-res %>% filter(Date >= ymd("2018-04-01"))
head(data_post)

# We calculate the Gini coefficients.
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
