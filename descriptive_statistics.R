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

## Transactions level Descriptive Statistics ##

data<-fread('D:\\processed_final_data.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$Date<-ymd(data$Date)
class(data$Date)

prop.table(table(data$CURRENT_ENERGY_RATING))

# Barplot of the relative frequencies of EPC ratings 
# before and after the MEES 2018 policy intervention
EPC_intervention<-data.frame(EPC=toupper(letters[1:7]),
                             Proportion=c(as.numeric(prop.table(table(select(filter(data, 
                                                                                    Date < "2018-04-01"), 
                                                                             CURRENT_ENERGY_RATING)))),
                                          as.numeric(prop.table(table(select(filter(data, 
                                                                                    Date >= "2018-04-01"), 
                                                                             CURRENT_ENERGY_RATING))))),
                             Period=rep(c('Before MEES 2018', 'After MEES 2018'), each=7))

ggplot(EPC_intervention, aes(fill=Period, y=Proportion, x=EPC)) + 
  geom_bar(position="dodge", stat="identity", color="black") +
  scale_fill_manual(name=NULL, values=c("#17406A", "#FFFFFF")) +
  ylim(0, 0.5) +
  labs(x="EPC Rating", y="Proportion") +
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


# Barplot of the average transaction price by EPC ratings 
# before and after the MEES 2018 policy intervention
data$Period<-as.factor(ifelse(data$Date >= ymd("2018-04-01"), 
                              "After MEES 2018", "Before MEES 2018"))

ggplot(data, aes(x=CURRENT_ENERGY_RATING, y=Price/1000, fill=Period)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", color="black") +
  scale_fill_manual(name=NULL, values=c("#17406A", "#FFFFFF")) +
  labs(x="EPC Rating", y="Price (Thousands)") +
  scale_y_continuous(label=dollar_format(prefix = "\u00A3", big.mark = ",")) +
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

# Density plots

ggplot(filter(data, 
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

ggplot(filter(data, 
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


table(data$EPC_LEVEL)
prop.table(table(data$EPC_LEVEL))


## Property level Descriptive Statistics ##

data_cs<-data %>% distinct(POSTCODE, PAON, SAON, Street, .keep_all = T) 
data_cs<-select(data_cs, c(PROPERTY_TYPE, TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS,
                           CONSTRUCTION_AGE_BAND, TENURE, Group1))
dim(data_cs)
head(data_cs)

describe(data_cs$TOTAL_FLOOR_AREA)

round(prop.table(table(data_cs$PROPERTY_TYPE)), 5)

data_cs$CONSTRUCTION_AGE_BAND<-as.factor(data_cs$CONSTRUCTION_AGE_BAND)

levels(data_cs$CONSTRUCTION_AGE_BAND)[2:3]<-"1930-1966"
levels(data_cs$CONSTRUCTION_AGE_BAND)[3:6]<-"1967-1995"
levels(data_cs$CONSTRUCTION_AGE_BAND)[4:5]<-"1996-2006"

prop.table(table(data_cs$CONSTRUCTION_AGE_BAND))

table(data_cs$TENURE)
prop.table(table(data_cs$TENURE))

data_cs$NUMBER_HABITABLE_ROOMS_grouped<-factor(cut(data_cs$NUMBER_HABITABLE_ROOMS, 
                                                   breaks=quantile(data_cs$NUMBER_HABITABLE_ROOMS,
                                                                   na.rm=TRUE),
                                                   include.lowest=TRUE))

levels(data_cs$NUMBER_HABITABLE_ROOMS_grouped)[2:3]<-"[4,5]"
table(data_cs$NUMBER_HABITABLE_ROOMS_grouped)
round(prop.table(table(data_cs$NUMBER_HABITABLE_ROOMS_grouped)), 5)

prop.table(table(data_cs$Group1))
