library(maps)
library(mapdata)
library(maptools)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)
library(data.table)
library(dplyr)
library(lubridate)

# We construct a heat map of the number of properties in our sample, 
# that are located in each of the 10 regions of England and Wales,
# using the online tutorial found in this link:
# https://datatricks.co.uk/creating-maps-in-r.
data<-fread('D:\\processed_final_data.csv', header = T, 
            data.table=FALSE)
head(data)
dim(data)

data$Date<-ymd(data$Date)
class(data$Date)

# I keep for each property the NUTS Level 1 region it is located in.
data_NUTS<-data %>% distinct(POSTCODE, ADDRESS, .keep_all = T) 
data_NUTS<-select(data_NUTS, c(ID, NUTS118NM))
head(data_NUTS)

# We calculate the number of properties in our sample, 
# that are located in each of the 10 regions of England and Wales.
number_NUTS<-aggregate(data_NUTS$ID, by=list(NUTS118NM=data_NUTS$NUTS118NM), FUN=length)
number_NUTS
colnames(number_NUTS)[2]<-"Total"

rm(data, data_NUTS)

codes<-fread('D:\\LAU218_LAU118_NUTS318_NUTS218_NUTS118_UK_LU.csv', header = T, 
             data.table=FALSE)
head(codes)
dim(codes)
codes<-select(codes, c(NUTS118CD, NUTS118NM))
test<-distinct(codes, .keep_all = T)
test

NUTS<-join(number_NUTS, test, by='NUTS118NM')
NUTS
colnames(NUTS)[3]<-"id"

mydata<-NUTS[,c(3,2)][order(NUTS$id),]
mydata

rm(codes, number_NUTS, NUTS, test)

# Heat map of England and Wales with the number of properties in each NUTS Level 1 Region,
# using the shapefiles sourced from https://geoportal.statistics.gov.uk/datasets/01fd6b2d7600446d8af768005992f76a_0.
shapefile <- readOGR(dsn="D:\\NUTS Level 1 shapefiles", 
                     layer="NUTS_Level_1__January_2018__Boundaries")

# We reshape the shapefiles, so as to use the ggplot2 package
mapdata <- tidy(shapefile, region="nuts118cd")
head(mapdata)

# We remove the coordinates for Scotland and Northern Ireland,
# in order to create a map for England and Wales only.
'%notin%'<-Negate('%in%')
mapdata<-filter(mapdata, mapdata$id%notin%c('UKM', 'UKN'))

# We see that we have kept only the NUTS Level 1 codes
# for England and Wales.
names(table(mapdata$id))
names(table(mydata$id))

#Join mydata with mapdata
df <- join(mapdata, mydata, by="id")

#Create the heatmap using the ggplot2 package
gg <- ggplot() + geom_polygon(data = df, aes(x = long, y = lat, group = group, fill = Total), color = "white", size = 0.25)
gg <- gg + scale_fill_gradient(low = "#6790BA", high = "#00042E", na.value = "black")
gg <- gg + coord_fixed(1)
gg <- gg + theme_minimal()
gg <- gg + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
gg <- gg + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
gg <- gg + theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
gg <- gg + theme(legend.title=element_text(size=20), legend.text=element_text(size=20))
gg <- gg + guides(fill = guide_colourbar(barheight = 10))
print(gg)
