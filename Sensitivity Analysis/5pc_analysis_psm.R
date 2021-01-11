# This R script can be used to replicate the Propensity Score Matching analysis
# for the sensitivity analysis that trims at the 5th and 95th percentile of prices
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
library(Information)
library(tableone)
library(knitr)
library(ranger)
library(MatchIt)
library(caret)
library(pROC)
library(rpart.plot)
library(randomForest)

# In order to make the R code portable,
# whenever I intend to import or save data in a CSV format
# I define a variable with the name 'my_path' early in each R script 
# that stores the path to each CSV file that is used in the code.
# That way each user of the code can easily change the path at will,
# thus improving its reproducibility.
my_path<-'data\\'

data<-fread(paste(my_path, '5pc_modified_data.csv', sep='\\'), header = T, 
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
levels(data$NUMBER_HABITABLE_ROOMS_grouped)[3]<-"more than 5"
table(data$NUMBER_HABITABLE_ROOMS_grouped)

levels(data$CONSTRUCTION_AGE_BAND)[2:3]<-"1930-1966"
levels(data$CONSTRUCTION_AGE_BAND)[3:6]<-"1967-1995"
levels(data$CONSTRUCTION_AGE_BAND)[4:5]<-"1996-2006"

# We focus on the properties prior to the intervention date, 
# and if there multiple rows, we take the one that is closest to 01/04/2018.
data_prior<-data %>% filter(Date < ymd("2018-04-01"))
head(data_prior)

res<-mutate(data_prior, Distance = as.numeric(difftime(ymd("2018-04-01"), Date, unit="days")))
head(res)
nrow(res)

res<-res %>% group_by(POSTCODE, PAON, SAON, Street) %>% slice(which.min(Distance))
res<-as.data.frame(res)
head(res)
nrow(res)

# Propensity score estimation
head(res)
res<-mutate(res, Class = ifelse(EPC_LEVEL=='Below E', 1, 0))

# Summary of balance for all data before matching.
xvars <- colnames(res)[c(15,16,18,19,24:26)]
table1 <- CreateTableOne(vars = xvars, strata = "EPC_LEVEL", data = res, test = FALSE)
print(table1, smd = TRUE)

res<-filter(res, PROPERTY_TYPE!='Park home')
nrow(res)

table(res$PROPERTY_TYPE)
levels(res$PROPERTY_TYPE)[5]<-NA

levels(res$EPC_LEVEL)<-c('Control', 'Treated')

# Train/Test split
set.seed(3456)
trainIndex <- createDataPartition(res$EPC_LEVEL, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

train <- res[ trainIndex,]
test  <- res[-trainIndex,]

nrow(train)
nrow(test)

table(train$EPC_LEVEL)
prop.table(table(train$EPC_LEVEL))

table(test$EPC_LEVEL)
prop.table(table(test$EPC_LEVEL))

round(prop.table(table(res$EPC_LEVEL)), 4)*100

## Logistic Regression ##

# We use the caret package to fit the main effects logistic regression model
# to the training data.
ctrl <- trainControl(method = "cv", number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = TRUE)
glmFit <- train(EPC_LEVEL ~ PROPERTY_TYPE+TOTAL_FLOOR_AREA+CONSTRUCTION_AGE_BAND+TENURE+
                  Group1+NUTS118NM+NUMBER_HABITABLE_ROOMS_grouped, 
                data=train, method="glm", metric="Sens", family=binomial, trControl=ctrl)

# We plot the ROC curve and calculate the AUC score.
glmFit_pred<-predict(glmFit, test, type="prob")
my_roc_glm<-roc(as.integer(test$EPC_LEVEL)-1, unlist(glmFit_pred[2]))
plot(my_roc_glm)

auc(my_roc_glm)


## Random Forest ##

tgrid <- expand.grid(
  mtry = c(2,7),
  splitrule = "gini",
  min.node.size = 30)

set.seed(1000)
model_caret <- train(EPC_LEVEL ~ PROPERTY_TYPE+TOTAL_FLOOR_AREA+CONSTRUCTION_AGE_BAND+TENURE+
                       Group1+NUTS118NM+NUMBER_HABITABLE_ROOMS_grouped, data = train,
                     method = "ranger",
                     trControl = trainControl(method="cv", number = 5, classProbs = T),
                     tuneGrid = tgrid,
                     verbose = T,
                     num.trees=1000)

rfpred<-predict(model_caret, test, type = "prob")
my_roc_rf<-roc(as.integer(test$EPC_LEVEL)-1, unlist(rfpred[,2]))
plot(my_roc_rf)

auc(my_roc_rf)

# We can also fit a random forest using the ranger() function,
# since the caret package above takes a long time to run.
set.seed(1000)
rf <- ranger(EPC_LEVEL ~ PROPERTY_TYPE+TOTAL_FLOOR_AREA+CONSTRUCTION_AGE_BAND+TENURE+
               Group1+NUTS118NM+NUMBER_HABITABLE_ROOMS_grouped, data = train, 
             write.forest = TRUE, probability = T, num.trees = 1000, min.node.size = 30)

rfpred<-rf %>% predict(data = test) %>% predictions()
my_roc_rf<-roc(as.integer(test$EPC_LEVEL)-1, unlist(rfpred[,2]))
plot(my_roc_rf)

auc(my_roc_rf)


## eXtreme Gradient Boosting ##

ctrl <- trainControl(method = "cv", number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
gbmGrid <-  expand.grid(
  nrounds = 500, 
  max_depth = 6, 
  eta = c(0.01, 0.001, 0.0001), 
  gamma = 1, 
  colsample_bytree = 1, 
  min_child_weight = 1,
  subsample = 1
)

set.seed(1000)
model_caret <- train(EPC_LEVEL ~ PROPERTY_TYPE+TOTAL_FLOOR_AREA+CONSTRUCTION_AGE_BAND+TENURE+
                       Group1+NUTS118NM+NUMBER_HABITABLE_ROOMS_grouped, data = train,
                     method = "xgbTree", 
                     trControl = ctrl, 
                     verbose = T,
                     metric="ROC",
                     tuneGrid = gbmGrid)

boost_pred <- predict(model_caret, test, type = "prob")
head(boost_pred)
my_roc_boost<-roc(as.integer(test$EPC_LEVEL)-1, unlist(boost_pred[,2]))
plot(my_roc_boost)

auc(my_roc_boost)


# We use XGBoost to estimate the propensity scores,
# and implement PSM with one-to-one nearest neighbor method.
ctrl <- trainControl(method = "none",
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
gbmGrid <-  expand.grid(
  nrounds = 500,
  max_depth = 6,
  eta = 0.01,
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(1000)
model_xgb <- train(EPC_LEVEL ~ PROPERTY_TYPE+TOTAL_FLOOR_AREA+CONSTRUCTION_AGE_BAND+TENURE+
                     Group1+NUTS118NM+NUMBER_HABITABLE_ROOMS_grouped, data = res,
                   method = "xgbTree", 
                   trControl = ctrl, 
                   verbose = T,
                   metric="ROC",
                   tuneGrid = gbmGrid)

res$PScores <- predict(model_xgb, type = "prob")[,2]

head(res)

set.seed(1000)
m.out <- matchit(Class ~ PROPERTY_TYPE+TOTAL_FLOOR_AREA+CONSTRUCTION_AGE_BAND+TENURE+
                   Group1+NUTS118NM+NUMBER_HABITABLE_ROOMS_grouped, data=res, 
                 method="nearest", distance=res$PScores)

summary(m.out)
plot(m.out, type="hist", cex.lab=1.5, cex.axis=1.5, cex.main=1.5,
     col='steelblue')

# We create a dataframe containing only the matched observations.
dta_nearest1 <- match.data(m.out)
dim(dta_nearest1)

table_match_nearest1 <- CreateTableOne(vars = xvars, strata = "EPC_LEVEL", 
                                       data = dta_nearest1, test = FALSE)
print(table_match_nearest1, smd = TRUE)

head(dta_nearest1)
dim(dta_nearest1)
all(dta_nearest1$Date < "2018-04-01")

# We save the PSM-derived matched dataset,
# as a csv file.
fwrite(dta_nearest1, paste(my_path, '5pc_psm_data.csv', sep='\\'))

