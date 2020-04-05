library(caret)
library(pscl)
library(plotROC)
library(pROC)
library(sf)
library(tidyverse)
library(knitr)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf")

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

##

Clg <- st_read("C:/Users/ililf/Dropbox/Spring_2020/CPLN_675/MidTerm/Calgary_Fishnet/Calgary_Fishnet.shp")

colnames(Clg)

Clg <- Clg %>%
  dplyr::select(-Id) %>%
  st_transform(crs=4326)

Clg[is.na(Clg)] <- 0

##  separate training and test sets

set.seed(3456)

trainIndex <- createDataPartition(Clg$inundation, p = .70, 
                                  list = FALSE,
                                  times = 1)

BothTrain <- Clg[ trainIndex,]
BothTest  <- Clg[-trainIndex,]

### estimate a logistic regression model
 
ClgModel <- glm(inundation ~ ., 
                     family="binomial"(link="logit"), data = BothTrain %>%
                       as.data.frame() %>%
                       dplyr::select(-geometry))

summary(ClgModel)

#

ClgModeltest <- glm(inundation ~ ., 
                family="binomial"(link="logit"), data = BothTrain %>%
                  as.data.frame() %>%
                  dplyr::select(-geometry,-ELEV))

summary(ClgModeltest)

# Sign. variables: elevation and distance to stream. If dropping elev: len becomes significant. 
# Although land use variables are not significant in the model, ResO (residential that is not low density) has P value of 0.14 
# which is relativly low.

# Model validation

classProbs <- predict(ClgModel, Clg, type="response")

hist(classProbs)

#

testProbs <- data.frame(obs = as.numeric(Clg$inundation),
                        pred = classProbs)

ggplot(testProbs, aes(x = pred, fill=as.factor(obs))) + geom_density() +
  facet_grid(obs ~ .) + xlab("Probability") + geom_vline(xintercept = .5) +
  scale_fill_manual(values = c("dodgerblue4", "darkgreen"),
                    labels = c("Not Inundated","Inundated"),
                    name = "")

# I think it means we can predict the 'not' but cannot predict the 'yes'. 

# at which probability level do we wish to classify land as being preserved.How do we make this decision? 0.5 (50%)

testProbs$predClass  = ifelse(testProbs$pred > .5 ,1,0)

caret::confusionMatrix(reference = as.factor(testProbs$obs), 
                       data = as.factor(testProbs$predClass), 
                       positive = "1")

#Predicted = 0, Observed = 0 -> True Negative
#Predicted = 1, Observed = 1 -> True Positive
#Predicted = 1, Observed = 0 -> False Positive
#Predicted = 0, Observed = 1 -> False Negative

# Our true negative is really high but our true positive is really low.

# Sensitivity: VERY LOW, Specificity: VERY HIGH

# WE NEED TO TRY TO CHANGE OUR TRESHOLD:

testProbs$predClass  = ifelse(testProbs$pred > .3 ,1,0)

caret::confusionMatrix(reference = as.factor(testProbs$obs), 
                       data = as.factor(testProbs$predClass), 
                       positive = "1")

# Sensitivity: A BIT HIGHER, Specificity: STILL VERY HIGH

# Sensitivity - the proportion of actual positives (1's) that were predicted to be positive. Also known as "true positive rate".
# Specificity - The proportion of actual negatives (0's) that were predicted to be negatives. Also known as "true negative rate".

# ROC curve:

ggplot(testProbs, aes(d = obs, m = pred)) + 
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') 

auc(testProbs$obs, testProbs$pred) 

# Cross Validation: 

ctrl <- trainControl(method = "cv", 
                     number = 100, 
                     savePredictions = TRUE)

clgNoGeo <- Clg %>% as.data.frame() %>% dplyr::select(-geometry)

cvFit <- caret::train(as.factor(inundation) ~ .,  data = clgNoGeo,
               method="glm", family="binomial",
               trControl = ctrl)

cvFit

#trying with dropping:

cvFit <- caret::train(as.factor(inundation) ~ .,  data = clgNoGeo %>% 
                        dplyr::select(ELEV,LEN,DistStr,inundation,LUresO), #insegnificant drop
                      method="glm", family="binomial",
                      trControl = ctrl)
cvFit

ggplot(as.data.frame(cvFit$resample), aes(Accuracy)) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Accuracy",
       y="Count")

## Map predictions

# of training set:

BothTrainNoG <- BothTrain %>%
  st_set_geometry(NULL)

trainingsetPred <- predict(cvFit, BothTrainNoG, type="prob")[,2]

TSWithPred <- 
  cbind(BothTrain,trainingsetPred) %>%
  mutate(trainingsetPred = round(trainingsetPred * 100))%>%
  mutate(predYN = ifelse(trainingsetPred == "0",0,1))%>%
  mutate(trueFalse = ifelse(inundation == "1" & predYN == "1","True_Positive",
                            ifelse(inundation == "1" & predYN == "0","False_Negative",
                                   ifelse(inundation == "0" & predYN == "1","False_Positive",
                                   "True_negative"))))
range(TSWithPred$trainingsetPred)

ggplot() + 
  geom_sf(data=TSWithPred, aes(fill=factor(trueFalse))) +
  scale_fill_manual(values = c("#b3cde3","#8c96c6","#8856a7","#810f7c"))+
  mapTheme() +
  labs(title="")

# all Clg:  

allPredictions <- 
  predict(cvFit, clgNoGeo, type="prob")[,2]

ClgWithPred <- 
  cbind(Clg,allPredictions) %>%
  mutate(allPredictions = round(allPredictions * 100))

ggplot() + 
  geom_sf(data=ClgWithPred, aes(fill=factor(ntile(allPredictions,5))), colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(ClgWithPred$allPredictions,
                                                 c(0.1,.2,.4,.6,.8),na.rm=T)),
                    name="Predicted\nProbabilities(%)\n(Quintile\nBreaks)") +
  mapTheme()

# I want to use a different approach here

##

## spokane predictions:

Spk <- st_read("C:/Users/ssmat/Desktop/CPLN675/Midterm/Spokane/fishnetCitySpk.shp")

colnames(Spk)

Spk <- Spk %>%
  dplyr::select(-OID_,-Shape_Leng,-Shape_Area,-LU1,-LU2,-LU3) %>%
  st_transform(crs=4326) 

Spk[is.na(Spk)] <- 0

# only work with thw unfitted, original model:

SpkPredictions2 <- 
  predict(ClgModel, SpkNoGeo, type="response")

SpkWithPred2 <- 
  cbind(Spk,SpkPredictions2) 

range(SpkWithPred2$SpkPredictions2)

ggplot() + 
  geom_sf(data=SpkWithPred2, aes(fill=factor(ntile(SpkPredictions2,5))), colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(SpkWithPred2$SpkPredictions2,
                                                 c(0.1,.2,.4,.6,.8),na.rm=T)),
                    name="Predicted\nProbabilities(%)\n(Quintile\nBreaks)") +
  mapTheme() +
  labs(title="")

# if we say anything above 0.3:

SpkWithPred3 <- SpkWithPred2 %>%
  mutate(predYN = ifelse(SpkPredictions2 > 0.3,1,0))

# our yes/no map:

ggplot() + 
  geom_sf(data=SpkWithPred3, aes(fill=factor(predYN))) +
  scale_fill_manual(values = c("#edf8fb","#810f7c"),
                    name="Yes/No with 0.3 threshold") +
  mapTheme() +
  labs(title="")

SpkWithPred3 <- SpkWithPred3 %>%
  mutate(trueFalse = ifelse(inundation == "1" & predYN == "1","True_Positive",
                            ifelse(inundation == "1" & predYN == "0","False_Negative",
                                   ifelse(inundation == "0" & predYN == "1","False_Positive",
                                          "True_negative"))))
  
ggplot() + 
  geom_sf(data=SpkWithPred3, aes(fill=factor(trueFalse))) +
  scale_fill_manual(values = c("#b3cde3","#8c96c6","#8856a7","#810f7c"))+
  mapTheme() +
  labs(title="")

# need to explain those two last plots of we want to use them. I think it can be nice..
