library(readr)
library(geojsonio)
library(knitr)
library(stringr)
library(dplyr)
list_det<-read.csv(choose.files(),header = TRUE,stringsAsFactors = FALSE)
lis_det_sel <- list_det[c('id', 'last_scraped', 'host_name', 'host_since', 'host_location', 'host_about', 'host_is_superhost', 'host_has_profile_pic', 'host_identity_verified', 'neighbourhood_cleansed', 'neighbourhood_group_cleansed', 'latitude', 'longitude', 'property_type', 'room_type', 'accommodates', 'bathrooms', 'bedrooms', 'beds', 'bed_type', 'amenities', 'price', 'security_deposit', 'cleaning_fee', 'guests_included', 'extra_people', 'minimum_nights', 'first_review', 'last_review', 'number_of_reviews', 'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin', 'review_scores_communication', 'review_scores_location', 'review_scores_value', 'instant_bookable', 'cancel_policy' = 'cancellation_policy', 'require_guest_profile_picture', 'require_guest_phone_verification', 'calculated_host_listings_count','review_scores_rating')]
lis_det_sel <- subset(lis_det_sel, number_of_reviews > 0)
lis_det_sel <- subset(lis_det_sel, review_scores_rating > 0)
lis_det_sel$host_since<-as.Date(lis_det_sel$host_since,format="%d-%m-%Y")
lis_det_sel$last_scraped<-as.Date(lis_det_sel$last_scraped,format ="%d-%m-%Y" )
lis_det_sel$first_review<-as.Date(lis_det_sel$first_review,format ="%d-%m-%Y" )
lis_det_sel$last_review<-as.Date(lis_det_sel$last_review,format ="%d-%m-%Y" )

lis_det_sel$review_scores_rating[is.na(lis_det_sel$review_scores_rating)]<-0
lis_det_sel$security_deposit[is.na(lis_det_sel$security_deposit)] <- 0
lis_det_sel$cleaning_fee[is.na(lis_det_sel$cleaning_fee)] <- 0
lis_det_sel$host_about[is.na(lis_det_sel$host_about)] <- ''
lis_det_sel <- subset(lis_det_sel, rowSums(is.na(lis_det_sel))==0)
lis_det_sel$host_is_superhost <- as.numeric(ifelse(lis_det_sel$host_is_superhost == 't', 1, 0))
lis_det_sel$host_has_profile_pic <- as.numeric(ifelse(lis_det_sel$host_has_profile_pic == 't', 1, 0))
lis_det_sel$host_identity_verified <- as.numeric(ifelse(lis_det_sel$host_identity_verified == 't', 1, 0))
lis_det_sel$price <- lis_det_sel$price %>% str_extract_all("\\(?[0-9,.]+\\)?") %>% gsub(",", "", .) %>% as.numeric()
lis_det_sel$security_deposit <- lis_det_sel$security_deposit  %>% str_extract_all("\\(?[0-9,.]+\\)?") %>% gsub(",", "", .) %>% as.numeric()
lis_det_sel$cleaning_fee <- lis_det_sel$cleaning_fee   %>% str_extract_all("\\(?[0-9,.]+\\)?") %>% gsub(",", "", .) %>% as.numeric()
lis_det_sel$extra_people <- lis_det_sel$extra_people   %>% str_extract_all("\\(?[0-9,.]+\\)?") %>% gsub(",", "", .) %>% as.numeric()
lis_det_sel$instant_bookable <- as.numeric(ifelse(lis_det_sel$instant_bookable == "t", 1, 0))
lis_det_sel$require_guest_profile_picture <- as.numeric(ifelse(lis_det_sel$require_guest_profile_picture == "t", 1, 0))
lis_det_sel$require_guest_phone_verification <- as.numeric(ifelse(lis_det_sel$require_guest_phone_verification == 't', 1, 0))
lis_det_sel <- lis_det_sel %>% 
  mutate(listing_duration = as.numeric(difftime(lis_det_sel$last_scraped,lis_det_sel$first_review, unit = 'days')),
         hosting_duration = as.numeric(difftime(lis_det_sel$last_scraped, lis_det_sel$host_since, unit = 'days')),
         host_local = as.numeric(str_detect(host_location, 'New York|NYC|New York,New York|Brooklyn|Bronx|Queens|Manhattan|Staten Island|new york')),
         host_about_len = ifelse(is.na(host_about), 0, nchar(host_about)),
         total_amenities = ifelse(nchar(amenities)>2, str_count(amenities, ',')+1, 0),
         price_per_person = price / accommodates)
k<-head(order(lis_det_sel$number_of_reviews,decreasing = T)[which(lis_det_sel$review_scores_rating[order(lis_det_sel$number_of_reviews,decreasing = T)]>90)],100)
lis_det_sel[k,"is_top_100"]=1
lis_det_sel[-k,"is_top_100"]=0
table(lis_det_sel$is_top_100)
lis_det_sel$neighbourhood_group_cleansed<-str_replace_all(lis_det_sel$neighbourhood_group_cleansed,"[^[:alnum:]]","_")
lis_det_sel$property_type <- str_replace_all(lis_det_sel$property_type, "[^[:alnum:]]", "_")
lis_det_sel$room_type <- str_replace_all(lis_det_sel$room_type, "[^[:alnum:]]", "_")
lis_det_sel$bed_type <- str_replace_all(lis_det_sel$bed_type, "[^[:alnum:]]", "_")
lis_det_sel$neighbourhood_group_cleansed <- str_replace_all(lis_det_sel$neighbourhood_group_cleansed, "[^[:alnum:]]", "_")
lis_det_sel$property_type <- str_replace_all(lis_det_sel$property_type, "[^[:alnum:]]", "_")
lis_det_sel$room_type <- str_replace_all(lis_det_sel$room_type, "[^[:alnum:]]", "_")
lis_det_sel$bed_type <- str_replace_all(lis_det_sel$bed_type, "[^[:alnum:]]", "_")

#Logistic regression 
InputData<-lis_det_sel
InputData$host_is_superhost<-as.factor(InputData$host_is_superhost)
InputData$host_has_profile_pic<-as.factor(InputData$host_has_profile_pic)
InputData$host_identity_verified<-as.factor(InputData$host_identity_verified)
InputData$property_type<-as.factor(InputData$property_type)
InputData$room_type<-as.factor(InputData$room_type)
InputData$bed_type<-as.factor(InputData$bed_type)
InputData$cancellation_policy<-as.factor(InputData$cancellation_policy)
InputData$require_guest_phone_verification<-as.factor(InputData$require_guest_phone_verification)
InputData$require_guest_profile_picture<-as.factor(InputData$require_guest_profile_picture)
InputData$host_local<-as.factor(InputData$host_local)
InputData$is_top_100<-as.factor(InputData$is_top_100)
InputData$instant_bookable<-as.factor(InputData$instant_bookable)
InputData$neighbourhood_group_cleansed<-as.factor((InputData$neighbourhood_group_cleansed))



data<- InputData[,-c(2,3,4,5,6,10,21,28,29)]
library(DMwR)
smoted<-SMOTE(is_top_100~.,data,perc.over = 300,perc.under = 300)
table(smoted$is_top_100)

train<-sample(nrow(smoted),0.7*nrow(smoted),replace = FALSE)
train1<-smoted[train,]
test1<-smoted[-train,]
table(train1$is_top_100)
table(test1$is_top_100)

logitM1<-glm(is_top_100~beds+require_guest_profile_picture+hosting_duration+require_guest_phone_verification+
               calculated_host_listings_count+bathrooms+review_scores_cleanliness+review_scores_value+
               cancellation_policy+bedrooms+review_scores_communication+review_scores_location+review_scores_accuracy+
               accommodates+host_local+review_scores_checkin+neighbourhood_group_cleansed+price+price_per_person+review_scores_rating,data = train1,family=binomial(link="logit"))
predicted<-predict(logitM1,test1,type = "response")
library(InformationValue)
optCutOff <- optimalCutoff(test1$is_top_100, predicted)[1] 
optCutOff
summary(logitM1) 
library(caret)
confusionMatrix(test1$is_top_100,predicted,threshold = optCutOff)
library(car)
vif(logitM1)  
misClassError(test1$is_top_100, predicted, threshold = optCutOff)
Concordance(test1$is_top_100, predicted)
sensitivity(test1$is_top_100, predicted, threshold = optCutOff)
specificity(test1 $is_top_100, predicted, threshold = optCutOff)
w<-ifelse(predicted>0.5,1,0)
w
table(w)
View(w)
test1$w<-w
View(test1)



#SMOTE
#train<- sample(nrow(InputData), 0.7*nrow(InputData), replace = FALSE)
#trainset<- InputData[train,]
#trainset1<- trainset[,-c(2,3,4,5,6,10,21,28,29)]
testset<- InputData[-train,]
testset1<- testset[,-c(2,3,4,5,6,10,21,28,29)]
table(trainset$is_top_100)
table(testset$is_top_100)
library(DMwR)
train1<- SMOTE(is_top_100~.,trainset1, perc.over = 300, perc.under = 300) 
table(train1$is_top_100)
test1<- SMOTE(is_top_100~.,testset1, perc.over = 300, perc.under = 300) 
table(test1$is_top_100)
View(train1$host_is_superhost)


#Information Value
library(tidyverse)
library(scorecard)

iv = iv(smoted, y = 'is_top_100') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv %>%
  knitr::kable()


#Logistic regression model with SMOTE and specific variables from IV

logitM1<-glm(is_top_100~beds+require_guest_profile_picture+hosting_duration+require_guest_phone_verification+
            calculated_host_listings_count+bathrooms+review_scores_cleanliness+review_scores_value+
            cancellation_policy+bedrooms+review_scores_communication+review_scores_location+review_scores_accuracy+
            accommodates+host_local+review_scores_checkin,data = train1,family=binomial(link="logit"))
predicted <- plogis(predict(logitM1, test1))  
library(InformationValue)
optCutOff <- optimalCutoff(test1$is_top_100, predicted)[1] 
optCutOff
summary(logitM1)   
confusionMatrix(test1$is_top_100,predicted,threshold = optCutOff)
library(car)
vif(logitM1)  
misClassError(test1$is_top_100, predicted, threshold = optCutOff)
Concordance(test1$is_top_100, predicted)
sensitivity(test1$is_top_100, predicted, threshold = optCutOff)
specificity(test1 $is_top_100, predicted, threshold = optCutOff)

?confusionMatrix


logitM2<-glm(is_top_100~beds+require_guest_profile_picture+hosting_duration+require_guest_phone_verification+
               calculated_host_listings_count+cancellation_policy+review_scores_communication+review_scores_location+review_scores_accuracy+
               accommodates+host_local+host_identity_verified+review_scores_checkin,data = train1,family=binomial(link="logit"))
predicted <- plogis(predict(logitM2, test1))  
library(InformationValue)
optCutOff <- optimalCutoff(test1$is_top_100, predicted)[1] 
optCutOff
summary(logitM2)   
confusionMatrix(test1$is_top_100,predicted,threshold = optCutOff)
library(car)
vif(logitM1)  
misClassError(test1$is_top_100, predicted, threshold = optCutOff)
Concordance(test1$is_top_100, predicted)
sensitivity(test1$is_top_100, predicted, threshold = optCutOff)
specificity(test1 $is_top_100, predicted, threshold = optCutOff)

#Decision Trees
dt_test$is_top_100<-as.factor(dt_test$is_top_100)

dt_train$is_top_100<-as.factor(dt_train$is_top_100)
install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)
install.packages("ROCR")
library(ROCR)
dt_model<-rpart(is_top_100~beds+require_guest_profile_picture+hosting_duration+require_guest_phone_verification+
                  calculated_host_listings_count+bathrooms+review_scores_cleanliness+review_scores_value+
                  cancellation_policy+bedrooms+review_scores_communication+review_scores_location+review_scores_accuracy+
                  accommodates+host_local+review_scores_checkin+neighbourhood_group_cleansed+price+price_per_person+review_scores_rating,data = train1,method = "class")
rpart.plot(dt_model,cex = 0.55)
prp(dt_model)
fancyRpartPlot(dt_model,type = 4)
?fancyRpartPlot()
library(randomForest)
plot(dt_model)
text(dt_model,pretty = 0)
install.packages("rattle")
library(rattle)
library(caret)
dt_prediction<-predict(dt_model,newdata = test1,type = "class")
confusionMatrix(test1$is_top_100,dt_prediction)
table<-table(test1$is_top_100,dt_prediction)
length(dt_prediction)
length(test1)
table
accuracy<-sum(diag(table))/sum(table)
accuracy

library(caret)
View(dt_prediction)
test1$dt_pred<-dt_prediction
View(test1)
install.packages()
library(caret)
dt_model1<-rpart(is_top_100~beds+require_guest_profile_picture+hosting_duration+require_guest_phone_verification+
                  calculated_host_listings_count+cancellation_policy+review_scores_communication+review_scores_location+review_scores_accuracy+
                  accommodates+host_local+host_identity_verified+review_scores_checkin,data = train1,method = "class")
rpart.plot(dt_model1,extra = 106)
dt_prediction1<-predict(dt_model1,newdata = test1,type = "class")
table1<-table(test1$is_top_100,dt_prediction1)
length(dt_prediction)
length(test1)
table1
accuracy1<-sum(diag(table1))/sum(table1)
accuracy1


#exporting the data-test1
library(xlsx)
write.csv(test1,"G:\\Data Science\\test1.csv")


names(lis_det_sel)
lis_det_clean <- lis_det_sel[, c(7:9, 11:11, 14:15, 17:20, 23:27, 31:49)]
#plots
discrete <- c("host_is_superhost", "host_has_profile_pic", "host_identity_verified", "instant_bookable", "require_guest_profile_picture", "require_guest_phone_verification", "host_local")

library(ggplot2)

for (colname in discrete) { 
  
  temp <- subset(lis_det_clean, is_top_100 == 1) 
  temp <- temp %>% 
    group_by(is_top_100, temp[,colname]) %>% 
    summarise(density = n()/nrow(.))
  colnames(temp)[2] <- colname
  
  temp1 <- subset(lis_det_clean, is_top_100 == 0) 
  temp1 <- temp1 %>% 
    group_by(is_top_100, temp1[,colname]) %>% 
    summarise(density = n()/nrow(.))
  colnames(temp1)[2] <- colname
  
  temp2 <- rbind(temp, temp1)
  
  plot <- ggplot(data=temp2, aes(x=as.factor(temp2[[colname]]), y=density, fill=as.factor(is_top_100))) + 
    geom_bar(position = 'dodge', stat='identity') + labs(fill = "is_top_100", x = colname, 
                                                         title = paste(colname, " relative density grouped by is_top_100")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(plot)
}
View(temp)


