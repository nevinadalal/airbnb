install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("choroplethrZip")
install.packages("GGally")
install.packages("lubridate")
install.packages("zoo")
install.packages("scales")
install.packages("ggmap")
install.packages("scales")
install.packages("stringr")
install.packages("zipcode")
install.packages("leaflet")
install.packages("extracat")
install.packages("gridExtra")
install.packages("devtools")
install.packages("jsonlite")
library(devtools)
install_github('arilamstein/choroplethrZip@v1.4.0')
update.packages()

?choroplethr
library(readr)
library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(GGally)
library(lubridate)
library(zoo)
library(scales)
library(ggmap)
library(scales)
library(stringr)
library(zipcode)
library(leaflet)
library(extracat)
library(gridExtra)
library(jsonlite)
listings=read.csv(choose.files())
listingdf1<- listings
read.csv("C:\\Users\\Sharvari Gokhale\\Downloads\\listings.csv (1)\\Newlistings.csv")


missingdata<-listingdf1
missingdata[missingdata == ""] <- NA
#missingdata<-select(missingdata$host_is_superhost,missingdata$neighbourhood_group_cleansed,missingdata$host_response_time,missingdata$name,missingdata$host_since,missingdata$zipcode,missingdata$review_scores_rating)

missingdata<- missingdata %>% select(host_is_superhost,neighbourhood_group_cleansed,host_response_time,name,host_since,zipcode,review_scores_rating)
missingdata<-missingdata%>% rename(!!"host is superhost":=host_is_superhost,!!"neighbourhood":=neighbourhood_group_cleansed,!!"response time":=host_response_time,!!"listing name":=name,!!"zipcode":=zipcode,!!"review scores rating":=review_scores_rating)

visna(missingdata,sort = "c",mar.col = c("#e06f69","#357b8a"))


#EDA
listingdf<-listing
leaflet(listingdf) %>%
  addMarkers(~longitude, ~latitude,labelOptions = labelOptions(noHide = F),
             clusterOptions = markerClusterOptions(),popup = paste0("<b> Name: </b>",
            listingdf$name , "<br/><b> Host Name: </b>", listingdf$host_name, "<br> <b> Price: </b>",
            listingdf$price, "<br/><b> Room Type: </b>", listingdf$room_type, "<br/><b> Property Type: </b>",
            listingdf$property_type)) %>% 
  setView(74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron")

zipReviews <- listings %>% group_by(zipcode = zipcode) %>% summarise(avg_loc_review = mean(review_scores_location, na.rm = TRUE))
colnames(zipReviews) <- c("region","value")
zipReviews$region <- as.character(zipReviews$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)



#g_locations <- zip_choropleth(zipReviews,
                              #county_zoom = nyc_fips,
                              #title = "Location Review Scores by Region",
                              #legend = "Average Score") + ggtitle("Which area is the best?",
                               #                                   subtitle = "Map showing Average Location Score by Area") +
  #theme(plot.title = element_text(face = "bold")) +
  #theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  #theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  #scale_fill_brewer("Location Review Score",palette=3)
#\g_locations

propertydf <-  listingdf %>% group_by(neighbourhood_group_cleansed, property_type) %>% summarize(Freq = n())
propertydf <- propertydf %>% filter(property_type %in% c("Apartment","House","Condominium","Townhouse", "Loft"))

totalproperty<-  listingdf %>% filter(property_type %in%
                                        c("Apartment","House","Condominium","Townhouse", "Loft"))%>% 
  group_by(neighbourhood_group_cleansed) %>% summarize(sum = n())

propertyratio <- merge(propertydf, totalproperty, by="neighbourhood_group_cleansed")
propertyratio <- propertyratio %>% mutate(ratio = Freq/sum)

ggplot(propertyratio, aes(x=neighbourhood_group_cleansed, y=ratio, fill = property_type)) +
  geom_bar(position = "dodge",stat="identity") + xlab("Borough") + ylab("Count")+
  scale_fill_discrete(name = "Property Type") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Which types of Listings are there in NYC?",
          subtitle = "Map showing Count of Listing Type by Borough ") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458")) +
  xlab("Neighborhood") + ylab("Percentage")



#Demand and Price Analysis
##Data for past bookings is not available so reviews is considered as indicator of demand
##acc to airbnb 50% users review will give good estimate of demand##
reviews1 = read.csv("C:/Users/Sharvari Gokhale/Desktop/review1.csv")
reviews2 <- read.csv("C:\\Users\\Sharvari Gokhale\\Downloads\\reviews2.csv\\reviews2.csv")
reviews <- read.csv(choose.files())
#### How popular has Airbnb become in New York City?

reviewsNum <- reviews %>%
  group_by(date = reviews$date) %>% 
  summarise(number = n())
reviewsNum <- reviews %>% group_by(date = reviews$date) %>% summarise(number = n())


ggplot(reviewsNum, aes(date,number)) +
  geom_point(na.rm=TRUE, color = "#007A87", alpha=0.5) +geom_smooth(color = "#FF5A5F")+ 
  ggtitle("How popular is Airbnb?",
          subtitle = "Number of Reviews across years") +
  labs(x = "Year", y = "Unique listings recieving reviews") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))


ggplot(reviewsNum, aes(date, number)) +
  geom_point(na.rm=TRUE, color = "#007A87", alpha=0.5) +geom_smooth(color = "#FF5A5F")+
  ggtitle("How popular is Airbnb?",
          subtitle = "Number of Reviews across years") +
  labs(x = "Year", y = "Unique listings recieving reviews") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))



#monthly demands for each of the years
ggplot(reviewsNum[year(reviewsNum$date) == 2016,], aes(date, number)) +
  geom_point(na.rm=TRUE, color = "#007A87", alpha=0.5) +geom_smooth(color = "#FF5A5F")+
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2016") +
  labs(x = "Month", y = "Unique listings recieving reviews") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))

ggplot(reviewsNum[year(reviewsNum$date) == 2017,], aes(date, number)) +
  geom_point(na.rm=TRUE, color = "#007A87", alpha=0.5) +geom_smooth(color = "#FF5A5F")+
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2017") +
  labs(x = "Month", y = "Unique listings recieving reviews") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))

ggplot(reviewsNum[year(reviewsNum$date) == 2018,], aes(date, number)) +
  geom_point(na.rm=TRUE, color = "#007A87", alpha=0.5) +geom_smooth(color = "#FF5A5F")+
  ggtitle("Seasonality in Demand",
          subtitle = "Number of Reviews across Months in 2018") +
  labs(x = "Month", y = "Unique listings recieving reviews") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))

#### How is Airbnb priced across the year?

calendar2016 <- read.csv("C:\\Users\\Sharvari Gokhale\\Downloads\\calendar2016.csv\\calendar2016.csv")
calendar2017 <- read.csv("C:\\Users\\Sharvari Gokhale\\Downloads\\calendar2017.csv\\calendar2017.csv")
calendar2018 <- read.csv("C:\\Users\\Sharvari Gokhale\\Downloads\\calendar.csv\\calendar.csv")

combinedCalendar <- read.csv(choose.file)


combinedCalendar$price <- as.numeric(gsub(",", "", substring(combinedCalendar$price, 2)))
groupedCalendarAll <- combinedCalendar %>% group_by(date = date) %>%
  summarise(averagePrice = mean(price, na.rm = TRUE)) %>%
  mutate(year = year(date), commonYear = paste("2016",substring(date, 6),sep="-"))

groupedCalendarAll$year <- as.factor(as.character(groupedCalendarAll$year))
groupedCalendarAll$commonYear <- ymd(groupedCalendarAll$commonYear)

ggplot(groupedCalendarAll[year(groupedCalendarAll$date) >= 2017 & year(groupedCalendarAll$date) < 2019,],
       aes(commonYear, averagePrice)) +
  geom_point(na.rm=TRUE, alpha=0.5, color = "#007A87") +
  geom_smooth(color = "#FF5A5F")+ facet_grid(~year)+
  ggtitle("Seasonality in Price", subtitle = "Average listing price across Months") +
  labs(x = "Month", y = "Average price across Listings") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68")) + scale_x_date(labels = date_format("%b"))



#box plot of average prices by day of the week
 
groupedCalendarAll <- groupedCalendarAll %>% mutate(day = strftime(date,'%A'))
groupedCalendarAll$day <- factor(groupedCalendarAll$day, 
                      levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), 
                      labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(groupedCalendarAll, aes(x = factor(day), 
                               y = averagePrice)) + 
  geom_boxplot(fill = "#FF5A5F", color = "#565A5C") + 
  geom_jitter(alpha = 0.05, width = 0.1, color = "#007A87") +
  ggtitle("Is it expensive to travel on weekends?",
          subtitle = "Boxplots of Price by Day of the Week") +
  labs(x = "Day of the week", y = "Average Price") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))


#### Occupancy Rate by Month 

calendar <- calendar %>% mutate(booked = ifelse(available=="f", 1, 0))
groupedCalendar <- calendar %>% group_by(date = date) %>%
  summarise(totalBooked = sum(booked, na.rm = TRUE), totalListings = n()) %>%
  mutate(percent_booked = (totalBooked/totalListings)*100)
calendarHeat(groupedCalendar$date, groupedCalendar$percent_booked,
             ncolors = 99, color = "g2r", varname="Occupancy (Percentage) by Month")

sampledreviews <- read_csv("C:\\Users\\Sharvari Gokhale\\Downloads\\sampledreviews.csv\\sampledreviews.csv")

splitsampledreviewscoloumn <- unlist(strsplit(as.character(sampledreviews$reviews), split=" "))
reviewsWordDF <- data.frame("word" = splitsampledreviewscoloumn)
wordDF <- reviewsWordDF %>% count(word, sort = TRUE) %>% 
  ungroup()

splitlistingsdfcoloumn <- unlist(strsplit(as.character(listingdf$amenities), split=","))
reviewsWordDF1 <- data.frame("word" = splitlistingsdfcoloumn)
wordDF1 <- reviewsWordDF1 %>% count(word, sort = TRUE) %>% ungroup()

splitlistingsdfcoloumn1 <- unlist(strsplit(as.character(listingdf$summary), split=" "))
summaryWordDF1 <- data.frame("word" = splitlistingsdfcoloumn1)
summarywordDF1 <- summaryWordDF1 %>% count(word, sort = TRUE) %>% ungroup()
###User Review (Textual Data) Mining

install.packages("tm")
library(tm)
docs <- Corpus(VectorSource(splitlistingsdfcoloumn))#? 
docs <- tm_map(docs, content_transformer(tolower))#?
docs <- tm_map(docs, removeWords, stopwords("english"))#?
docs = tm_map(docs,removePunctuation)#?
docs <- tm_map(docs, removeWords,
        c("we","it", "he", "this", "i", "the", "apartment","de", "un","us","well","es","5","la","2","")) #?

newcorpusdf <- data.frame(text=sapply(docs, identity), 
                          stringsAsFactors=F)
newcorpusdffiltered <- newcorpusdf %>% filter(text != "")
wordDF1 <- newcorpusdffiltered %>% count(text, sort = TRUE) %>% 
  ungroup()


library(RColorBrewer)
install.packages("wordcloud")
library(wordcloud)
set.seed(789)
wordcloud(words = wordDF1$text, 
          freq = wordDF1$n,
          min.freq = 50000,
          max.words=500, colors = c("#e06f69","#357b8a", "#7db5b8", "#59c6f3"))





#Data Cleaning
listingdfcleaned<-read.csv("C:\\Users\\Sharvari Gokhale\\Downloads\\listings.csv (1)\\Newlistings.csv")
head(listingdf1)
colnames(listingdfcleaned)
#removing the variables
listingdfcleaned<-within(listingdfcleaned,rm(reviews_per_month,calculated_host_listings_count,require_guest_phone_verification,require_guest_profile_picture,is_business_travel_ready,jurisdiction_names,license,requires_license,calendar_last_scraped,monthly_price,weekly_price,square_feet,neighbourhood,host_has_profile_pic,host_picture_url,host_thumbnail_url,host_acceptance_rate,xl_picture_url,medium_url,thumbnail_url,experiences_offered,space,summary,last_scraped,scrape_id))
colnames(listingdfcleaned)
is.na(listingdfcleaned)
summary(listingdfcleaned)
sum(is.na(listingdfcleaned[,]))

names(listingdfcleaned)

#missing values
for (i in colnames(listingdfcleaned)) {
  missing <- sum(is.na(listingdfcleaned[,i]))
  if (missing > 0) {
    print(c(i,missing))
  }
}

colSums(is.na(listingdf1))
colSums(is.na(listingdfcleaned))

#replacing missing values by mean
listingdfcleaned$bathrooms[is.na(listingdfcleaned$bathrooms)] <- mean(listingdfcleaned$bathrooms, na.rm = TRUE)
listingdfcleaned$bedrooms[is.na(listingdfcleaned$bedrooms)] <- mean(listingdfcleaned$bedrooms, na.rm = TRUE)
listingdfcleaned$beds[is.na(listingdfcleaned$beds)] <- mean(listingdfcleaned$beds, na.rm = TRUE)
listingdfcleaned$review_scores_rating[is.na(listingdfcleaned$review_scores_rating)] <- mean(listingdfcleaned$review_scores_rating, na.rm = TRUE)
listingdfcleaned$review_scores_accuracy[is.na(listingdfcleaned$review_scores_accuracy)] <- mean(listingdfcleaned$review_scores_accuracy, na.rm = TRUE)
listingdfcleaned$review_scores_cleanliness[is.na(listingdfcleaned$review_scores_cleanliness)] <- mean(listingdfcleaned$review_scores_cleanliness, na.rm = TRUE)
listingdfcleaned$review_scores_checkin[is.na(listingdfcleaned$review_scores_checkin)] <- mean(listingdfcleaned$review_scores_checkin, na.rm = TRUE)
listingdfcleaned$review_scores_communication[is.na(listingdfcleaned$review_scores_communication)] <- mean(listingdfcleaned$review_scores_communication, na.rm = TRUE)
listingdfcleaned$review_scores_location[is.na(listingdfcleaned$review_scores_location)] <- mean(listingdfcleaned$review_scores_location, na.rm = TRUE)
listingdfcleaned$review_scores_value[is.na(listingdfcleaned$review_scores_value)] <- mean(listingdfcleaned$review_scores_value, na.rm = TRUE)


View(listingdfcleaned)


doc1<-"Our best guests are seeking a safe, clean, spare room in a family apartment.  They are comfortable being independent, accommodating of family noise (quiet hours 11pm-7am), and aren't afraid of a friendly two year old golden lab (dog).  Our guests aren't put off by an old bathroom that while perfectly clean, has some peeling paint.  In short, our guests want to feel like they are staying at their sister's apartment while visiting the city! (only their sister changed the sheets and cleaned)."
doc2<-"Updated, modern 3 bedroom, 2 bathroom apartment. Very spacious room including large closet, private bathroom and shower."
doc3<-"This bright & sunny bedroom is in a shared 3 bedroom, 2 bath flat in a quiet neighborhood 7 minutes from the subway. The room has two large southern-facing windows facing the park across the street. Includes a full size bed, tv, closet, desk & chair."
doc4<-"Private room, dedicated bath and a separate entrance. Totally renovated Queen Anne Historic Townhouse Home on a safe, Landmark Block. Only one stop from Midtown Manhattan! Enjoy the rear private Garden at breakfast time. Beautiful tree lined,quiet street. 4 subway lines ( E, M, 7 and G ) are one block away! Great restaurants, supermarket and cafes nearby."
doc5<-"This is a floor of our four-story brownstone on a residential street in Ft. Greene, Brooklyn. We live in the home but it's private, w/own entrance. Quiet and clean. You'll have a bedroom, bathroom, living room, kitchenette (no cooking facilities but microwave, fridge, sink, etc.). Garden access. There's a big TV and internet. Note: Bathroom is en suite, so additional guests would have to walk through the bedroom to use it. Temperature tends toward cool and many fans but no air conditioning."
doc.list<-list(doc1,doc2,doc3,doc4)
N.docs <- length(doc.list)
names(doc.list) <- paste0("doc", c(1:N.docs))
query<-"private family apartment"
library(tm)
my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")

my.corpus <- Corpus(my.docs)
my.corpus
getTransformations()
my.corpus1 <- tm_map(my.corpus, removePunctuation)
my.corpus$doc1
my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, tolower)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus$doc1
doc1
term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
inspect(term.doc.matrix.stm[0:14, ])
my.corpus <- tm_map(my.corpus, stemDocument)

as.String(listingdf1$summary)
class(listingdf1$summary)

