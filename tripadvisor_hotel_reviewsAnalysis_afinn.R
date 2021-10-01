
library(dslabs)
library(tidyverse)    # includes readr
library(readxl)
library(lubridate)
library(tidytext)
library(tidyverse)
library(dplyr)

# read file in CSV format
filename <- "tripadvisor_hotel_reviews.csv"
# check if the file exists
if (file.exists(filename)) {
  data <- read_csv(filename , col_names = TRUE)
}else{
  #read date from url
  url <-"https://raw.githubusercontent.com/eyad-alkronz/Sentiment-Analysis-Hotel-Reviews-/main/tripadvisor_hotel_reviews.csv"
  data <- read_csv(url , col_names = TRUE)
}



temp<- data.frame(id = seq(1,count(data)%>%pull()))
data <- cbind(data, temp) 

#groub by rate
data %>%  
  ggplot(aes(x= Rating ))+
  geom_bar(fill="deepskyblue")  

data <- data %>%
mutate(newRating = ifelse(Rating <= 2   , 0 ,
                                 ifelse(Rating >=3  , 1 , Rating)))  
  
#0 represent Negative 
#1 represent positive

data%>% group_by(newRating) %>% summarize(count = n()) 
 
 
data %>%  
  ggplot(aes(x= newRating ))+
  geom_bar( fill="deepskyblue" )  


Review_words <- data %>% 
  unnest_tokens(word, Review )  %>%  
  filter(!word %in% stop_words$word   & 
           !str_detect(word, "^\\d+$")) 


#calculate world count for each statment

#plot rating vs wordCount
SentenceWithWordCount <- Review_words %>% group_by(id) %>%
  summarize( WordCount = n()) 

data <- data %>% left_join(SentenceWithWordCount  , by="id")

data%>% group_by(newRating) %>% 
  summarize(meanCount = mean(WordCount)) %>%
  ggplot(aes(x = newRating , y = meanCount   ))+
  geom_point() +
   geom_line()  
 #Insight
#Higher Rated Reviews tend to have less words while, lower rated reviews have very high word count



#Sentiment Analysis
#In sentiment analysis we assign a word to one or more "sentiment". 

#to show words count by sentiments that offers by afinn
get_sentiments("afinn")%>%count(value)


#to convert value from [-5,5] range to [0,5] to be more suitable for out anlysis
afinn <- get_sentiments("afinn") %>%
  select(word, value)%>%
   mutate(value = ifelse(value < 0 , 0 , value) )  %>%
   mutate(value = ifelse(value > 0 , 1 , value) ) 


#New form of afinn
afinn%>%count(value)

 

#assign value for each words to its value that offered by afinn
Review_words <- Review_words %>%
  left_join(afinn, by = "word")  

#Now , at this moment there are alot of words without value 
#because afinn does not have value for all words 


#i want to replace all NA values to average of statment values for each statment 
 Review_words <- Review_words %>% group_by(id) %>%
   mutate(value = ifelse(is.na(value), mean(value , na.rm = TRUE),value  ))

 

Review_words <-  Review_words %>%
  mutate(value = ifelse(is.nan(value), 0.5,value  ))

#Now we want to calculate value for each
# result = average of values for all words in this sentence
predicted_rating_per_sentences <-  Review_words%>%
  group_by(id) %>% 
  summarize(result = (mean(value)))


#plot histogram for predicted_rating_per_sentences
predicted_rating_per_sentences %>% ggplot(aes((result))) +
  geom_histogram()

 
predicted_rating_per_sentences <- predicted_rating_per_sentences%>% 
  mutate(result  = round(result))

predicted_rating_per_sentences%>% 
  ggplot(aes((result))) +
  geom_bar()




#join predicted_rating_per_sentences with original Data 
# calculate differ for enhancement pupose 
# diff value must be near to zero 
data <- predicted_rating_per_sentences %>% left_join(data ,by = "id")%>%
  mutate(differ = result - newRating ) 

#plot histogram to show differ 
data %>% ggplot(aes((differ))) +
  geom_histogram()
 

data %>% ggplot(aes(result)) +
  geom_bar()


#calculate accurecy 
successCount <- data %>% filter(differ == 0) %>% count()%>%pull
failCout<- data %>% filter(differ != 0) %>% count()%>%pull
totalCount <- data%>%count()%>%pull

accuracy <- successCount / totalCount
print(accuracy)
 



#Enhancy accuracy
data%>%filter(differ!=0) %>% group_by(Rating , newRating , result) %>% summarize(count = n())
