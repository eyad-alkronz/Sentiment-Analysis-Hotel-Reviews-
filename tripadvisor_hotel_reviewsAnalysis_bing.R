
library(dslabs)
library(tidyverse)  
library(readxl)
library(lubridate)
library(tidytext)
library(tidyverse)
library(dplyr)



readData<- function(fileName){
  # check if the file exists
  if (file.exists(filename)) {
    data <- read_csv(filename , col_names = TRUE)
  }else{
    #read date from url
    url <-"https://raw.githubusercontent.com/eyad-alkronz/Sentiment-Analysis-Hotel-Reviews-/main/tripadvisor_hotel_reviews.csv"
    data <- read_csv(url , col_names = TRUE)
  }
  
  # this two lines of code to add id for each row , this id represent reviewID
  temp<- data.frame(id = seq(1,count(data)%>%pull()))
  data <- cbind(data, temp) 
  
  
  #this line to create new column newRating 
  # 1 ,2  equal zero  ,0 represent Negative
  # 3 , 4 , 5 equal 1 ,1 represent positive
  data <- data %>%
    mutate(newRating = ifelse(Rating <= 2   , 0 ,1))
 
  
  data
}


# this function has data.frme as input and Split a column into tokens
tokens <- function(data){
  data %>% 
    unnest_tokens(word, Review )  %>%  
    filter(!word %in% stop_words$word) 
}
 
  # prepareData  
  # read file in CSV format
  filename <- "tripadvisor_hotel_reviews.csv"
  data <- readData(filename)
 
  
  Review_words <- tokens (data) 
  
  
  #calculate world count for each statment
  SentenceWithWordCount <- Review_words %>%
    group_by(id) %>%
    summarize( WordCount = n()) 
  
  data <- data %>% left_join(SentenceWithWordCount  , by="id")
  
 

 

#groub by rate Plot
data %>%  
  ggplot(aes(x= Rating ))+
  geom_bar(fill="deepskyblue")  


#groub by new rate Plot
data %>%  
  ggplot(aes(x= newRating ))+
  geom_bar( fill="deepskyblue" )  



#plot relation between newRating and WordCount
data%>% group_by(newRating) %>% 
  summarize(meanCount = mean(WordCount)) %>%
  ggplot(aes(x = newRating , y = meanCount   ))+
  geom_point() +
  geom_line()  
#Insight
#Higher Rated Reviews tend to have less words while, lower rated reviews have very high word count



#Sentiment Analysis
#In sentiment analysis we assign a word to one or more "sentiment". 
bing <- get_sentiments("bing") %>%
  select(word, sentiment)%>%
  mutate(value = ifelse(sentiment == "negative" , 0 , 1) )   




#assign value for each words to its value that offered by bing
Review_words <- Review_words %>%
  left_join(bing, by = "word")  

 
#i want to replace all NA values to average of statment values for each statment 
Review_words <- Review_words %>% group_by(id) %>%
  mutate(value = ifelse(is.na(value), mean(value , na.rm = TRUE),value  ))



Review_words <-  Review_words %>%
  mutate(value = ifelse(is.nan(value), 0.5,value  ))


#Now we want to calculate value for eachsentences
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
 