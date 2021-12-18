####Loading the required libraries#######

library(readxl) ### reading the data into R
library(ggplot2) ### visualizing the data 
library(tidyverse) ### data structuring
library(dplyr) ###manipulating the data
library(magrittr) ###pipe operator
library(tidytext) ###for text mining
library(SnowballC) ###Stemming of words
library(caret) ###classification and data pre-processing 
library(rsample) ###re-sampling the data set
library(broom) ### cleaning the data
library(yardstick) ### evaluating the models
library(wordcloud2) ###creating the wordclouds
library(keras) ##developing the deep learning models
library(tensorflow) ### developing the deep learning models
use_condaenv("r-tensorflow")

##reading the data into R
amazon_clothes <- read_excel("WomensClothingReviews.xlsx")

##descriptive statistics of the dataset
summary(amazon_clothes)

##Renaming the column names
colnames(amazon_clothes) <- c('X1','ClothingID', 'Age', 'Title', 'Review', 'Rating', 'Recommend', 'LikedReview', 'Division', 'DepartmentName', 'Class')

####Data Pre-processing#######
##Checking for missing values in the dataset
sum(is.na(amazon_clothes))

##Checking for missing values in the Review column
sum(is.na(amazon_clothes$Review))

##Removing all the missing values from the dataset and saving it in the same dataset
amazon_clothes <- amazon_clothes %>%
  drop_na()

##Checking the missing values after removing them
sum(is.na(amazon_clothes))

###checking the dataset rows and columns after deleting the missing values
glimpse(amazon_clothes)

#####Explanatory Analysis ###############
#####Data Visualization######

##Density plot of Reviewer's Age
qplot(Age, data=amazon_clothes, geom="density", alpha=I(0.5), main="Reviewers' Age", xlab="Age", fill=I("lightblue"),  alpha = .3, show.legend = NA)

##Checking the distribution of reviewer's age with the mean
ggplot(amazon_clothes,aes(Age))+ 
  geom_histogram()+ 
  labs(title = "Distribution of Age with the mean", x = "Age")+
  geom_vline(xintercept = mean(amazon_clothes$Age), linetype = 2)+
  ggplot2::annotate("text", x = mean(amazon_clothes$Age), y = -150,
                    label = paste0("Avg. Age:", round(mean(amazon_clothes$Age), digits =2)), color="red")

##Checking the distribution of rating and it's mean
ggplot(amazon_clothes,aes(Rating))+ 
  geom_bar()+ 
  labs(title = "Distribution of Rating with the mean", x = "Rating")+
  geom_vline(xintercept = mean(amazon_clothes$Rating), linetype = 2)+
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))+
  ggplot2::annotate("text", x = mean(amazon_clothes$Rating), y = -1000, # I add where I want the Avg rating to be in the plot
                    label = paste0("Avg. rating:", round(mean(amazon_clothes$Rating), digits =2)),color="red") # Create the label with the text

##Distribution of Rating by Age
ggplot(amazon_clothes,aes(Age, fill = as.factor(Rating)))+ 
  geom_histogram()+
  labs(title = "Distribution of Rating by Age", x = "Age")+
  geom_vline(xintercept = mean(amazon_clothes$Age), linetype = 2)+ 
  ggplot2::annotate("text", x = mean(amazon_clothes$Age), y = -150, 
                    label = paste0("Avg Age:", round(mean(amazon_clothes$Age), digits =2)), color="red")

## Percentage of reviews by department
ggplot(data.frame(prop.table(table(amazon_clothes$DepartmentName))), aes(x=Var1, y = Freq*100)) + 
  geom_bar(stat = 'identity') + 
  xlab('Department Name') + ylab('Percentage of Reviews/Ratings (%)') + 
  geom_text(aes(label=round(Freq*100,2)), vjust=-0.25) + ggtitle('Percentage of Reviews By Department')

## Distribution of Percentage of Ratings by Department
rating_by_department <- amazon_clothes %>% filter(!is.na(DepartmentName), DepartmentName != 'Trend') %>% mutate(DepartmentName = factor(DepartmentName)) %>% group_by(DepartmentName) %>% count(Rating) %>% mutate(perc = n/sum(n))
rating_by_department %>% ggplot(aes(x=Rating, y = perc*100, fill = DepartmentName)) + geom_bar(stat = 'identity', show.legend = FALSE) + facet_wrap(~DepartmentName) + ylab('Percentage of reviews (%)') + geom_text(aes(label=round(perc*100,2)), vjust = -.2) + scale_y_continuous(limits = c(0,65)) 

##Distribution of Department by Age
department_by_age <-amazon_clothes%>% filter(!is.na(Age), !is.na(DepartmentName), DepartmentName != 'Trend') %>% select(ClothingID, Age, DepartmentName) %>% mutate(Age_group = ifelse(Age < 30, '18-29', ifelse(Age < 40, '30-39', ifelse(Age < 50, '40-49', ifelse(Age < 60, '50-59', ifelse(Age < 70, '60-69', ifelse(Age < 80, '70-79', ifelse(Age < 90, '80-89', '90-99')))))))) 

department_by_age<- department_by_age %>% mutate(Age_group = factor(Age_group), DepartmentName = factor(DepartmentName, levels = rev(c('Tops', 'Dresses', 'Bottoms', 'Intimate', 'Jackets'))))

department_by_age %>% filter(Age < 80) %>% group_by(Age_group) %>% count(DepartmentName) %>% ggplot(aes(DepartmentName, n, fill = Age_group)) + geom_bar(stat='identity', show.legend = FALSE) + facet_wrap(~Age_group, scales = 'free') + xlab('Department') + ylab('Number of Reviews') + geom_text(aes(label = n), hjust = 1) + scale_y_continuous(expand = c(.1, 0)) + coord_flip() 

##################Bigram Analysis##################
###defining the bigram function
bigramming <- function(data){
  nbigram <- data %>% unnest_tokens(bigram, Review, token = 'ngrams', n = 2)
  nbigram_AmazonClothes <- nbigram %>% separate(bigram, c('first', 'second'), sep = ' ')
  nbigram2 <- nbigram_AmazonClothes %>% filter(!first %in% stop_words$word, !second %in% stop_words$word, !str_detect(first,      '\\d'), !str_detect(second, '\\d')) %>% unite(bigram, c(first, second), sep = ' ') 
  return(nbigram2)
}

common_bigrams <- bigramming(amazon_clothes) %>% mutate(Rating = factor(Rating, levels <- c(5:1))) %>% mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% group_by(Rating) %>% count(bigram, sort=TRUE) %>% top_n(10, n) %>% ungroup() 

common_bigrams  %>% ggplot(aes(bigram, n, fill = Rating)) + geom_col(show.legend = FALSE) + facet_wrap(~Rating, ncol = 3, scales = 'free') + labs(x=NULL, y = 'frequency') + ggtitle('Most Common Bigrams By Ratings') + coord_flip()

#########Creating the Wordclouds for the highest and lowest rating#######
TopRating <- amazon_clothes %>% filter(Rating == 5)
LowRating <- amazon_clothes %>% filter(Rating == 1)

Top <- bigramming(TopRating) %>% count(bigram, sort = TRUE)
Low <- bigramming(LowRating) %>% count(bigram, sort = TRUE)

wordcloud2(Low %>% filter(n>5) %>% mutate(n = sqrt(n)), size = .5)

wordcloud2(Top %>% filter(n>10) %>% mutate(n = sqrt(n)), size = .5)

#######Modelling############
#####Structuring the dataset by using Bag-of-Words#####
####tokenizing the Reviews
clean_amazon <- amazon_clothes %>% 
  select(X1, Review) %>% 
  unnest_tokens(output = word, input = Review)

#####Anti joining stop words####
clean_amazon %<>%         
  anti_join(stop_words, by = "word") 

##for sorting the words in descending order and counting the frequency of the common words
clean_amazon %>%
  count(word, sort = TRUE) %>% 
  head(20)

###deleting all the alphanumeric characters
clean_amazon %<>%
  mutate(word = trimws(gsub("[^\\s]*[0-9][^\\s]*", "", word, perl = T))) %>%
  filter(str_length(word) > 1) ##removing the blank words
clean_amazon %<>%
  mutate(word = word %>% str_remove_all("[^[:alnum:]]") ) %>%  
  filter(str_length(word) > 1) ##removing the words with 1 character

##Stemming the words
clean_amazon %<>%
  mutate(word = wordStem(word))

########Adding the sentiment lexicon
Total_Words <- amazon_clothes %>% 
  group_by(X1) %>% 
  count() 

Senti_Words <- data_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(X1) %>%
  count(sentiment) %>%
  inner_join(Total_Words, by = "X1") %>%
  mutate(value = ifelse(sentiment == 'positive', n.x/n.y, -n.x/n.y))

Senti_Words <- Senti_Words %>%
  group_by(X1) %>%
  summarise(total_value = sum(value)) %>%
  inner_join(data, by = "X1")

Senti_Words <- Senti_Words %>%
  mutate(Sentiment = ifelse(total_value>0, "Positive", "Negative"))

count(Senti_Words, Senti_Words$Sentiment)

Senti_Words <- Senti_Words %>%
  mutate(label = ifelse(Sentiment == 'Positive', 1, 0))

##########Developing the Deep Leaning Models######
###Splitting the dataset into training and testing
set.seed(40317880) ##Student number as setting the seed
index <- createDataPartition(y = Senti_Words$label, p = 0.75, list = FALSE)
training <- Senti_Words[index,] 
test <- Senti_Words[-index,]

########Setting the parameters
maxlen <- 100 
max_features <- 10000 
batch_size <- 128
epochs <- 10
.
##Setting the tokenizer same as maximum features which is 10000
tokenizer_1 <- text_tokenizer(num_words = max_features)

tokenizer_training <- tokenizer_1 %>%
  fit_text_tokenizer(training$Review)

tokenizer_tesing <- tokenizer_1 %>%
  fit_text_tokenizer(test$Review)

##Padding the reviews
x_training <- tokenizer_training %>% 
  texts_to_sequences(training$Review) %>%
  pad_sequences(maxlen=maxlen)

y_training <- training$label 

x_testing <- tokenizer_testing %>% 
  texts_to_sequences(test$Review) %>%
  pad_sequences(maxlen=maxlen)

y_testing <- test$label 

##testing the same lenght for x and y
dim(x_training)
length(y_train)

#####Developing the Feedforward ANN Model
##Setting the tuning and architecture
set.seed(40317880)
model_FF_ANN <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(100)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid") %>% 
  compile( 
    loss = "binary_crossentropy", 
    optimizer = "rmsprop",
    metrics = "accuracy"
  )

###Summary of FF ANN model
summary(model_FF_ANN)

##Model Fitting
set.seed(40317880)
hist_FF_ANN <- model_FF_ANN %>%
  fit(
    x_training,
    y_training,
    batch_size = batch_size, 
    epochs = epochs, 
    validation_split=0.2 
  )

#####Model Evaluation
plot(hist_FF_ANN)

hist_FF_ANN$metrics$val_acc

results_FF_ANN <- model_FF_ANN %>% evaluate(x_testing, y_testing)
results_FF_ANN

###Confusion matrix for feedforward ANN Model##############
pred_model_FF_ANN <- predict(model_FF_ANN, x_testing)
confusionMatrix(table(pred_model_FF_ANN, y_testing))

############Developing the Recurrent Neural Network Model
##Setting the tuning and the architecture for RNN Model
##Adding an embedding layer for RNN

set.seed(40317880)
model_RNN <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 100, input_length=maxlen) %>%
  layer_simple_rnn(units = 32, activation = "tanh", dropout = 0.25) %>% 
  layer_dense(units = 1, activation = "sigmoid")  %>% 
  compile( 
    loss = "binary_crossentropy", 
    optimizer = "rmsprop",  
    metrics = "accuracy" 
  )

####Model Fit
set.seed(40317880)
hist <- model_RNN %>%
  fit(
    x_training,
    y_training,
    batch_size = batch_size,
    epochs = epochs,
    validation_split=0.2
  )
##Evaluation of RNN model
plot(hist)

hist$metrics$val_acc

results_RNN <- model_RNN %>% evaluate(x_testing, y_testing)
results_RNN

#####Confusion Matrix for RNN Model
pred_RNN_model <- predict_classes(model_RNN, x_testing)
confusionMatrix(table(pred_RNN_model, y_testing))

#######Developing the Long Short Term Memory Network Model#######
##Setting the architecture and tuning
set.seed(40317880)
LSTM_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 32) %>%
  layer_lstm(units = 500, dropout = 0.25, recurrent_dropout = 0.25, return_sequences = FALSE) %>%
  layer_dense(units = 1, activation = "sigmoid") %>% 
  compile( 
    loss = "binary_crossentropy", 
    optimizer = "rmsprop", 
    metrics = "accuracy" 
  )

##The Model Fit
set.seed(40317880)
hist_lstm <- LSTM_model %>%
  fit(
    x_training,
    y_training,
    batch_size = batch_size,
    epochs = 20, 
    validation_split=0.2
  )

##Model Evaluation
plot(hist_lstm)

LSTM_results <- LSTM_model %>% evaluate(x_testing, y_testing)
LSTM_results

hist_lstm$metrics$val_acc

######Confusion Matrix for LSTM Model
pred_lstm <- predict_classes(model_lstm, x_test)
confusionMatrix(table(pred_lstm, y_test))

