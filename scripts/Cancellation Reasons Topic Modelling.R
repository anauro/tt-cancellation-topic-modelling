#Program: Cancellation Survey Reasons - Topic Modelling
#Created date: May 2, 2024
#Last modified date: May 29, 2024
#Created by: Ana Urosevic

#GOAL: Extract meaningful topics from open-form cancellation survey responses
#Source: https://rpubs.com/Argaadya/topic_lda

###########################
# Set-up                  #
###########################

#Clear directory
rm(list = ls())

#Install packages
#install.packages('devtools')
#install.packages('bigrquery')
#install.packages('tidyverse')
#install.packages("readr")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("wordcloud2")
#install.packages("tm")
#install.packages("stringr")
#install.packages("tidytext")
#install.packages('topicmodels')
#install.packages('textclean')
#install.packages('textmineR')

#Load packages
library(tidyverse)
library(stringr)
library(readr)
library(bigrquery)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(tidytext)
library(topicmodels)
library(textclean)
library(textmineR)
library(ggplot2)


###########################
# STEP 1. BigQuery        #
###########################

#Create a function to connect to TT database and execute query
tt_query <- function(sql_str, dl_page_size = NULL) {
  #Run query and store in temp table, get table reference
  bq_table_temp <- bq_project_query(query = sql_str,
                                    x = 'tt-dp-prod',
                                    use_legacy_sql = FALSE)
  
  #Get size of table 
  temp_rows <- bq_table_nrow(bq_table_temp) 
  cat('N rows in table: ',temp_rows, '\n')
  
  #Download temporary table to local r session
  bq_table_download(bq_table_temp, page_size = dl_page_size) %>% as.data.frame() 
} 

#We want to focus on more recent trends
#cutoff <- today()-years(1)
start_date <- '2024-03-15' #Implementation of new survey reasons 
end_date <- '2024-05-29' #Analysis date

#All we need to do is grab the text for all cancellations with free responses
query <- paste0("select 
CAST(survey_response_pk as string) as survey_pk,
timestamp_millis(create_time) as date_created,
key as response,
value as free_form
from tt-dp-prod.surveys.in_product_survey_responses,
UNNEST(answers)
where key='SOMETHING_ELSE' and value!='true'
and timestamp_millis(create_time)>'",start_date,"'","and timestamp_millis(create_time)<='",end_date,"'")
print(query)
#key in ('customer-cancelled-questionnaire-reason-text','OTHER','SOMETHING_ELSE') and value!='true'

cancellations <- tt_query(query) 
#Yay it worked!! :) 
#9,810 free-form responses

#Are there multiple cancellations by customer? 
#test <- cancellations %>%
#  group_by(user_pk) %>%
#  summarize(count=n()) %>%
#  filter(count>1)
#Yes, there are a handful of customers who cancelled more than once


###########################
# STEP 2. Preprocessing   #
###########################

#Preprocess the data to make sure we have meaningful information 
preprocess <- cancellations %>%
  mutate(free_form_clean=free_form %>%
           replace_non_ascii() %>% #Remove emojis 
           str_replace_all("[0-9]", "") %>% #Replace numbers
           str_replace_all("[-|]", "") %>% #Replace "-" 
           replace_symbol() %>% #Remove all symbols
           replace_contraction() %>% #e.g., can't -> can not
           replace_word_elongation() %>%  #Shorten lengthened word - e.g., hiii -> hi
           str_replace_all("[[:punct:]]", "") %>% #Remove punctuation
           str_squish() %>% #Remove double whitespace
           str_trim() %>% #Remove whitespace at the start and end of the text
           tolower()
  ) %>%
  arrange(free_form_clean)

#Manual review of pre-processed text
#Remove meaningless responses:
#(1) NAs (e.g., n/a, na)
#(2) Missing responses (blanks or empty strings)
#(3) Variations of cancelling: cancel, canceling, cancelled
#(4) Other meaningless responses: other, thanks, thank you
#(5) Responses that are less than 3 characters (e.g., a, ccc)
delete_empty <- preprocess %>%
  select(survey_pk,free_form_clean) %>%
  rename(text=free_form_clean) %>%
  filter(!(text %in% c('na','none','null'))) %>%
  filter(text!='') %>%
  filter(!(text %in% c('cancel','cancel booking','canceling','cancelling','canceled','cancelled'))) %>%
  filter(!(text %in% c('other','thanks','thank you','thanks again','sorry'))) %>%
  filter(nchar(text)>3)
#8,921 usable responses 

#write.csv(delete_empty,"~/Downloads/clean_responses.csv", row.names = FALSE)

comment_length <- sapply(strsplit(delete_empty$text, " "), length)
comment_length %>% 
  summary()
#On average there are 7 words, median 5
#Relatively short responses will make this model difficult to implement

#Usually we would only keep documents with sufficient number of words, 
#but in our case even one-word answers are important (e.g., price, cost, etc.)


###########################
# STEP 3. Stopwords       #
###########################

#Remove words in English which are considered "filler words"
stopwords_default_list <- stopwords('en')

#Add additional words we may want to get rid of!
#TT-related terms that will appear often due to the nature of the comments
tt_related_terms <- c('sorry','thanks','cancel','work','book','booked','booking','thumbtack','job','service','work','pro','thank','customer')
#Other terms which may occur if customers do not spell correctly
other_terms <- c('im','know','done','still','shes','hes','just','get','didnt','wasnt','need','will','cant','wont','dont','want','doesnt','isnt')

stopwords_tt_terms <- append(stopwords_default_list,tt_related_terms)
stopwords_final <- append(stopwords_tt_terms,other_terms)
print(stopwords_final)
#Remove words which actually may be useful for our purposes 
#stopwords_final <- stopwords_final[!stopwords_final %in% c('no','not','cant')]

stop_words <- paste0("\\b", stopwords_final, "\\b", collapse="|")
#The \\b ensures that there are spaces between words - i.e., remove AN if "AN apple" but not if "cleAN"

#Note - this step takes a while because we're doing a lot of string cleaning
remove_stopwords <- delete_empty %>%
  #Replace stopwords with blanks
  mutate(text_clean=trimws(gsub(stop_words,"",text))) %>%
  #Fix double spaces between words
  mutate(text_clean=gsub("\\s+"," ",text_clean)) %>%
  select(survey_pk,text_clean) %>%
  #Remove any data that is now blank after removing all of the critical words 
  filter(text_clean!='') %>%
  arrange(text_clean)
#Looks good! 
#Dropped a few which were only made up of stopwords :) 
#8,709 remaining


###########################
# STEP 4. Word Cloud      #
###########################

#Let's create a word cloud now - this is the only way to visualize the trends/topics we're seeing
words <-  remove_stopwords %>%
  select(text_clean) %>%
  unnest_tokens(word, text_clean)
#Count frequency of word occurrence
word_count <- words %>% 
  count(word, sort=TRUE)

#Set seed for reproducibility
set.seed(123)

#Create word cloud
wordcloud(words = word_count$word,
          freq = word_count$n,
          min.freq = 10,
          max.words=100,
          scale=c(1,0.25),
          random.order=FALSE,
          rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))

#Alternative format
#wordcloud2(data=word_count, size=1.6, color='random-dark')


###########################
# STEP 5. Model input     #
###########################

#LDA=Latent Dirichlet Allocation (aka Topic Modelling; unsupervised ML)
#Allows us to understand the number of topics in a document 

#Transform data into document-term matrix (DTM)
#The value inside the matrix represents the term frequency or the number of terms appearing inside each document
model_input <- remove_stopwords

dtm <- remove_stopwords %>% 
  unnest_tokens(output = "word", input = text_clean) %>% 
  count(survey_pk,word) %>%
  cast_dtm(document = survey_pk, term = word, value = n) 

inspect(dtm)

#Remove rare words that occur only in less than 5 documents 
#and also the common words that appear in more than 95% of all documents
word_freq <- findFreqTerms(dtm, 
                           lowfreq = 5, 
                           highfreq = nrow(dtm)*0.95
)
dtm_final <- dtm[ , word_freq]
inspect(dtm_final)

dtm_lda <- Matrix::Matrix(as.matrix(dtm_final), sparse = T)


###########################
# STEP 6. # of topics     #
###########################

#We want to choose the number of topics that gives us the highest average coherence

#Maximum of up to 5 topics (more likely that optimal number is about 2 or 3)
#Intuitively we know that there aren't all that many reasons customers cancel
#Most will be scheduling, emergencies, price, searching for quotes, etc. 
k_list <- seq(2, 10) #We will never choose 1 topic only
temp <- data.frame(num_topics=0, coherence=0)

#Refit the data each time with a new number of topics and calculate coherence score 
iterations <- function() {
  for (i in k_list) {
    
    #Keep track ~
    print(i)
    
    m <- FitLdaModel(dtm = dtm_lda, 
                     k = i, 
                     iterations = 500, #Reduce number of iterations to speed up
                     burnin = 200, 
                     calc_coherence = T)
    
    results <- data.frame(
      num_topics = i,
      coherence=mean(m$coherence))
    
    temp <- rbind(temp,results)
  }
  return(temp)
}

optimize_num_topics <- iterations() %>%
  filter(num_topics!=0) %>%
  arrange(desc(coherence))
#4 topics seems to be the best choice


###########################
# STEP 7. LDA             #
###########################

#Set number of topics - based on analysis above (2 or 4 is optimal)
num_topics <- 4

#Run LDA model
lda <- FitLdaModel(dtm = dtm_lda, 
                        k = num_topics, 
                        iterations = 5000, #Increase iterations for the final version of the model
                        burnin = 4000, 
                        calc_coherence = T)


###########################
# STEP 8. Topics          #
###########################

#Identify probability of each topic for each document 
topic_probs <- lda$theta %>% 
  as.data.frame() %>% 
  set_names(paste("Topic", 1:num_topics)) %>% 
  rownames_to_column("document")

mycols <- c(paste("Topic", 1:num_topics))

prediction <- topic_probs %>% 
  rowwise() %>%
  mutate(probability_max = max(c_across(all_of(mycols)))) %>%
  #Identify which column has the maximum probability 
  mutate(row_max = paste0(mycols[c_across(all_of(mycols))==max(c_across(all_of(mycols)))],collapse='_'))

#If the model predicts equally across all topics, then delete
#If the model predicts multiple topics (without a clear winner) then also drop
prediction_clear <- prediction %>%
  filter(!grepl("_",row_max))

#Merge these back onto the original free-form text to see examples
prediction_examples <- cancellations %>%
  rename(document=survey_pk) %>%
  inner_join(prediction_clear) %>%
  arrange(row_max)
#WOW! Look at her go, she's doing so well at prediction :') It's doing just as well as I would've classified

#Great let's organize this
for_excel <- prediction_examples %>% 
  select(free_form,row_max,probability_max) %>%
  rename(response=free_form, topic_num=row_max, probability=probability_max) %>%
  mutate(topic=case_when(topic_num=='Topic 1' ~ "Pro no longer needed",
                         topic_num=='Topic 2' ~ "Rescheduling",
                         topic_num=='Topic 3' ~ "Pro changed job details",
                         topic_num=='Topic 4' ~ "Found another pro")) %>%
  select(-topic_num)
write.csv(for_excel,"~/Downloads/predicted_responses.csv", row.names = FALSE)

#Summarize proportion of topics!!! 
summary <- prediction_clear %>%
  group_by(row_max) %>%
  summarize(total=round(n()/nrow(prediction_clear)*100,2))

topic_words <- GetTopTerms(lda$phi, 5) %>% 
  as.data.frame() %>%
  set_names(paste("Topic", 1:num_topics))

