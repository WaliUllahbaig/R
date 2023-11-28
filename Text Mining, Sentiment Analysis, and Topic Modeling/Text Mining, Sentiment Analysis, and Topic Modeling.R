# Shah Wali Ullah Baig

install.packages("readxl")
install.packages("tidytext")
install.packages("dplyr")
install.packages("SnowballC")
install.packages("tm")
install.packages("udpipe")
install.packages("rmarkdown")
install.packages("textstem")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("gplots")
install.packages("stringr")
install.packages("textdata")
install.packages("topicmodels")
install.packages("reshape2")
install.packages("pals")
install.packages("lda")
install.packages("ldatuning")
install.packages("kableExtra")
install.packages("DT")
install.packages("flextable")
install.packages("remotes")
install.packages("ldatuning")
remotes::install_github("rlesur/klippy")


options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4)
library(ldatuning)
library(rmarkdown)
library(kableExtra) 
library(DT)
library(ldatuning)
library(flextable)
library(lda)
library(pals)
library(reshape2)
library(topicmodels)
library(textdata)
library(stringr)
library(gplots)
library(tm)
library(readxl)
library(dplyr)
library(tidytext)
library(SnowballC)
library(udpipe)
library(textstem)
library(wordcloud)
library(ggplot2)

data <- read.csv("MS4S09_CW_Data.csv")

# To examine the structure of the data frame and check that the variable types are correct.

str(data)

# To get summary statistics for each variable

summary(data)

# <------------------------------Task A – Text Mining-------------------------------->

# (1): •	Utilise techniques associated to text mining/cleaning.

# Tokenization:
# Tokenize review text into individual words

reviews_tokens <- data %>%
  select(Review.Text) %>%
  unnest_tokens(word, Review.Text)

reviews_tokens

# Stopword removal:
# Remove stopwords from tokenized text

reviews_tokens_nostop <- reviews_tokens %>%
  anti_join(stop_words)

reviews_tokens_nostop

# Stemming and lemmatization:
# Stem the tokenized text

reviews_tokens_stemmed <- reviews_tokens %>%
  mutate(word = stemDocument(word))
reviews_tokens_stemmed

# Lemmatization:
# Lemmatize the tokenized text

reviews_tokens_lemmatized <- reviews_tokens %>%
  mutate(word = lemmatize_words(word))

# (2): •	Provide appropriate and informative visualizations.

# Word cloud:
# Create a word cloud of the most frequent words in the review text

wordcloud(reviews_tokens_nostop$word, max.words = 100, random.order = FALSE)

# Bar chart:
# Create a bar chart of the distribution of ratings

ggplot(data, aes(x = Rating)) +
  geom_bar() +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Count")

# Stacked bar chart:
# Create a stacked bar chart of the distribution of ratings by product division

ggplot(data, aes(x = "Division Name", fill = factor(Rating))) +
  geom_bar() +
  labs(title = "Distribution of Ratings by Product Division", x = "Product Division", y = "Count") +
  scale_fill_discrete(name = "Rating")

# Scatter plot:
# Create a scatter plot of the relationship between age and positive feedback counts
ggplot(data, aes(x = Age, y = "Positive Feedback Count")) +
  geom_point(alpha = 0.5) +
  labs(title = "Relationship between Age and Positive Feedback Counts", x = "Age", y = "Positive Feedback Count")

# (3): •	Correct and efficient practices.

# When working with text data in R language, there are several best practices 
# we can follow to ensure that your analysis is correct and efficient:

# 1): Data cleaning
# 2): Tokenization
# 3): Text preprocessing 
# 4): Vectorization
# 5): Efficient use of memory 
# 6): Visualization
# 7): By following these best practices, you can ensure that your text analysis 
#     in R is correct, efficient, and informative.

# <--------------------------Task B – Sentiment Analysis ---------------------------->

reviews <- read.csv("MS4S09_CW_Data.csv")

# (1): •	Application of Sentiment Analysis techniques.

reviews$Review.Text <- str_remove_all(reviews$Review.Text, "[^[:alnum:] ]")
reviews$Review.Text <- str_to_lower(reviews$Review.Text)

reviews_tokenized <- reviews %>%
  unnest_tokens(word, Review.Text)

afinn <- get_sentiments("afinn")
# Setect 1 and press enter

reviews_sentiment <- reviews_tokenized %>%
  inner_join(afinn) %>%
  group_by(Clothing.ID) %>%
  summarize(sentiment_score = sum(value))

ggplot(reviews_sentiment, aes(x = Clothing.ID, y = sentiment_score)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Analysis by Clothing ID")

# (2): •	Sentiment-specific visualisations.

# Word Cloud

positive_words <- get_sentiments("bing") %>%
  filter(sentiment == "positive") %>%
  pull(word)

negative_words <- get_sentiments("bing") %>%
  filter(sentiment == "negative") %>%
  pull(word)

reviews_tokenized <- reviews %>%
  unnest_tokens(word, Review.Text)

positive_words_freq <- reviews_tokenized %>%
  filter(word %in% positive_words) %>%
  count(word, sort = TRUE)

negative_words_freq <- reviews_tokenized %>%
  filter(word %in% negative_words) %>%
  count(word, sort = TRUE)

wordcloud(positive_words_freq$word, positive_words_freq$n, scale=c(4,0.5), 
          colors = brewer.pal(8,"Dark2"), random.order = FALSE, rot.per = 0.2, 
          main = "Positive Word Cloud")

wordcloud(negative_words_freq$word, negative_words_freq$n, scale=c(4,0.5), 
          colors = brewer.pal(8,"Dark2"), random.order = FALSE, rot.per = 0.2, 
          main = "Negative Word Cloud")
# Bar Chart:

# Bar Chart:

reviews_sentiment <- reviews %>%
  cross_join(get_sentiments("afinn")) %>%
  group_by(Department.Name) %>%
  summarize(sentiment_score = mean(value))

ggplot(reviews_sentiment, aes(x = Department.Name, y = sentiment_score)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Sentiment Score by Department")

# Scatter Plot:


ggplot(reviews, aes(x = Age, y = Positive.Feedback.Count, color = Rating)) +
  geom_point() +
  labs(title = "Relationship between Age, Positive Feedback, and Rating")


# (3): •	Informative conclusions drawn from your findings.

# Here are some possible informative conclusions that could be drawn from the visualization findings:

# 1): Word Cloud:
#               Positive sentiment words in customer reviews tend to include words 
#               such as "love", "great", "Top" and "perfect".
#               Negative sentiment words in customer reviews tend to include words 
#               such as "loose", "fall", "worn" and "bust".
# 2): Bar Chart:
#               The Women's Clothing department has the highest average sentiment score, indicating 
#               that customers tend to have a more positive sentiment towards women's clothing products 
#               compared to other departments.
#               The Intimate department has the lowest average sentiment score, indicating that 
#               customers tend to have a less positive sentiment towards intimate products compared 
#               to other departments.
# 3): Scatter Plot:
#               There does not appear to be a clear relationship between customer age and positive 
#               feedback count.
#               Higher-rated products tend to have a higher positive feedback count, indicating 
#               that customers are more likely to leave positive feedback for products they rate highly.

# <---------------------------Task C – Topic Modelling ----------------------------->

# (1): •	Application of topic modelling/clustering techniques.

# Text Preprocessing:

# activate klippy for copy-to-clipboard button
klippy::klippy()
# Load data

mydata <- read.csv("MS4S09_CW_Data.csv", stringsAsFactors = FALSE)

textdata <- read.csv("MS4S09_CW_Data.csv", stringsAsFactors = FALSE)

textdata <- head(textdata , 1000)

# load stopwords
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
# Create a corpus
corpus <- Corpus(VectorSource(data$`Review Text`))

dtm <- DocumentTermMatrix(corpus, control = list(tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, stemming = TRUE))
dropped_docs <- which(rowSums(as.matrix(dtm)) == 0)

corpus <- subset(corpus, !(seq_along(corpus) %in% dropped_docs))

corpus <- head(corpus,1000)

dtm <- DocumentTermMatrix(corpus, control = list(tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, stemming = TRUE))

# Clean thhead()# Clean the text
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, english_stopwords)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(corpus, removeWords, stopwords("english"))
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM)

sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

DTM <- as.matrix(DTM)
DTM <- DocumentTermMatrix(corpus)

DTM <- as.matrix(DTM)
DTM <- DocumentTermMatrix(corpus)

result <- try(ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
) , silent=TRUE)

# (2): •	Topic/cluster-specific visualisations.

# Visualizations:

ggplot(data, aes(Age, Rating)) +
  geom_point(alpha = 1/2, size = 1.66) +
  geom_density_2d_filled(
    binwidth = 0.004
    ,size = 0.39 , color = "Red") +
  xlim(57, 82) +
  labs(x = "Age" ,y = "Rating") + theme_minimal()

ggplot(data, aes(Age, Rating, fill = Positive.Feedback.Count , color = Positive.Feedback.Count)) +
  geom_point(alpha = 1/2, size = 1.66) +
  geom_density_2d(
    binwidth = 0.004
    , size = 0.39) +
  xlim(57, 82) +
  labs(
    x = "Age"
    ,y = "Rating")

# (3): •	Informative conclusions drawn from your findings.

# From the cluster visualizations i drawn this conclusion that there are very less number of 
# peoples that rated 1 to products mostly peoples give 5 ratings and mainly those peoples who 
# are under the age of 65 will give 5 ratings.  

# <---------------------------Task D – Further exploration ----------------------------->

# Load text data
data <- read.csv("MS4S09_CW_Data.csv", stringsAsFactors = FALSE)
colnames(data)[2] ="text"

# Convert text to lowercase and remove punctuation
data$text <- tolower(data$text)
data$text <- gsub("[[:punct:]]", " ", data$text)

# Create a corpus and remove stop words
corpus <- VCorpus(VectorSource(data$text))
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Perform stemming
corpus <- tm_map(corpus, stemDocument)

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms
dtm <- removeSparseTerms(dtm, 0.95)

# Convert document-term matrix to a data frame
df <- as.data.frame(as.matrix(dtm))

# Add document labels
df$doc_id <- rownames(df)
df <- left_join(df, data, by = "doc_id")

# Create a word cloud
freq <- colSums(df[,1:ncol(df)-1])
wordcloud(names(freq), freq, max.words = 100, random.order = FALSE)

words_freq <- reviews_tokenized %>%
  count(word, sort = TRUE)

wordcloud(words_freq$word, words_freq$n, scale=c(4,0.5), 
          colors = brewer.pal(8,"Dark2"), random.order = FALSE, rot.per = 0.2, 
          main = "Positive Word Cloud")

