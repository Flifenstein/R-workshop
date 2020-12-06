## 2.3 Import the data in R
# Set the path of the folders
path_a <- "C:\\fakepath\\WhatsApp Chat with Ioana Frîncu\\Wouter Stoter" 
path_b <- "C:\\fakepath\\WhatsApp Chat with Ioana Frîncu\\Ioana Frîncu"

# Check content of the folders
dir(path_a)
dir(path_b)

#Import the content of the folders as a corpus
corpus_a <- VCorpus(DirSource(path_a))
corpus_b <- VCorpus(DirSource(path_b))

## 3.1 File list
summary(corpus_a)

## 3.2 File metadata
inspect(corpus_a[1:5])
inspect(corpus_a[[1]])

## 3.3 Read content
writeLines(as.character(corpus_a[[1]]))

## 3.4 Explore other corpus
summary(corpus_b) #View file list
inspect(corpus_b[1:5]) #Information about the first few files
inspect(corpus_b[[1]]) #The first text
writeLines(as.character(corpus_b[[1]])) #The plain text only

## 4.1 Stemming words
corpus_a <- corpus_a %>%
  tm_map(stemDocument)

## 4.2 Remove punctuation
corpus_a <- corpus_a %>%
  tm_map(removePunctuation)

## 4.3 Remove numbers
corpus_a <- corpus_a %>%
  tm_map(removeNumbers)

## 4.4 Change to lowercase
corpus_a <- corpus_a %>%
  tm_map(content_transformer(tolower))

## 4.5 Remove stopwords
corpus_a <- corpus_a %>%
  tm_map(removeWords, stopwords("english"))

## 4.6 Remove additional whitespace
corpus_a <- corpus_a %>%
  tm_map(stripWhitespace)

## 4.7 Remove special characters
corpus_a <- corpus_a %>%
  tm_map(content_transformer(iconv), from = "latin1", to = "ASCII", sub = "" )

# Check first document
writeLines(as.character(corpus_a[[1]]))

## 4.8 Clean other dataset
corpus_b <- corpus_b %>%
  tm_map(stemDocument) %>% #Stemming words
  tm_map(removePunctuation) %>% #Remove punctuation
  tm_map(removeNumbers) %>% #Remove numbers
  tm_map(content_transformer(tolower)) %>% #Change to lowercase
  tm_map(removeWords, stopwords("English")) %>% #Remove stopwords
  tm_map(content_transformer(iconv), from = "latin1", to = "ASCII", sub = "" ) %>% #Remove special characters
  tm_map(stripWhitespace) #Remove additional whitespace

# Check first document
writeLines(as.character(corpus_b[[1]]))

## 5.1 Create Document Term Matrix
dtm_a <- DocumentTermMatrix(corpus_a)

## 5.2 Count total amount of words
freqs_a <- dtm_a %>% 
  as.matrix() %>% 
  colSums()

## 5.3 Sort by frequency
freqs_a <- freqs_a %>% 
  sort(decreasing = TRUE)

## 5.4 Create frequency table
wf_a <- tibble(word = factor(names(freqs_a), names(freqs_a), ordered = TRUE), 
               freqs = freqs_a)

## 5.5 Other dataset
wf_b <- corpus_b %>%
  DocumentTermMatrix() %>% #Create Document Term Matrix
  as.matrix() %>% 
  colSums() %>% #Count total amount of words
  sort(decreasing = TRUE) %>% #Sort by frequency
  tibble(word = factor(names(.), names(.), ordered = TRUE), freqs = .) #Create frequency table

## 6.1 Create bar chart
wf_a %>% 
  top_n(20) %>%
  ggplot(aes(x = word, y = freqs)) + 
  geom_col() +
  coord_flip()

## 6.2 Create a wordcloud
wordcloud(wf_a$word, wf_a$freqs, max.words = 50)

## 6.3 Visualize other dataset
# Create bar chart
wf_b %>% 
  top_n(20) %>%
  ggplot(aes(x = freqs, y = word)) + 
  geom_col(fill = "blue")

# Create wordcloud
wordcloud(wf_b$word, wf_b$freqs, max.words = 50, color = rainbow(50))

## 7.1 Merge corpora
corpus <- c(corpus_a, corpus_b)

## 7.2 Count files
n_a <- length(corpus_a)
n_b <- length(corpus_b)

## 7.3 Add metadata
meta(corpus, "is_me") <- c(rep(TRUE, n_a), 
                           rep(FALSE, n_b))

## 8.1 Create a Document Term Matrix
dtm <- DocumentTermMatrix(corpus)

## 8.2 Check matrix
inspect(dtm)

## 8.3 Remove sparse terms
dtm <- dtm %>% 
  removeSparseTerms(0.95)

## 8.4 Convert to dataframe
dtm <- dtm %>% 
  as.matrix() %>% 
  as.data.frame()

## 8.5 Add metadata
dtm$is_me <- meta(corpus)$is_me

## 9.1 Determine test indices
test_indices <- sample( 1:nrow(dtm), nrow(dtm) * 0.25 )
test_indices

## 9.2 Create datasets
dtm_test <- dtm[test_indices, ]
dtm_training <- dtm[-test_indices, ]

## 10.1 Create tree
tree <- rpart( is_me ~ ., data = dtm_training, method = "class" )

## 10.2 Plot tree
rpart.plot(tree, type = 4, fallen = F)

## 10.3 Make predictions
prediction_tree <- predict(tree, newdata = dtm_test, type = "class")
prediction_tree <- prediction_tree  == "TRUE"

## 10.4 Judge accuracy
# Accuracy table
tibble(predicted = prediction_tree, truth = dtm_test$is_me) %>% 
  tabyl(predicted, truth) %>% 
  adorn_title("combined")
# Accuracy number
mean(prediction_tree == dtm_test$is_me)

## 11.1 Determine importance
head(tree$variable.importance, 2)

## 11.2 Create regression
logistic_regression <- glm(is_me ~ can + ill, 
                           data = dtm_training, family = binomial)
summary(logistic_regression)

## 11.3 Make predictions
prediction_glm <- predict(logistic_regression, newdata = dtm_test, type = "response")
prediction_glm <- prediction_glm > .5

## 11.4 Judge accuracy
tibble(predicted = prediction_glm, truth = dtm_test$is_me) %>% 
  tabyl(predicted, truth) %>% 
  adorn_title("combined")

## 12.1 Count words
wc <- corpus %>%
  DocumentTermMatrix() %>% 
  as.matrix() %>% 
  rowSums() %>%
  tibble(
    count = .,
    is_me = as.integer(meta(corpus)$is_me)
  )

## 12.2 Split test and train
wc_test <- wc[test_indices, ]
wc_training <- wc[-test_indices, ]

## 12.3 Create model
model <- wc_training %>% 
  glm(is_me ~ count, data = ., family = binomial)
tidy(model)

## 12.4 Visualize model
wc_training %>% 
  ggplot(aes(y = is_me, x = count)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F)

## 12.5 Judge accuracy
prediction_wc <- predict(model, newdata = wc_test, type = "response")
prediction_wc <- prediction_wc > .5

tibble(predicted = prediction_wc, truth = wc_test$is_me) %>% 
  tabyl(predicted, truth) %>% 
  adorn_title("combined")

mean(prediction_wc == wc_test$is_me)