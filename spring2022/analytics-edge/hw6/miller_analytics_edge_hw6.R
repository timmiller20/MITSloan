#-----------------------
# ANALYTICS EDGE
# HW6
# TIM MILLER
#-----------------------

## LOAD AND INSTALL PACKAGES ##

#install.packages('tm')
#install.packages('SnowballC')
#install.packages('ggcorrplot')
#install.packages('wordcloud2')
library(tm)
library(SnowballC)
library(caret)
library(rpart)
library(rpart.plot)
library(ggcorrplot)

##----- PROBLEM 1a -----##

# import AirBnB data
reviews = read.csv("airbnbReviews.csv", stringsAsFactors = FALSE, encoding="UTF-8")

#count number of each review score
table(reviews$review_scores_rating)

##----- PROBLEM 1b -----##

#Create new column and compute review character length
reviews$review_length = nchar(reviews$comments)

#Average length by review score
aggregate(reviews$review_length, list(reviews$review_scores_rating), mean)


##----- PROBLEM 1c -----##

# Create a corpus of the comments text
corpus = Corpus(VectorSource(reviews$comments)) # An array of document
corpus

# Clean the corpus
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeWords, c("airbnb","apartment","location","place","room","host","stay"))
corpus = tm_map(corpus, stemDocument)

# Review the text in document 1
strwrap(corpus[[1]])


##----- PROBLEM 1d -----##

# word clouds
library(wordcloud2)

# Positive reviews
matrix_pos = as.matrix(TermDocumentMatrix(
  corpus[reviews$review_scores_rating>3] ))
df_pos = data.frame(word = row.names(matrix_pos), freq=rowSums(matrix_pos))
wordcloud2(df_pos[order(df_pos$freq, decreasing=TRUE),])

# Negative reviews
matrix_neg = as.matrix(TermDocumentMatrix(
  corpus[reviews$review_scores_rating<=3] ))
df_neg = data.frame(word = row.names(matrix_neg), freq=rowSums(matrix_neg))
wordcloud2(df_neg[order(df_neg$freq, decreasing=TRUE),])

##----- PROBLEM 1e -----##

#APPROACH 1

#create matrix for word frequency for all words
matrix_count = as.matrix(TermDocumentMatrix(
  corpus[reviews$review_scores_rating]))
df_count = data.frame(word = row.names(matrix_count), freq=rowSums(matrix_count))
df_count_ordered = df_count[order(df_count$freq, decreasing=TRUE),]
df_count_ordered

# Positive reviews
matrix_pos = as.matrix(TermDocumentMatrix(
  corpus[reviews$review_scores_rating>3] ))
df_pos = data.frame(word = row.names(matrix_pos), freq=rowSums(matrix_pos))
df_pos_ordered = df_pos[order(df_pos$freq, decreasing=TRUE),]
df_pos_ordered

# Negative reviews
matrix_neg = as.matrix(TermDocumentMatrix(
  corpus[reviews$review_scores_rating<=3] ))
df_neg = data.frame(word = row.names(matrix_neg), freq=rowSums(matrix_neg))
df_neg_ordered = df_neg[order(df_neg$freq, decreasing=TRUE),]
df_neg_ordered

#APPROACH 2

# Calculate the frequency of each words over all reviews. 
frequencies = DocumentTermMatrix(corpus)
frequencies

findFreqTerms(frequencies, lowfreq=900)

##----- PROBLEM 1f -----##

# "Sparsify" the corpus and remove infrequent words. 
# Calculate the frequency of each words over all tweets. 
frequencies = DocumentTermMatrix(corpus)

# Let us only keep terms that appear in at least 1% of the tweets. We create a list of these words as follows. 
# 0.99: maximal allowed sparsity
# We now have 404 terms instead of 7040
sparse = removeSparseTerms(frequencies, 0.99)   
sparse 


##----- PROBLEM 1g -----##

# CART dataset
documentterms = as.data.frame(as.matrix(sparse))
documentterms$review_score = factor(reviews$review_scores_rating, ordered = TRUE, levels = 1:5)

# Training and test set.
split1 = (reviews$date < "2018-01-01")
split2 = (reviews$date >= "2018-01-01")
train = documentterms[split1,]
test = documentterms[split2,]

# Cross-validation for CART
RNGkind(sample.kind = "Rounding")
set.seed(17)
cv.trees = train(y = train$review_score,
                 x = subset(train, select=-c(review_score)),
                 method = "rpart", 
                 trControl = trainControl(method = "cv", number = 10),
                 tuneGrid = expand.grid(cp = seq(0.001,0.01,.0005)) )

plot(cv.trees$results$cp, cv.trees$results$Accuracy, type = "l", ylab = "Accuracy", xlab = "cp")

##----- PROBLEM 1h -----##

cart.mod = rpart(review_score ~ ., data = train, method = "class", cp=0.0030)
prp(cart.mod, type=2, extra=104, tweak = 1.1)

##----- PROBLEM 1i -----##

reviews[grepl("ask", reviews$comments), "comments"]


##----- PROBLEM 1j -----##

# Assessing the out-of-sample performance of the CART model, test set
predictions.cart <- predict(cart.mod, newdata=test, type="class")
matrix.cart = table(test$review_score, predictions.cart) # confusion matrix
accuracy.cart = (matrix.cart[1,1]
                 + matrix.cart[2,2] 
                 + matrix.cart[3,3] 
                 + matrix.cart[4,4] 
                 + matrix.cart[5,5])/nrow(test)
accuracy.cart


# Assessing the out-of-sample performance of the CART model, training set
predictions.cart <- predict(cart.mod, newdata=train, type="class")
matrix.cart = table(train$review_score, predictions.cart) # confusion matrix
accuracy.cart = (matrix.cart[1,1]
                 + matrix.cart[2,2] 
                 + matrix.cart[3,3] 
                 + matrix.cart[4,4] 
                 + matrix.cart[5,5])/nrow(train)
accuracy.cart

##----- PROBLEM 1k -----##

# Assessing the out-of-sample performance of the CART model, test set
predictions.cart <- predict(cart.mod, newdata=test, type="class")
matrix.cart = table(test$review_score, predictions.cart) # confusion matrix

# 1-star out of sample accuracy

accuracy.cart = (matrix.cart[1,1]) / nrow(test[test$review_score == "1", ])
accuracy.cart

# 2-star out of sample accuracy

accuracy.cart = (matrix.cart[2,2]) / nrow(test[test$review_score == "2", ])
accuracy.cart

# 3-star out of sample accuracy

accuracy.cart = (matrix.cart[3,3]) / nrow(test[test$review_score == "3", ])
accuracy.cart

# 4-star out of sample accuracy

accuracy.cart = (matrix.cart[4,4]) / nrow(test[test$review_score == "4", ])
accuracy.cart

# 5-star out of sample accuracy

accuracy.cart = (matrix.cart[5,5]) / nrow(test[test$review_score == "5", ])
accuracy.cart


##----- PROBLEM 2 -----##

# Packages
library(softImpute)
library(dplyr)
library(tidyr)
library(ggplot2)

# Reading the function file
source("functionsCF.R")

# Read data
songs <- read.csv("Songs.csv")
users <- read.csv("Users.csv")
music <- read.csv("MusicRatings.csv")

# Train/test split
set.seed(331)
training.rows <- cf.training.set(music$userID, music$songID, prop=0.92)
music.train <- music[training.rows,]
music.test <- music[-training.rows,]
mat.train <- Incomplete(music.train[,1], music.train[,2], music.train[,3])

# test value is correct
mean(music.train$rating)

# Center matrix
set.seed(138)
mat.scaled <- biScale(mat.train, maxit=1000, row.scale = FALSE, col.scale = FALSE)

music.train
mat.train

##----- PROBLEM 2A -----##

# Evaluate ranks
set.seed(789)
rank.info <- cf.evaluate.ranks(music.train, 0:7, prop.validate=0.05)

rank.info

plot(rank.info$rank, rank.info$r2, type = "l", ylab = "R^2", xlab = "rank")

##----- PROBLEM 2B -----##

# Fit collaborative model using center matrix
set.seed(157)
fit <- softImpute(mat.scaled, rank.max=2, lambda=0, maxit=1000)

# Make out-of-sample prediction
pred_outsample_0 <- impute(fit, music.test$userID, music.test$songID, unscale = TRUE)

# look at distribution
hist(pred_outsample_0)

#set min of 1 and max of 4
pred_outsample <- pmax(pmin(pred_outsample_0, 4), 1)

#check updated output and confirm bounds
hist(pred_outsample)

# Calculate osr^2
R2_outsample <- 1 - sum((pred_outsample-music.test$rating)^2)/sum((mean(music.test$rating) - music.test$rating)^2)
R2_outsample

##----- PROBLEM 2C -----##

# add score from archetype 1
songs$score1 = fit$v[,1]

# add score from archetype 2
songs$score2 = fit$v[,2]

# Score differences
songs$diff = songs$score1 - songs$score2

# Top 10 greatest negative difference
head(songs[order(songs$diff),], 10)

# Top 10 greatest positive difference
head(songs[order(-songs$diff),], 10)



##----- PROBLEM 2D -----##

# aggregate difference by artist
artist_diff = aggregate(songs$diff, by=list(songs$artist), FUN=sum) 

# aggregate by number of songs per artist
song_count = aggregate(songs$artist, by=list(songs$artist), FUN=length)


# filter for artists with 4 songs
songs_min = song_count[which(song_count[,2]>=4),]

#confirm correct filtering
songs_min[order(-songs_min$x),]

# filter for artists with 4 or more songs
artist_diff_2 = filter(artist_diff,
                       Group.1 %in% songs_min$Group.1)

# Top 5 greatest negative difference
head(artist_diff_2[order(artist_diff_2$x),], 5)

# Top 5 greatest positive difference
head(artist_diff_2[order(-artist_diff_2$x),], 5)


##----- PROBLEM 2E -----##

music.test
specific_user = music[which(music[,1]==1089),]
specific_user

specific_user <- impute(fit, music$userID == 1089, music$songID, unscale = TRUE)
?impute

music.test
pred_outsample



fit$d

head(fit$v %*% diag(fit$d))