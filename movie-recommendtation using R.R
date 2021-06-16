#MovieLens Recommendation

#library importing 
library(recommenderlab) #for recommendation
library(reshape2)
library(data.table)
library(ggplot2) #visualization
library(magrittr) 
library(dplyr)  
library(tidyr)
library(wordcloud)

#Retriveving the data
movie_data <- read.csv("D:/Submission/Sem 8/R/movie recommendation/IMDB-Dataset/movies.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("D:/Submission/Sem 8/R/movie recommendation/IMDB-Dataset/ratings.csv")

#structure
str(movie_data)
str(rating_data)

#tabuler view
data.table(movie_data)  #id,title,genres
data.table(rating_data)

#summary statistics
summary(movie_data)
summary(rating_data)


#-------------------------PREPROCESS-----------------------------------------------------------------------------------------------------

#we need to do some thing with genre more usefull
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], "[|]", type.convert = TRUE),stringsAsFactors = FALSE)


colnames(movie_genre2) <-c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_matl <- matrix(0,10330,18)
genre_matl[1,] <- list_genre
colnames(genre_matl) <- list_genre

for (index in 1:nrow(movie_genre2)){
  for(col in 1:ncol(movie_genre2)){
    gen_col = which(genre_matl[1,] == movie_genre2[index,col])
    genre_matl[index+1,gen_col] <- 1
  }
}


genre_mat2 <- as.data.frame(genre_matl[-1,], stringsAsFactors=FALSE)

for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

str(genre_mat2)


#head(movie_data)
#create a search matrix that gives us films based on genres

SearchMovie <- cbind(movie_data[,1:2],genre_mat2[])

head(SearchMovie)


#many movies have several genre 
#let's create sparse matrix for recommendation

ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE) #basically our sparse matrix
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")



#---------------------------------------visualization-----------------------------------------

#similarity matrix----------------------------------------------------------------------------

#let's check similarity
similarity_mat <- similarity(ratingMatrix[1:4, ],method = "pearson",which="users")

as.matrix(similarity_mat)

image(as.matrix(similarity_mat), main = "User's Similarity")

#let's check similarity of movies
movie_similarity<- similarity(ratingMatrix[ ,1:4],method = "pearson",which="items")

as.matrix(movie_similarity)

image(as.matrix(movie_similarity), main = "Movie Similarity")

#----------------------------------------------Histogram----------------------------------------------------


#histogram : number of ratings for each rating
group <-  ifelse((rating_data$rating == 1 |rating_data$rating == 2 | rating_data$rating == 3 | 
                    rating_data$rating == 4 | rating_data$rating == 5) ,
                 "full_star_rating", 
                 "half_star_rating") 

explore_ratings <- data.frame(rating_data$rating, group)

ggplot(explore_ratings, aes(x= rating_data.rating, fill = group)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  scale_fill_manual(values = c("half_star_rating"="purple", "full_star_rating"="brown")) +
  labs(x="rating", y="number of ratings") +
  ggtitle("Histogram : number of ratings for each rating")

#--------------------------------------wordcloud------------------------------------------------------
#Wordcloud
top_genr <- movie_data %>% tidyr::separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))



layout(matrix(c(1,2), nrow =2) , heights = c(1,4))
par(mar=rep(0,4))
plot.new()
text(x=0.5,y=0.5, "Top Genres by number of ratings")
wordcloud(words=top_genr$genres,freq=top_genr$count,min.freq=50,
          max.words = 20,random.order=FALSE,random.color=FALSE,
          rot.per=0.35,colors = brewer.pal(8,"Dark2"),scale=c(5,.2),
          family="plain",font=2,
          main = "Top genres by number of ratings")
#--------------------------------------------------------------------------------------------


#most viewed movies-------------------------------------------------------------------------

#most viewed movies visualization

movie_views <- colCounts(ratingMatrix) # count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # sort by number of views
table_views$title <- NA
for (index in 1:10325){
  table_views[index,3] <- as.character(subset(movie_data,
                                              movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,]

#plotting this data
ggplot(table_views[1:20, ], aes(x = title, y = views)) +
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0, 500)) +
  labs(x="", y="Number of views") +
  geom_text(aes(label= views), hjust=-0.1, size=3) +
  labs(title="Top 20 movies title based on total number of views" )

#Heatmap-------------------------------------------------------------------------------

#Heatmap of rating matrix

image(ratingMatrix[1:30,1:30], axes=FALSE, main = "Heatmap of the first 30 rows and 30 columns")




#lot's of sparse data

#now we will
#1.select usefull data  done
#2.normalize it
#3.binarize it

#you have seen the rating dataset ok so what do you think how many user needs to 
#rate a movie to be usefull let's say 50

#Heatmap------------------------------------------------------------------------------------

movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,colCounts(ratingMatrix) > 50]

movie_ratings

#this bunch of code finds a heatmap of top user and movies
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

#average rating per user---------------------------------------------------------------------

#what we need to do much ago distribution of average rating of users
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings) +
  ggtitle("Distribution of average rating per user") +
  geom_histogram(color = "white")

#Heatmap (Ratings of the Top Users)-----------------------------------------------------------------------------

#normalize data
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

#heatmap of normalized value
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")


#binarize means 0 and 1 we will recommend if rating of that movie 
# is greater than 3.5

#Heatmap of top users and movies-----------------------------------------------------------------------------


binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.90)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.90)


movies_watched <- binarize(movie_ratings, minRating = 1)


good_rated_films <- binarize(movie_ratings, minRating = 3.5)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")

#--------------------------------------------------------------------------------------------


#Collaborative Filtering System


sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]


#recommendation system
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

lapply(recommendation_system, "[[", "description")

recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model


class(recommen_model)

#recommendation system------------------------------------------------------------------------

#let's recommend

top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations



#let's see some of the names

user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                             movie_data$movieId == movies_user1[index])$title)
}
movies_user2


#recommendation matrix----------------------------------------------------------------------


recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
recommendation_matrix[,1:4]


number_of_items <- factor(table(recommendation_matrix))

chart_title <- "Distribution of the Number of Items for Item Based Collaborative Filtering"

qplot(number_of_items, fill=I("blue"), col=I("black")) + ggtitle(chart_title)