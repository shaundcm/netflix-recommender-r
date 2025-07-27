netflix_data = read.csv("D://R Codes//netflix_titles.csv")
user_data = read.csv("D://R Codes//random_user_movies_updated.csv")

user_id = 3

x = unlist(strsplit(user_data$movie_ids[user_id], split = ","))
p = netflix_data$listed_in[match(x, netflix_data$movie_id)]

genres = ""
for (i in p) {
  genres = paste(genres, i, sep = ",")
}
genres = unlist(strsplit(genres, split = ","))[-1]
top_g = names(sort(table(genres), decreasing = T)[1:5])

recent_g = ""
for (i in rev(p)[1:3]) {
  recent_g = paste(recent_g, i, sep = ",")
}
recent_g = unlist(strsplit(recent_g, split = ","))[-1]

rec_g = union(top_g, recent_g)

ratings = netflix_data$rating[match(x, netflix_data$movie_id)]
avg_rating = mean(ratings)

runtimes = netflix_data$duration[match(x, netflix_data$movie_id)]
avg_runtime = mean(runtimes)

years = netflix_data$release_year[match(x, netflix_data$movie_id)]
top_years = as.numeric(names(sort(table(years), decreasing = T)[1:5]))

content = netflix_data$content_rating[match(x, netflix_data$movie_id)]
pref_content = names(sort(table(content), decreasing = T)[1:5])

# Calculate variance for each feature

# Function to calculate variance for categorical features (e.g., genres)
variance_categorical <- function(user_data_column) {
  freq_table = table(user_data_column)
  return(1 / var(as.numeric(freq_table)))
}

# Variance for numerical features
variance_numerical <- function(user_data_column) {
  return(var(user_data_column))
}

# Calculate variance for each feature based on user data
genre_variance = variance_categorical(genres)
rating_variance = variance_numerical(ratings)
runtime_variance = variance_numerical(runtimes)
year_variance = variance_numerical(years)
content_rating_variance = variance_categorical(content)

# Normalize variances using min-max normalization
variances = c(genre_variance, rating_variance, runtime_variance, year_variance, content_rating_variance)
normalized_variances = 1 / variances

# Assign weights based on normalized variance
weights = normalized_variances * 100 / sum(normalized_variances)
print(weights)

# Assign weights to each feature
w_g = weights[1]  # Genre weight
w_r = weights[2]  # Rating weight
w_t = weights[3]  # Runtime weight
w_y = weights[4]  # Year weight
w_c = weights[5]  # Content Rating weight

# Normalize rating function
normalize_rating <- function(movie_rating, avg_rating, max_rating = 10) {
  return(1 - abs(movie_rating - avg_rating) / (max_rating - 1))
}

# Normalize runtime function
normalize_runtime <- function(movie_runtime, avg_runtime, max_runtime_diff = max(netflix_data$duration) - min(netflix_data$duration)) {
  return(1 - abs(movie_runtime - avg_runtime) / max_runtime_diff)
}

# Normalize release year function
normalize_year <- function(movie_year, top_years, year_range = max(netflix_data$release_year) - min(netflix_data$release_year)) {
  return(max(0, 1 - min(abs(rep(movie_year, length(top_years)) - top_years)) / year_range))
}

genre_similarity <- function(movie_genres, top_genres) {
  movie_genres <- unlist(strsplit(movie_genres, ","))
  return(length(intersect(movie_genres, top_genres)) / length(top_genres))
}

# Binary content rating similarity
content_similarity <- function(movie_content, pref_content) {
  return(match(movie_content, pref_content) / length(pref_content))
}

# Compute similarity score for each movie
compute_similarity_score <- function(movie, avg_rating, avg_runtime, top_genres, top_years, pref_content) {
  rating_score = normalize_rating(movie$rating, avg_rating)
  runtime_score = normalize_runtime(movie$duration, avg_runtime)
  year_score = normalize_year(movie$release_year, top_years)
  
  genre_score = c()
  content_score = c()
  for (i in 1:nrow(movie)) {
    genre_score = c(genre_score, genre_similarity(movie$listed_in[i], top_genres))
    content_score = c(content_score, content_similarity(movie$content_rating[i], pref_content))
  }
  
  # Final similarity score
  similarity_score = w_g * genre_score + w_r * rating_score + w_t * runtime_score + w_y * year_score + w_c * content_score
  return(similarity_score)
}

# Apply the similarity score to all movies
netflix_data$similarity_score = compute_similarity_score(netflix_data, avg_rating, avg_runtime, rec_g, top_years, pref_content)
rec_mov = netflix_data[order(-netflix_data$similarity_score), ][1:10, "title"]

#Using confidence intervals
confidence_interval <- function(data, confidence_level = 0.95) {
  n <- length(data)
  mean_val <- mean(data, na.rm = TRUE)
  std_dev <- sd(data, na.rm = TRUE)
  stderr <- std_dev / sqrt(n)
  z_value <- qnorm((1 + confidence_level) / 2)
  
  # Confidence Interval
  lower_bound <- mean_val - z_value * stderr
  upper_bound <- mean_val + z_value * stderr
  ci_width <- upper_bound - lower_bound
  
  return(list(lower_bound = lower_bound, upper_bound = upper_bound, ci_width = ci_width, range_vals = max(data) - min(data)))
}

# Calculate confidence intervals for each feature
rating_ci <- confidence_interval(ratings)
runtime_ci <- confidence_interval(runtimes)
year_ci <- confidence_interval(years)

# Assign weights inversely proportional to CI width
ci_widths <- c(rating_ci$ci_width, runtime_ci$ci_width, year_ci$ci_width)
weights1 <- c(rating_ci$range_vals/rating_ci$ci_width, runtime_ci$range_vals/runtime_ci$ci_width, year_ci$range_vals/year_ci$ci_width, w_g, w_c)
weights1 <- weights1 * 100 / sum(weights1)  # Normalize weights to sum to 100

w_r = weights1[1]  # Weight for rating
w_t = weights1[2]  # Weight for runtime
w_y = weights1[3]  # Weight for release year
w_g = weights1[4]  # Weight for genre
w_c = weights1[5]  # Weight for content rating

# Function to compute similarity score based on confidence intervals
compute_similarity_score_ci <- function(movie, rating_ci, runtime_ci, year_ci, genres, content) {
  # Check if the movie's features fall within the user's confidence interval for each feature
  rating_score = c()
  runtime_score = c()
  year_score = c()
  genre_score = c()
  content_score = c()
  for (i in 1:nrow(movie)) {
    rating_score = c(rating_score, min(movie$rating[i] - rating_ci$lower_bound, rating_ci$upper_bound - movie$rating[i]))
    runtime_score = c(runtime_score, min(movie$duration[i] - runtime_ci$lower_bound, runtime_ci$upper_bound - movie$duration[i]))
    year_score = c(year_score, min(movie$release_year[i] - year_ci$lower_bound, year_ci$upper_bound - movie$release_year[i]))
    genre_score = genre_similarity(movie$listed_in[i], genres)
    content_score = content_similarity(movie$content_rating[i], content)
  }
  
  # Calculate final similarity score using weights
  similarity_score = w_r * rating_score + w_t * runtime_score + w_y * year_score + w_g * genre_score + w_c * content_score
  
  return(similarity_score)
}

netflix_data$similarity_score_ci = compute_similarity_score_ci(netflix_data, rating_ci, runtime_ci, year_ci, rec_g, pref_content)
rec_mov_ci = netflix_data[order(-netflix_data$similarity_score_ci), ][1:10, "title"]

cat("Recommended movies for user", user_id, "\nUsing variance:\n")
print(rec_mov)
cat("\nUsing confidence interval:\n")
print(rec_mov_ci)

movs = c()
for (i in 1:nrow(user_data)) {
  movs = c(movs, unlist(strsplit(user_data$movie_ids[i], ",")))
}
mov_titles = netflix_data$title[match(movs, netflix_data$movie_id)]
pop_movs = names(sort(table(mov_titles), decreasing = T))[1:50]
cat("\nThe 50 most popular movies among users are:\n")
print(pop_movs)

# Assuming 'genres' is a vector of the user's preferred genres
library(ggplot2)
library(plotrix)
# Create a table of genres
genre_count <- table(genres)
genre = names(genre_count)
count = as.numeric(genre_count)

piepercent<- round(100 * count / sum(count), 1)

# Plot the chart.
pie3D(count, labels = piepercent, labelcex = 0.5,
      main = "Genres", col = rainbow(length(count)))
legend("topright", genre,
       cex = 0.5, fill = rainbow(length(count)))

hist(ratings, breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
