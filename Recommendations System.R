##Recommendation System 

library(cluster)
library(dplyr)

class(games_by_domain$Domains)

book_feature = games_by_domain[1:10000,c("Complexity.Average","Play.Time","Domains")] 
book_feature$Domains = as.factor(book_feature$Domains)
book_feature

dissimilarity = daisy(book_feature, metric = "gower", weights = c(1,1,1))
dissimilarity_mat = as.matrix(dissimilarity)

row.names(dissimilarity_mat) <- games_by_domain$Name[1:10000]
colnames(dissimilarity_mat) <- games_by_domain$Name[1:10000]

dissimilarity_mat[15:20,15:20]

#now let's test the recommendation system

games = c(
  sample(games_by_domain$Name, 10, replace = FALSE, prob = NULL))

rating = c(
  sample(1:10, 10, replace = FALSE, prob = NULL))

user.rating = data.frame(games, rating)

selected_games = user.rating[ ,c("games", "rating")]

selected_game_indexes = which(colnames(dissimilarity_mat) %in% selected_games$games)

recomendar = function(selected_games, dissimilarity_mat, 
                      book_feature, n_recommendations = 5){
  
  selected_game_indexes = which(colnames(dissimilarity_mat) %in% selected_games$games)

  results = data.frame(dissimilarity_mat[, selected_game_indexes], 
                       recommended_game = row.names(dissimilarity_mat),
                       stringsAsFactors = FALSE) 
  recomendaciones = results %>%
    pivot_longer(cols = c(-"recommended_game") , names_to = "played_game", 
                 values_to = "dissimilarity") %>%
    left_join(selected_games, by = c("recommended_game" = "games"))%>%
    arrange(desc(dissimilarity)) %>%
    filter(recommended_game != played_game) %>%
    filter(!is.na(rating) ) %>%
    mutate(
      similarity = 1 - dissimilarity,
      weighted_score = similarity * rating) %>%
    arrange(desc(weighted_score)) %>%
    filter(weighted_score>0) %>%
    group_by(recommended_game) %>% slice(1) %>%
    top_n(n_recommendations, weighted_score)  %>%
    left_join(games_by_domain, by = c("recommended_game" = "Name"))
  
  return(recomendaciones)
}

recomendaciones = recomendar(selected_games, dissimilarity_mat, games_by_domain)
recomendaciones 

