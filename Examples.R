# Example 1
library(quanteda)
load("/home/juan/Comments.RData")
my_corpus <- corpus(Comments$text)
mycorpus <- data.frame(summary(my_corpus, n = nrow(Comments)))
summary(my_corpus)

spanishstopwords <- c("q", stopwords("spanish"))
Restaurant <- dfm(corpus(
  my_corpus), 
  remove_numbers = TRUE, 
  remove = spanishstopwords, 
  stem = TRUE, remove_punct = TRUE) 
restaurant <- textstat_simil(Restaurant, 
                        margin = "documents", 
                        method = "jaccard")
restaurantdf <- data.frame(as.matrix(restaurant))
restaurantdf[is.na(restaurantdf)] = 0
restaurant <- data.frame(jaccard = restaurant[lower.tri(restaurant, diag = FALSE)])
library(clustertend)
set.seed(123)
hopkins(restaurantdf, n = nrow(restaurantdf)/10)
# Hopkins = 0.06922988

library(mclust)
fit <- Mclust(restaurantdf)
summary(fit)
clasificados <- data.frame(fit$classification)
names(clasificados)[1] <- "classification"
clasificados$Category <- "restaurant"

library(topicmodels)
restaurantC <- convert(Restaurant, to = "topicmodels", docvars = NULL)
pave <- LDA(restaurantC, k = 9)
library(tidytext)
restaurant_topics <- tidy(pave, matrix = "beta")
restaurant_topics$Category <- "restaurant"

library(broom)
EssentialWordsrestaurant <- tidy(pave)
library(dplyr)
topics <- EssentialWordsrestaurant %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

library(igraph)
g_rest <- graph.data.frame(topics, directed = FALSE)
bipartite.mapping(g_rest)
V(g_rest)$type <- bipartite_mapping(g_rest)$type
V(g_rest)$color <- ifelse(V(g_rest)$type, "lightblue", "red")
V(g_rest)$shape <- ifelse(V(g_rest)$type, "circle", "square")
E(g_rest)$color <- "black"
V(g_rest)$size <- eccentricity(g_rest) * 2.5
V(g_rest)$label.cex <- degree(g_rest) * 2.5
plot(g_rest, 
     vertex.label.cex = 0.8, 
     vertex.label.color = "black", 
     layout = layout_with_dh)

# Example 2 
load("/home/juan/SkillsOccupations.RData")
library(dplyr)
Scientists <- filter(skills, grepl("Scientist", Occupation))

library(igraph)
bn2 <- graph.data.frame(Scientists,directed=FALSE)
bipartite.mapping(bn2)

V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "lightblue", "yellow")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.8, 1.5)
V(bn2)$size <- (Scientists$Importance)/2
E(bn2)$color <- "black"
plot(bn2,
     vertex.label.color = "black",
     layout = layout_with_graphopt, main = "")

