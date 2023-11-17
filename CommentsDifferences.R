# First, we upload our raw data set.
load("Comments.RData")
# Second, we will use the quanteda package
# for generating the corpus and conducting
# usual preprocessing such as removing
# stopwords, and incorporate metadata
# that associates the category of the restaurant
# the commercial name of the provider
# and the quantitative rating of each comment
library(quanteda)
my_corpus <- corpus(UserComments$text)
mycorpus <- data.frame(summary(my_corpus, n = nrow(UserComments)))
summary(my_corpus)
docvars(my_corpus, "Category") <- UserComments$Category
docvars(my_corpus, "Provider") <- UserComments$Provider
docvars(my_corpus, "Rating") <- UserComments$Rating
# Let's include in our initial dataset
# the number of different words per comment (Types),
# the total number of words per comment (Tokens)
# and the number of sentences (Sentences)
UserComments$Types <- mycorpus$Types
UserComments$Tokens <- mycorpus$Tokens
UserComments$Sentences <- mycorpus$Sentences


# Here, we see that we have six categories
unique(UserComments$Provider)
# ...and here, we see the distribution of discrete ratings
Ratings <- table(UserComments$Rating)
barplot(Ratings, main="Distribution of Ratings", 
        xlab="Rating value")

# In third place, we create a similarity matrix for 
# each restaurant category. In these matrices we basically
# estimate the Jaccard index for pairwise comparison
# of comments.

spanishstopwords <- c("q", stopwords("spanish"))
library(dplyr)
Asian = UserComments %>% filter(., Category == "Asian")
MCAsian <- corpus(Asian$text)
class(MCAsian)

# I introduced here some modifications that make this code work with 
# recent quanteda updates
library(quanteda)
CommentsAsian <- tokens(MCAsian) 
CommentsAsian <- dfm(CommentsAsian)
library(quanteda.textstats)
Asian <- textstat_simil(CommentsAsian, margin = "documents", method = "jaccard")
Asiandf <- data.frame(as.matrix(Asian))
Asian <- data.frame(jaccard = Asian[lower.tri(Asian, diag = FALSE)])

# In fourth place, we applied
# a Gaussian finite mixture model fitted by EM algorithm
library(mclust)
fit <- Mclust(Asiandf)
summary(fit)
clasificados <- data.frame(fit$classification)
names(clasificados)[1] <- "classification"
clasificados$Category <- "Asian"

CommentsBurgers <- dfm(corpus_subset(my_corpus, Category == "Burgers"), remove = spanishstopwords, stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE) 
Burgers <- textstat_simil(CommentsBurgers, margin = "documents", method = "jaccard")
Burgersdf <- data.frame(as.matrix(Burgers))
Burgers <- data.frame(jaccard = Burgers[lower.tri(Burgers, diag = FALSE)])
fit2 <- Mclust(Burgersdf)
summary(fit2)
clasificados2 <- data.frame(fit2$classification)
names(clasificados2)[1] <- "classification"
clasificados2$Category <- "Burgers"

CommentsChicken <- dfm(corpus_subset(my_corpus, Category == "Chicken"), remove = spanishstopwords, stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)
Chicken <- textstat_simil(CommentsChicken, margin = "documents", method = "jaccard")
Chickendf <- data.frame(as.matrix(Chicken))
Chicken <- data.frame(jaccard = Chicken[lower.tri(Chicken, diag = FALSE)])
fit3 <- Mclust(Chickendf)
summary(fit3)
clasificados3 <- data.frame(fit3$classification)
names(clasificados3)[1] <- "classification"
clasificados3$Category <- "Chicken"

CommentsBeverage <- dfm(corpus_subset(my_corpus, Category == "Alcoholic Beverages"), remove = spanishstopwords, stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)
Beverage <- textstat_simil(CommentsBeverage, margin = "documents", method = "jaccard")
Beveragedf <- data.frame(as.matrix(Beverage))
Beverage <- data.frame(jaccard = Beverage[lower.tri(Beverage, diag = FALSE)])
fit4 <- Mclust(Beveragedf)
summary(fit4)
clasificados4 <- data.frame(fit4$classification)
names(clasificados4)[1] <- "classification"
clasificados4$Category <- "Beverages"

CommentsMeat <- dfm(corpus_subset(my_corpus, Category == "Meat"), remove = spanishstopwords, stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)
Meat <- textstat_simil(CommentsMeat, margin = "documents", method = "jaccard")
Meatdf <- data.frame(as.matrix(Meat))
Meat <- data.frame(jaccard = Meat[lower.tri(Meat, diag = FALSE)])
fit5 <- Mclust(Meatdf)
summary(fit5)
clasificados5 <- data.frame(fit5$classification)
names(clasificados5)[1] <- "classification"
clasificados5$Category <- "Meat"

CommentsPizza <- dfm(corpus_subset(my_corpus, Category == "Pizzas"), remove = spanishstopwords, stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)
Pizza <- textstat_simil(CommentsPizza, margin = "documents", method = "jaccard")
Pizzadf <- data.frame(as.matrix(Pizza))
Pizza <- data.frame(jaccard = Pizza[lower.tri(Pizza, diag = FALSE)])
fit6 <- Mclust(Pizzadf)
summary(fit6)
clasificados6 <- data.frame(fit6$classification)
names(clasificados6)[1] <- "classification"
clasificados6$Category <- "Pizza"

classification <- do.call("rbind", list(clasificados, clasificados2, clasificados3, clasificados4, clasificados5, clasificados6))
UserComments$classification <- classification$classification
table(UserComments$classification)

boxplot(UserComments$Rating ~ UserComments$classification)

# As we systematically obtained a total of nine clusters that best 
# represent the hidden topics of customers' comments, we now
# turn our attention to identifying the membership of single
# words to each of these clusters.

library(topicmodels)
AsianC <- convert(CommentsAsian, to = "topicmodels", docvars = NULL)
pave <- LDA(AsianC, k = 9)
library(tidytext)
Asian_topics <- tidy(pave, matrix = "beta")
Asian_topics$Category <- "Asian"


BurgersC <- convert(CommentsBurgers, to = "topicmodels", docvars = NULL)
pave2 <- LDA(BurgersC, k = 9)
Burger_topics <- tidy(pave2, matrix = "beta")
Burger_topics$Category <- "Burgers"


ChickenC <- convert(CommentsChicken, to = "topicmodels", docvars = NULL)
pave3 <- LDA(ChickenC, k = 9)
Chicken_topics <- tidy(pave3, matrix = "beta")
Chicken_topics$Category <- "Chicken"

MeatC <- convert(CommentsMeat, to = "topicmodels", docvars = NULL)
pave4 <- LDA(MeatC, k = 9)
Meat_topics <- tidy(pave4, matrix = "beta")
Meat_topics$Category <- "Meat"


BeverageC <- convert(CommentsBeverage, to = "topicmodels", docvars = NULL)
pave5 <- LDA(BeverageC, k = 9)
Beverage_topics <- tidy(pave5, matrix = "beta")
Beverage_topics$Category <- "Beverage"

PizzaC <- convert(CommentsPizza, to = "topicmodels", docvars = NULL)
pave6 <- LDA(PizzaC, k = 9)
Pizza_topics <- tidy(pave6, matrix = "beta")
Pizza_topics$Category <- "Pizza"

hidden_topics <- do.call("rbind", list(Asian_topics, Burger_topics, Chicken_topics, Meat_topics, Beverage_topics, Pizza_topics))
library(broom)
EssentialWordsAsian <- tidy(pave)
EssentialWordsAsian$Category <- "Asian"
EssentialWordsBurgers <- tidy(pave2)
EssentialWordsBurgers$Category <- "Burgers"
EssentialWordsChicken <- tidy(pave3)
EssentialWordsChicken$Category <- "Chicken"
EssentialWordsMeat <- tidy(pave4)
EssentialWordsMeat$Category <- "Meat"
EssentialWordsBeverages <- tidy(pave5)
EssentialWordsBeverages$Category <- "Beverages"
EssentialWordsPizza <- tidy(pave6)
EssentialWordsPizza$Category <- "Pizza"

EssentialWords <- do.call("rbind", list(EssentialWordsAsian, EssentialWordsBeverages, EssentialWordsBurgers, EssentialWordsChicken, EssentialWordsMeat, EssentialWordsPizza))  
library(ggplot2)
library(dplyr)

AsianTopics <- EssentialWordsAsian %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

AsianTopics %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

BeverageTopics <- EssentialWordsBeverages %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

BeverageTopics %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

BurgerTopics <- EssentialWordsBurgers %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

BurgerTopics %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ChickenTopics <- EssentialWordsChicken %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ChickenTopics %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

MeatTopics <- EssentialWordsMeat %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

MeatTopics %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

PizzaTopics <- EssentialWordsPizza %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

PizzaTopics %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


AllTopics <- do.call("rbind", list(AsianTopics, BeverageTopics, BurgerTopics, ChickenTopics, MeatTopics, PizzaTopics)) %>% arrange(-beta)


AllTopics %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggplot(AsianTopics, aes(x= reorder(term, -beta),beta)) + geom_bar(stat ='identity') + ylab('Beta') + xlab('Words') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(igraph)
set.seed(239)
g <- graph.data.frame(AllTopics, directed = FALSE)
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightgreen", "red")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
V(g)$size <- eccentricity(g) * 2.5
V(g)$label.cex <- degree(g) * 4.5
plot(g, vertex.label.cex = 0.7, vertex.label.color = "black", layout = layout.auto)

g_asian <- graph.data.frame(AsianTopics, directed = FALSE)
bipartite.mapping(g_asian)
V(g_asian)$type <- bipartite_mapping(g_asian)$type
V(g_asian)$color <- ifelse(V(g_asian)$type, "lightgreen", "red")
V(g_asian)$shape <- ifelse(V(g_asian)$type, "circle", "square")
E(g_asian)$color <- "lightgray"
V(g_asian)$size <- eccentricity(g_asian) * 2.5
V(g_asian)$label.cex <- degree(g_asian) * 4.5
plot(g_asian, vertex.label.cex = 0.7, vertex.label.color = "black", layout = layout.auto, main = "Asian")

g_beverage <- graph.data.frame(BeverageTopics, directed = FALSE)
bipartite.mapping(g_beverage)
V(g_beverage)$type <- bipartite_mapping(g_beverage)$type
V(g_beverage)$color <- ifelse(V(g_beverage)$type, "lightgreen", "red")
V(g_beverage)$shape <- ifelse(V(g_beverage)$type, "circle", "square")
E(g_beverage)$color <- "lightgray"
V(g_beverage)$size <- eccentricity(g_beverage) * 2.5
V(g_beverage)$label.cex <- degree(g_beverage) * 4.5
plot(g_beverage, vertex.label.cex = 0.7, vertex.label.color = "black", layout = layout.auto, main = "Alcoholic Beverages")

g_burgers <- graph.data.frame(BurgerTopics, directed = FALSE)
bipartite.mapping(g_burgers)
V(g_burgers)$type <- bipartite_mapping(g_burgers)$type
V(g_burgers)$color <- ifelse(V(g_burgers)$type, "lightgreen", "red")
V(g_burgers)$shape <- ifelse(V(g_burgers)$type, "circle", "square")
E(g_burgers)$color <- "lightgray"
V(g_burgers)$size <- eccentricity(g_burgers) * 2.5
V(g_burgers)$label.cex <- degree(g_burgers) * 4.5
plot(g_burgers, vertex.label.cex = 0.7, vertex.label.color = "black", layout = layout.auto, main = "Burgers")

g_chicken <- graph.data.frame(ChickenTopics, directed = FALSE)
bipartite.mapping(g_chicken)
V(g_chicken)$type <- bipartite_mapping(g_chicken)$type
V(g_chicken)$color <- ifelse(V(g_chicken)$type, "lightgreen", "red")
V(g_chicken)$shape <- ifelse(V(g_chicken)$type, "circle", "square")
E(g_chicken)$color <- "lightgray"
V(g_chicken)$size <- eccentricity(g_chicken) * 2.5
V(g_chicken)$label.cex <- degree(g_chicken) * 4.5
plot(g_chicken, vertex.label.cex = 0.7, vertex.label.color = "black", layout = layout.auto, main = "Chicken")

g_meat <- graph.data.frame(MeatTopics, directed = FALSE)
bipartite.mapping(g_meat)
V(g_meat)$type <- bipartite_mapping(g_meat)$type
V(g_meat)$color <- ifelse(V(g_meat)$type, "lightgreen", "red")
V(g_meat)$shape <- ifelse(V(g_meat)$type, "circle", "square")
E(g_meat)$color <- "lightgray"
V(g_meat)$size <- eccentricity(g_meat) * 2.5
V(g_meat)$label.cex <- degree(g_meat) * 4.5
plot(g_meat, vertex.label.cex = 0.7, vertex.label.color = "black", layout = layout.auto, main = "Meat")

g_pizza <- graph.data.frame(PizzaTopics, directed = FALSE)
bipartite.mapping(g_pizza)
V(g_pizza)$type <- bipartite_mapping(g_pizza)$type
V(g_pizza)$color <- ifelse(V(g_pizza)$type, "lightgreen", "red")
V(g_pizza)$shape <- ifelse(V(g_pizza)$type, "circle", "square")
E(g_pizza)$color <- "lightgray"
V(g_pizza)$size <- eccentricity(g_pizza) * 2.5
V(g_pizza)$label.cex <- degree(g_pizza) * 4.5
plot(g_pizza, vertex.label.cex = 0.7, vertex.label.color = "black", layout = layout.auto, main = "Pizza")


library(ggridges)
ggplot(UserComments, aes(x=Rating, y=as.factor(UserComments$classification))) + geom_density_ridges(fill="green", alpha = 0.4) + ylab("Cluster") + xlab("Rating") + theme(axis.text.y = element_text(family="Arial", face="bold", colour="black", size=rel(4))) + theme(axis.text.x = element_text(family="Arial", face="bold", colour="black", size=rel(2)))


devtools::install_github("briatte/ggnet")
library(network)
library(sna)
library(ggplot2)
library(ggnet)
library(intergraph)
ggnet2(asNetwork(g), mode = "fruchtermanreingold")
