# Here's an example that shows how to model a text input as a bipartite network
library(quanteda)
library(quanteda.textstats)
load("Comments.RData")
my_corpus <- corpus(UserComments$text)
mycorpus <- data.frame(summary(my_corpus, n = nrow(UserComments)))
docvars(my_corpus, "Category") <- UserComments$Category
summary(my_corpus)

Asiatica <- corpus_subset(my_corpus, Category == "Asian")
asiatica <- tokens(Asiatica, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) %>% dfm()
asiatica[,1:5]
