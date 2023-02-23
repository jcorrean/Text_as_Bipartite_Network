load("/home/jc/Documents/GitHub/Text_as_Bipartite_Network/SkillsOccupations.RData")
# Example for Job Titles and Skills
library(dplyr)
Scientists <- filter(skills, grepl("Scientist", Occupation))
library(igraph)
bn2 <- graph.data.frame(Scientists,directed=FALSE)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "lightblue", "yellow")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.8, 1)
V(bn2)$size <- (Scientists$Importance)/3
E(bn2)$color <- "black"
plot(bn2,
     vertex.label.color = "black",
     layout = layout_with_graphopt, main = "")
