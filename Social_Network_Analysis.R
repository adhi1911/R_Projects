#Social Network Analysis

if (!require("igraph")) install.packages("igraph")
library(igraph)
#making a graph
g <- graph(c(1,2,3,1,3,2,2,3,4,2,1,4),
           directed = T,
           n =5)
plot(g,
     vertex.color = "green",
     vertex.size = 35,
     edge.color = 'red',
     edge.arrow.size = 0.5
     )

g[] #prints adjaceny matrix for graph

g1 <- graph(c("A","R","R","L","L","A","A","L","K","L"))
plot(g1,
     vertex.color = "green",
     vertex.size = 35,
     edge.color = 'red',
     edge.arrow.size = 0.5
)

#network measures
ecount(g1)
vcount(g1)
degree(g1)
degree(g1 , mode = "in")
degree(g1 , mode = "out")

#diameter: length of shortest path between most distanced nodes
diameter(g1 , directed = T, weights = NA)

edge_density(g1 ,loops = F)
#edge_density = ecount(g1)/(vcount(g1)*(vcount(g1)-1))

reciprocity((g1))
closeness(g1 , mode="all", weights = NA)
betweenness(g1 , directed = T, weights = NA)
edge_betweenness(g1 , directed = T, weights = NA)


#importing data
data <- read.csv(file.choose() , header = T)
y <- data.frame(data$first , data$second)

#create network
net <- graph.data.frame(y , directed = T)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

#histogram
hist(V(net)$degree,
     col = "green",,
     main = "Node Degree",
     ylab = "Frequency",
     xlab = "Degree of vertices"
     )

#network diagram
set.seed(222)
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.5,
     vertex.label.dist = 0,
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     layout = layout.fruchterman.reingold.grid)

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.5,
     vertex.label.dist = 0,
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     layout = layout.kamada.kawai)


#hub and authorities
hs <- hub_score(net)$vector
as <- authority_score(net)$vector    

#par(mfrow = c(1,2))
set.seed(123)
plot(net , 
     vertex.size = hs*30,
     vertex.label.cex = 0.5,
     main = "hubs",
     vertex.color = rainbow(52),
     edge.arrow.size = 0.2,
     layout = layout.kamada.kawai)

set.seed(123)
plot(net , 
     vertex.size = as*30,
     vertex.label.cex = 0.5,
     main = "authorities",
     vertex.color = rainbow(52),
     edge.arrow.size = 0.2,
     layout = layout.kamada.kawai)


# Community Detection
net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,
     net,
     vertex.size = 12,
     vertex.label.cex = 0.6)

