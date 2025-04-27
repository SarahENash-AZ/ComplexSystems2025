# Header ------------- 
# Spring 2025 Complex Systems R Script
# Sarah Nash
# sarahenash@arizona.edu
# With help from Anna Dornhaus and Emiliano Calvo-Alcaniz

# Set working directory --------------
setwd <- "C:/Users/sarah.nash/OneDrive - USDA/Desktop/ComplexSystems2025/"

# Install packages -------------
install.packages("sna")
install.packages("intergraph")
install.packages("igraph")
install.packages("viridis")
install.packages("scales")
install.packages("bipartite")
install.packages("networkD3")
install.packages("circlize")

# Activate packages -------------
library(sna)
library(intergraph)
library(igraph)
library(viridis)
library(scales)
library(bipartite)  
library(networkD3) 
library(circlize)


# Cocaine Network ------------

# Import data as dataframe
coke <- read.csv("COCAINE_DEALING.csv", header = TRUE)
# turns it into a graph
cokeg <- graph_from_adjacency_matrix(as.matrix(coke[,-1]))
# turns it into adjacency matrix
coke_adj <- as_adjacency_matrix(cokeg, sparse = FALSE)
# as a network
coke_sna <- asNetwork(coke, matrix.type = "adjacency matrix")


plot(cokeg)
heatmap(coke_adj, scale = "none")

?heatmap()

gplot(coke_sna, diag=TRUE, vertex.col=1, edge.lwd=1, loop.cex=2)

# Measure the degree 
cokedegree <- igraph::degree(cokeg, mode = "all") 
# Transform to numeric so that colors can be assigned later
cokedegree_numeric <- as.numeric(unlist(cokedegree))  
# scaling the degree in order to color nodes
coke_max <- max(cokedegree_numeric)
c_scaled_calculations <- cokedegree/coke_max
c_scaled <- as.numeric(ceiling(c_scaled_calculations * 100))
coke_color <- rev(viridis(100))
coke_deg_colorscale <- coke_color[c_scaled]


plot(cokeg
     # remove the labels to increase plot visibility
     ,     vertex.label = NA                        
     # fit the color scale to the number of nodes
     ,     vertex.color = coke_deg_colorscale
     # reduce arrow size
     ,     edge.arrow.size = 0.5
)
# Add a title -- the "\n" moves the title down some
title(main = paste("\n                       ", "\n ",
                   "\n Cocaine Drug Dealer Interactions"), cex.main = 1)  


# Create an object that contains the number of communications each individual
# received. ie. the edge weight
comm_rec <- as.numeric(rowSums(caviarm))
# Makes the colorscale dependent on the edge weight (communications received)
# rev() is used because it makes the dark colors heavier
comm_c_max <- max(comm_rec)
comm_c_scaled_calc <- comm_rec/comm_c_max
comm_scaled <- as.numeric(ceiling(comm_c_scaled_calc * 100))
commcaviar_colorscale <- caviar_color[comm_scaled]

communication_caviarplot <- plot(simplecaviar_igraph 
                                 # remove the labels because there's a lot of nodes
                                 ,     vertex.label = NA
                                 # makes the color scale dependent on the weight of communications
                                 ,     vertex.color = commcaviar_colorscale
                                 # shrinks the arrow sizes because there's a lot of them
                                 ,     edge.arrow.size = 0.5
)
# Add a title -- the "\n" knocks down the text by a line because it was too
# high in the plot
title(main = paste("\n                       ", "\n",
                   "\n Drug Smuggler Interactions colored",  
                   "\n by Number of Communications Received"), cex.main = 1.5)  

# Hen Network -------------



# Elberling Network -------------
Elberling <- graph_from_biadjacency_matrix(as.matrix(elberling1999), directed = FALSE, weighted = TRUE)

View(elberling1999)





# Dopi Focus Network ------------



