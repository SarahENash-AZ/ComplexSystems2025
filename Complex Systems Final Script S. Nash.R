# Header ------------- 
# Spring 2025 Complex Systems R Script
# Sarah Nash
# sarahenash@arizona.edu
# With help from Anna Dornhaus and Emiliano Calvo-Alcaniz

# Please note: with my limited R coding experience, I was helped immensely by Emiliano,
# much of my code was influenced by how he wrote his. As such I need to give proper credit. 
# I would not have been able to complete this project without his help. 

# Set working directory --------------

# setwd("C:/Users/sarah.nash/OneDrive - USDA/Desktop/ComplexSystems2025/")
 setwd("C:/Users/senas/OneDrive - University of Arizona/Desktop/Complex Systems/")

getwd()

# Install packages -------------

install.packages("sna")
install.packages("intergraph")
install.packages("igraph")
install.packages("viridis")
install.packages("scales")
install.packages("bipartite")
install.packages("networkD3")
install.packages("circlize")
install.packages("networkD3")
install.packages("dplyr")
install.packages("tidyr")

# Activate packages -------------

library(sna)
library(intergraph)
library(igraph)
library(viridis)
library(scales)
library(bipartite)  
library(network) 
library(circlize)
library(networkD3)
library(dplyr)
library(tidyr)


# Cocaine Network ------------

# Import data as dataframe
coke <- read.csv("COCAINE_DEALING.csv", header = TRUE)

# Make adjacency matrix removing first column and row
coke_adj_matrix <- as.matrix(coke[,-1])

rownames(coke_adj_matrix) <- coke[,1]
colnames(coke_adj_matrix) <- coke[,1]

#View(coke_adj_matrix)

# Make into igraph object
cokeg <- graph_from_adjacency_matrix(coke_adj_matrix)
#cokeg <- graph_from_data_frame(coke, directed = TRUE)

class(cokeg)
#View(cokeg)

# as a network
coke_sna <- network(coke_adj_matrix, matrix.type = "adjacency")

# Initial plots
plot(cokeg)
heatmap(coke_adj_matrix, scale = "none")

gplot(coke_sna, diag=TRUE, vertex.col=1, edge.lwd=1, loop.cex=2)

# Measure the degree 
cokedegree <- igraph::degree(cokeg, mode = "all") 
# Transform to numeric so that colors can be assigned later
cokedegree_numeric <- as.numeric(unlist(cokedegree))  
# scaling the degree in order to color nodes
coke_max <- max(cokedegree_numeric)
c_scaled_calculations <- cokedegree/coke_max
c_scaled <- as.numeric(ceiling(c_scaled_calculations * 100))
coke_color <- rev(magma(100))
coke_deg_colorscale <- coke_color[c_scaled]


plot(cokeg
     # change label color
     , vertex.label.color = "gray40"   
     # change the label size
     , vertex.label.cex = 0.6      
     # fit the color scale to the number of nodes
     ,     vertex.color = coke_deg_colorscale
     # reduce arrow size
     ,     edge.arrow.size = 0.5
)
# Add a title -- the "\n" moves the title down some
title(main = paste("\n                       ", "\n ",
                   "\n Cocaine Drug Dealer Interactions"), cex.main = 1)  

legend_labels <- c("Low Degree", "Medium Degree", "High Degree")
legend_colors <- coke_color[c(1, 50, 100)]  

# Add a legend

legend("bottomright",
       legend = legend_labels,
       fill = legend_colors,
       title = "Degree Level",
       border = "black",
       cex = 0.8)

# Create an object that holds the number of communications each dealer has received aka edge weight
communication_rec <- as.numeric(rowSums(coke_adj_matrix))
# Colorscale dependent on the edge weight
comm_coke_max <- max(communication_rec)
comm_coke_scaled_calc <- communication_rec/comm_coke_max
comm_scaled <- as.numeric(ceiling(comm_coke_scaled_calc * 100))
commcoke_colorscale <- coke_color[comm_scaled]

communication_cokeplot <- plot(cokeg 
                                 # change label color
                                 , vertex.label.color = "gray40"
                                 # change the label size
                                 , vertex.label.cex = 0.6 
                                 #  fit the color scale to the number of nodes
                                 ,     vertex.color = commcoke_colorscale
                                 # reduce arrow size
                                 ,     edge.arrow.size = 1
)
# Add a title 
title(main = paste("\n                       ", "\n",
                   "\n Drug Dealer Interactions Recieved"), cex.main = 1)
# Add a legend

legend_labels <- c("Low Communication", "Medium Communication", "High Communication")
legend_colors <- coke_color[c(1, 50, 100)] 

legend("bottomright",
       legend = legend_labels,
       fill = legend_colors,
       title = "Communication Level",
       border = "black",
       cex = 0.8)


# Hierarchical Clustering
colorscale <- viridis(5)
node_coordinates <- layout_nicely(cokeg)
distance_object <- dist(coke_adj_matrix)
hier_clust <- hclust(distance_object)
plot(hier_clust)
# The hier_clust object contains the information of the dendrogram.
# To cut the dendrogram into a predefined number of clusters, we can
# use 'cutree', which outputs a list of the nodes with their cluster IDs.
# This in turn can be used to define the colors in a plot, for example:
plot(cokeg
     , vertex.color = colorscale[cutree(hier_clust, k = 3)]
     , vertex.label.cex = 0.7
     , vertex.label.color = "grey"
     , vertex.label.family = "Arial"
     , edge.arrow.size = 0
     , edge.width = 2
     , layout = node_coordinates
)

# Add a title 
title(main = paste("\n                       ", "\n",
                   "\n Drug Dealer Clustered Heirarchy"), cex.main = 1)
# Add a legend

legend_labels <- c("Cluster 1", "Cluster 2", "Cluster 3")
legend_colors <- colorscale[cutree(hier_clust, k = 3)] 

legend("bottomright",
       legend = legend_labels,
       fill = colorscale,
       title = "Clusters",
       border = "black",
       cex = 0.8)

# Cocaine Network Measures -------------

# -- Degree -- 

# calculate degree
coke_deg <- igraph::degree(cokeg, mode = "all")
# make this measure numeric for calculating the mean
num_coke_deg <- as.numeric(coke_deg)
# mean degree 
coke_avg_deg <- mean(coke_deg)
# calculate and print
cat("Average degree:", coke_avg_deg, "\n")

# -- Closeness --

# calculate closeness 
coke_closeness <- igraph::closeness(cokeg)
# mean of closeness
coke_avg_closeness <- mean(coke_closeness)
# calculate and print
cat("Average closeness:", coke_avg_closeness, "\n")

# -- Betweenness -- 
coke_betweenness <- igraph::betweenness(cokeg)
# mean of the betweenness 
coke_avg_betweenness <- mean(coke_betweenness)
# Calculate it and print it out
cat("Average betweenness:", coke_avg_betweenness, "\n")

# -- clustering coefficient -- 

# Calculate the clustering coefficient
coke_cc <- transitivity(cokeg, type = "global")
# print it out
cat("Clustering coefficient:", coke_cc, "\n")

# Create a random network [sample_gnm()] that has the same number of nodes 
# [vcount()] and edges [ecount()]
randomcoke_cc1a <- sample_gnm(vcount(cokeg), ecount(cokeg)  
                                , directed = TRUE, loops = FALSE)
# Calculate the Clustering coefficient of the random network
random_coke_cc1 <- transitivity(randomcoke_cc1a, type = "global")
# Calculate it and print it out
cat("Clustering coefficient of a RANDOM network:", random_coke_cc1, "\n")


# -- Motifs -- Doesn't work anymore `\_(._.)_/` 

# Calculate the motifs of the dataset for groups of 3 individuals
coke_mot <- motifs(cokeg, size = 3)
# Set the aesthetic parameters & layout
par(mar=c(0,1,0,1), oma = c(1,2,1,1), xpd=TRUE)# bottom, left, top, right
layout(matrix(c(1,1,1,1,0,1,0,0,1,2,1,4,1,1,3,1,1,1,0,2,2,4,5,0,7,2,9,1,11,2,13,14,15,22,26,1)
              , nrow = 2, ncol = 36, byrow = T)
       , heights=c(4,1)) 
# set the colors for this graph, matching the color scale used earlier
color_distributions <- magma(50)
# create a barplot showing the number of motifs out of each possible motif
barplot(coke_mot
        , col = color_distributions
        , names.arg = seq(1, 16)
)
# set the parameters to show the aesthetics going underneath
par(mar=c(0,0.5,0,0))
for (i in 0:15) {
  # This command gives the graph number i that is possible with
  # 3 nodes. 
  coke_motif_graph <- graph_from_isomorphism_class(3, i)
  # Now plotting that:
  plot(coke_motif_graph
       , edge.arrow.size = 0.5
       , edge.color = alpha("grey27", 0.5)
       , edge.width = 2
       , vertex.label.color = color_distributions
       , vertex.label.cex = 1
  )
}

# -- Assortativity --

# calculate the assortativity of the degree
coke_assortativity <- assortativity(cokeg, values = coke_deg, directed = TRUE)
# Calculate it and print it out
cat("Assortativity of this network:", coke_assortativity, "\n")

# calculate the degree of the random graph from earlier
random_coke_deg1 <- igraph::degree(randomcoke_cc1a, mode = "in")
# Calculate the assortativity of the random graph
random_coke_assortativity <- assortativity(randomcoke_cc1a, values = random_coke_deg1, directed = TRUE)
# Calculate it and print it out
cat("Assortativity of a RANDOM network:", random_coke_assortativity, "\n")


# -- Modularity --

# Calculate the modularity 
communities_coke <- cluster_edge_betweenness(cokeg)
coke_modularity <- modularity(communities_coke)
#  Print it out
cat("Modularity of this network:", coke_modularity, "\n")

# -- Diameter

# Calculate the diameter
coke_diameter <- diameter(cokeg, directed = TRUE)
# Print it out
cat("Diameter of this network:", coke_diameter, "\n")


# Hen Network -------------

hen_df <- read.csv("HenMatrix.csv", header = TRUE)

# Make adjacency matrix removing first column and row
hen_adj_matrix <- as.matrix(hen_df[,-1])

rownames(hen_adj_matrix) <- hen_df[,1]
colnames(hen_adj_matrix) <- hen_df[,1]

class(hen_df)

# as a data frame
hen_m <- data.matrix(hen_adj_matrix)
# as a matrix
#View(hen_m)
hen <- graph_from_adjacency_matrix(as.matrix(hen_adj_matrix))
# as an adjacency matrix
# NAs introduced by coercion

#View(hen)
#As a network
hen_sna <- asNetwork(hen)

# Initial Plots

heatmap(hen_adj_matrix)
plot(hen)
plot(hen_sna)

# Better heatmap

heatmap(as.matrix(hen_adj_matrix)
        , Rowv = NA
        , Colv = NA
        , col = magma(50)
        , scale = "none"
        , margins = c(8,6)
)

#  Add a legend 
legend("bottomright"
       , legend = c("No Pecking", "Pecking")
       , fill = magma(2)
)


gplot(hen_sna, diag=TRUE, vertex.col=1, edge.lwd=1, loop.cex=2)

# Measure the degree 
hendegree <- igraph::degree(hen, mode = "all") 
# Transform to numeric so that colors can be assigned later
hendegree_numeric <- as.numeric(unlist(hendegree))  
# scaling the degree in order to color nodes
hen_max <- max(hendegree_numeric)
h_scaled_calculations <- hendegree/hen_max
h_scaled <- as.numeric(ceiling(h_scaled_calculations * 100))
hen_color <- rev(magma(100))
hen_deg_colorscale <- hen_color[h_scaled]


plot(hen
     # change label color
     , vertex.label.color = "gray60"   
     # change the label size
     , vertex.label.cex = 0.6      
     # fit the color scale to the number of nodes
     ,     vertex.color = hen_deg_colorscale
     # reduce arrow size
     ,     edge.arrow.size = 0.5
)
# Add a title -- the "\n" moves the title down some
title(main = paste("\n                       ", "\n ",
                   "\n Hen Directed Interactions"), cex.main = 1)  

legend_labels <- c("Low Degree", "High Degree")
legend_colors <- hen_color[c(1, 100)]  

# Add a legend

legend("bottomright",
       legend = legend_labels,
       fill = legend_colors,
       title = "Degree Level",
       border = "black",
       cex = 0.8)


# Create an object that holds the number of communications each dealer has recieved aka edge weight
communication_rec <- as.numeric(rowSums(hen_adj_matrix))
# Colorscale dependent on the edge weight
comm_hen_max <- max(communication_rec)
comm_hen_scaled_calc <- communication_rec/comm_hen_max
comm_scaled <- as.numeric(ceiling(comm_hen_scaled_calc * 100))
commhen_colorscale <- hen_color[comm_scaled]

communication_henplot <- plot(hen 
                                 # change label color
                                 , vertex.label.color = "gray60"
                                 # change the label size
                                 , vertex.label.cex = 0.6 
                                 #  fit the color scale to the number of nodes
                                 ,     vertex.color = commhen_colorscale
                                 # reduce arrow size
                                 ,     edge.arrow.size = 1
)
# Add a title 
title(main = paste("\n                       ", "\n",
                   "\n Chicken Pecks Recieved"), cex.main = 1)
# Add a legend

legend_labels <- c("Low Pecking", "Medium Pecking", "High Pecking")
legend_colors <- hen_color[c(1, 50, 100)] 

legend("bottomright",
       legend = legend_labels,
       fill = legend_colors,
       title = "Pecks Recieved",
       border = "black",
       cex = 0.8)


# Hen Network Degree Measures ---------

# -- Degree -- 

# calculate degree
hen_deg <- igraph::degree(hen, mode = "all")

total_deg <- sum(hen_deg)
print(total_deg)
# make this measure numeric for calculating the mean
num_hen_deg <- as.numeric(hen_deg)
# mean degree 
hen_avg_deg <- mean(hen_deg)
# calculate and print
cat("Average degree:", hen_avg_deg, "\n")

# -- Closeness --

# calculate closeness
hen_closeness <- igraph::closeness(hen)
# mean of closeness
hen_avg_closeness <- mean(hen_closeness)
# calculate and print
cat("Average closeness:", hen_avg_closeness, "\n")

# -- Betweenness --
hen_betweenness <- igraph::betweenness(hen)
# mean of the betweenness 
hen_avg_betweenness <- mean(hen_betweenness)
# Calculate it and print it out
cat("Average betweenness:", hen_avg_betweenness, "\n")

## -- clustering coefficient -- 

edge_density(hen)

# I was previously getting a clustering coefficient of 1, but this is extremely
# unlikely, so with the assistance of Anna's ChatGPT bot I made the following
# fixes

# Remove self-loops
diag(hen_adj_matrix) <- 0

# Threshold issues - to fix, make matrix binary
hen_bin <- (hen_adj_matrix > 0) * 1

# Remake iGraph
hen_graph_dir <- graph_from_adjacency_matrix(hen_bin, mode = "directed")

# Measure clustering coefficient
transitivity(hen_graph_dir, type = "local")

head(transitivity(hen_graph_dir, type = "local"))


# I am still getting a clustering coefficient of 1. While unlikely, this seems
# to be the actual result


# Create a random network 
randomhen_cc1a <- sample_gnm(vcount(hen), ecount(hen)  
                                , directed = TRUE, loops = FALSE)
# Calculate the Clustering coefficient of the random network
randomhen_cc1 <- transitivity(randomhen_cc1a, type = "global")
# Calculate it and print it out
cat("Clustering coefficient of a RANDOM network:", randomhen_cc1, "\n")

# -- Assortativity --

# calculate the assortativity of the degree
hen_assortativity <- assortativity.degree(hen, directed = TRUE)
# Calculate it and print it out
cat("Assortativity:", hen_assortativity, "\n")

# calculate the degree of the random graph from earlier
random_caviar_deg1 <- igraph::degree(randomcaviar_cc1a, mode = "in")
# Calculate the assortativity of the random graph
random_caviar_assortativity <- assortativity(randomcaviar_cc1a, values = random_caviar_deg1, directed = TRUE)
# Calculate it and print it out
cat("Assortativity of a RANDOM network:", random_caviar_assortativity, "\n")


# -- Modularity --

# Calculate the modularity 
communities_coke <- cluster_edge_betweenness(cokeg)
coke_modularity <- modularity(communities_coke)
#  Print it out
cat("Modularity of this network:", coke_modularity, "\n")

# Visualize modularity

hen_comm <- cluster_infomap(hen)
modularity_score <- modularity(hen_comm)
cat("Modularity score:", modularity_score, "\n")

adj <- as.matrix(as_adjacency_matrix(hen))

# Reorder rows and columns by community
ord <- order(membership(hen_comm))
adj_ord <- adj[ord, ord]

# Plot the matrix
image(t(adj_ord[nrow(adj_ord):1, ]), 
      col = viridis(2), 
      axes = FALSE,
      main = paste("Adjacency Matrix Ordered by Module\nModularity:", round(modularity_score, 3)))

# -- Diameter --

# Calculate the diameter
coke_diameter <- diameter(cokeg, directed = TRUE)
# Print it out
cat("Diameter of this network:", coke_diameter, "\n")

# -- Hierarchical Clustering --

# Compute Jaccard distance between rows (nodes)
jaccard_dist <- dist(hen_adj_matrix, method = "binary")  # binary treats rows as presence/absence

# Perform hierarchical clustering
hc <- hclust(jaccard_dist, method = "average")  # or "complete", "ward.D", etc.

# Basic dendrogram
plot(hc, hang = -1, cex = 0.6,
     main = "Hierarchical Clustering of Whiteleg Chickens",
     xlab = "Nodes", ylab = "Dissimilarity")


# Elberling Network -------------

# This is a built in data set for the bipartite package, so I don't need to import the data
# So, all I need to do is make it into a iGraph object and into a network
Elberling <- graph_from_biadjacency_matrix(as.matrix(elberling1999), directed = FALSE, weighted = TRUE)
# As a network
elberling_sna <- asNetwork(Elberling)


# Define top 6 Diptera-related genera
diptera_genera <- c("Alliopsis", "Botanophilia", "Delai", "Egle", "Paradelia", 
                    "Pegomya", "Pegoplata", "sp. indet.", "Zaphne", "Acamptocladius", 
                    "Corynoneura", "Heterotrissocladius", "Limnophyes", "Microspectra", 
                    "Oliveridia", "Pseudosmittia", "Smittia", "Empis", "Rhamphomyia",
                    "Coenosia", "Phaonia", "Spilogona", "Thricops", "Megaselia", "Bradysia",
                    "Bradysia sp.", "Corynoptera", "Cornyoptera sp.", "Lycoriella", "Lycoriella sp.",
                    "Trichosia"
                    )
# Empididae, muscidae, chironomidae, phoridae, anthomyiidae, sciaridae



# Logical vector: does the column name (pollinator name) contain any Diptera genus?
is_diptera <- sapply(colnames(elberling1999), function(x) {
  any(sapply(diptera_genera, function(genus) grepl(genus, x)))
})


is_diptera
# Subset the matrix: all rows (plants), Diptera columns only
diptera_pollinators <- elberling1999[, is_diptera]

# View the result
diptera_pollinators

#As a matrix
diptera_m <- as.matrix(diptera_pollinators)

#As an igraph object
dipterans_igraph <- graph_from_biadjacency_matrix(as.matrix(diptera_pollinators), directed = FALSE, weighted = TRUE)

plot(dipterans_igraph)


#Heatmap of Dipteran-Plant interactions

par(mar = c(2, 2, 2, 2))

colorscale_dips <- turbo(1+max(igraph::degree(dipterans_igraph, mode = "total")))
d_map <- heatmap(diptera_m
                 , col = colorscale_dips
                 , scale = "none"
                 , cexRow = 0.5
                 , cexCol = 0.5
)

# Add a title
title(main = paste("Dipteran Interaction Heatmap"), cex.main = 1)  

legend_labels <- c("Low Degree", "Medium Degree", "High Degree")
legend_colors <- colorscale_dips[c(1, 5, 10)]  

# Add a legend

legend("topleft",
       legend = legend_labels,
       fill = legend_colors,
       title = "Degree Level",
       border = "black",
       cex = 0.8)

# Bipartite network

# Define layout
layout(matrix(1:2, ncol = 2), widths = c(4, 1))

# Set margins
par(mar = c(5, 4, 4, 1))

# Reorder rows and columns by their total interaction strength
row_order <- order(rowSums(diptera_m), decreasing = TRUE)
col_order <- order(colSums(diptera_m), decreasing = TRUE)

# Apply the order to the matrix
dipt_sorted <- diptera_m[row_order, col_order]

# Calculate degree (number of connections) for each species
pollinator_deg <- rowSums(dipt_sorted)
plant_deg <- colSums(dipt_sorted)

# Map degrees to colors
pollinator_colors_dipt <- viridis(length(pollinator_deg))[rank(pollinator_deg)]
plant_colors_dipt <- viridis(length(plant_deg))[rank(plant_deg)]

# Plot with degree-based colors
plotweb(dipt_sorted,
        method = "normal",
        text.rot = 90,
        col.low = pollinator_colors_dipt,
        col.high = plant_colors_dipt,
        labsize = 0.7,
        y.width.low = 0.1,
        y.width.high = 0.1)

# Add a title
title(main = paste("Elberling Dipteran Bipartite Network"), cex.main = 1)  

# Add a legend

#Calculate degree
pollinator_deg <- rowSums(diptera_m)
plant_deg <- colSums(diptera_m)

#Cut into bins label
bin_labels <- c("Low", "Medium", "High")
pollinator_bins <- cut(pollinator_deg, breaks = 3, labels = bin_labels, include.lowest = TRUE)
plant_bins <- cut(plant_deg, breaks = 3, labels = bin_labels, include.lowest = TRUE)

#Assign colors
color_palette <- viridis(3)
pollinator_colors <- color_palette[as.numeric(pollinator_bins)]
plant_colors <- color_palette[as.numeric(plant_bins)]

par(mar = c(5, 0, 4, 2))  # no left margin, room on right
plot.new()
legend("center", legend = bin_labels, fill = color_palette,
       title = "Node Degree", border = NA)

# Elberling Network Measures ----------


# calculate degree
dipt_deg <- igraph::degree(dipterans_igraph, mode = "all")
# total degree of network
total_deg <- sum(dipt_deg)
print(total_deg)
# make this measure numeric for calculating the mean
num_dipt_deg <- as.numeric(dipt_deg)
# mean degree 
dipt_avg_deg <- mean(dipt_deg)
# calculate and print
cat("Average degree:", dipt_avg_deg, "\n")


# Nestedness for Elberling Dipterans ----------

compute_nestedness <- function(bipartite_network, mode = "total"){
  # Get number of rows and columns
  nrows <- nrow(bipartite_network)
  ncols <- ncol(bipartite_network)
  # Compute nestedness of rows
  nestedness_rows <- 0
  for(i in 1:(nrows-1)){
    for(j in (i+1): nrows){
      c_ij <- sum(bipartite_network[i,] * bipartite_network[j,])      # Number of interactions shared by i and j
      k_i <- sum(bipartite_network[i,])               # Degree of node i
      k_j <- sum(bipartite_network[j,])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      nestedness_rows <- nestedness_rows + o_ij
    }
  }
  
  # Compute nestedness of columns
  nestedness_cols <- 0
  for(i in 1: (ncols-1)){
    for(j in (i+1): ncols){
      c_ij <- sum(bipartite_network[,i] * bipartite_network[,j])      # Number of interactions shared by i and j
      k_i <- sum(bipartite_network[,i])               # Degree of node i
      k_j <- sum(bipartite_network[,j])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected.
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      nestedness_cols <- nestedness_cols + o_ij         
    }
  }
  
  # Compute nestedness of the network
  nestedness <- (nestedness_rows + nestedness_cols) / ((nrows * (nrows - 1) / 2) + (ncols * (ncols - 1) / 2))
  nestedness_rows <- nestedness_rows / (nrows*(nrows-1)/2)
  nestedness_cols <- nestedness_cols / (ncols*(ncols-1)/2)
  if(mode=="rows") {return(nestedness_rows)}
  if(mode=="cols") {return(nestedness_cols)}
  return(nestedness)
}

# Nestedness reduced network for Elberling Dipterans ----

class(dipterans_igraph)

# Create a unique list of vertex names from both columns of the edge list
vertex_names <- unique(c(as.character(diptera_m[,1]), as.character(diptera_m[,2])))

# Create a proper vertex data frame
vertices_df <- data.frame(name = vertex_names)

# Create the bipartite igraph object
g_dipt <- graph_from_data_frame(diptera_m, vertices = vertices_df, directed = FALSE)

# identify pollinators and plants based on your assumption
pollinators <- unique(diptera_m[,1])
plants <- unique(diptera_m[,2])

# remove edges where both ends are pollinators or both are plants
diptera_m_clean <- diptera_m[!(diptera_m[,1] %in% plants & diptera_m[,2] %in% plants) &
                               !(diptera_m[,1] %in% pollinators & diptera_m[,2] %in% pollinators), ]

# recreate graph from cleaned data
vertex_names <- unique(c(as.character(diptera_m_clean[,1]), as.character(diptera_m_clean[,2])))
vertices_df <- data.frame(name = vertex_names)
g_dipt <- graph_from_data_frame(diptera_m_clean, vertices = vertices_df, directed = FALSE)

# assign types again (pollinators = TRUE, plants = FALSE)
V(g_dipt)$type <- V(g_dipt)$name %in% pollinators



# Generate the matrix (Cargo as rows, Carrier as columns)
dipteran_matrix <- as_biadjacency_matrix(dipterans_igraph, types = V(dipterans_igraph)$type, sparse = FALSE)
#View(pathogen_matrix_red)
unweighed_dipt <- matrix(0, nrow = nrow(dipteran_matrix), ncol = ncol(dipteran_matrix))
unweighed_dipt[dipteran_matrix>0] <- 1
colnames(unweighed_dipt) <- colnames(dipteran_matrix)
rownames(unweighed_dipt) <- rownames(dipteran_matrix)

unweighed_dipt <- as.matrix(unweighed_dipt)

dim(unweighed_dipt)


compute_nestedness(unweighed_dipt, "cols")
compute_nestedness(unweighed_dipt, "rows")
compute_nestedness(unweighed_dipt, "total")

# Assume 'mat' is your bipartite adjacency matrix

dipterans_mat <- as.matrix(as_adjacency_matrix(dipterans_igraph, sparse = FALSE))

image(t(dipterans_mat[nrow(dipterans_mat):1, ]), col = viridis(2), axes = FALSE)

# Basic matrix with labels
# Set wider margins: bottom, left, top, right
par(mar = c(5, 8, 10, 2))  # Adjust as needed for label length

row_order <- order(rowSums(dipterans_mat), decreasing = TRUE)
col_order <- order(colSums(dipterans_mat), decreasing = TRUE)

mat_ordered <- dipterans_mat[row_order, col_order]

image(t(mat_ordered[nrow(mat_ordered):1, ]),
      col = viridis(2), axes = FALSE)

# Add box
box()

# Add column labels (top axis)
axis(3, at = seq(0, 1, length.out = ncol(mat_ordered)),
     labels = colnames(mat_ordered)[col_order], las = 2, cex.axis = 0.6)

# Add row labels (left axis)
axis(2, at = seq(0, 1, length.out = nrow(mat_ordered)),
     labels = rev(rownames(mat_ordered)[row_order]), las = 2, cex.axis = 0.6)


# Robustness for Elberling Dipterans -----------

simulate_robustness <- function(mat, remove_from = "columns") {
  mat <- mat[rowSums(mat) > 0, colSums(mat) > 0]  # Clean matrix
  
  if (remove_from == "columns") {
    guild <- colnames(mat)
  } else {
    guild <- rownames(mat)
  }
  
  extinct_secondary <- numeric(length(guild))
  
  for (i in seq_along(guild)) {
    # Remove one species
    if (remove_from == "columns") {
      mat <- mat[, !(colnames(mat) %in% guild[i]), drop = FALSE]
    } else {
      mat <- mat[!(rownames(mat) %in% guild[i]), , drop = FALSE]
    }
    
    # Count secondaries: rows or columns that are now all zeros
    if (remove_from == "columns") {
      extinct <- sum(rowSums(mat) == 0)
    } else {
      extinct <- sum(colSums(mat) == 0)
    }
    
    extinct_secondary[i] <- extinct
  }
  
  # Plot robustness curve
  plot(extinct_secondary, type = "s", lwd = 2,
       xlab = paste("Number of dipterans removed"),
       ylab = "Cumulative plant extinctions",
       main = "Network Robustness of the dipterans in the Elberling network")
}

# Measure Elberling Dipt Robustness --------------

simulate_robustness(dipterans_mat, remove_from = "columns")

# --------------------------------------------
# DOPi Focus Network ------------

# My focus network is actually the combination of 3 different networks
# all of which come from the Dopi data base. I am going to compare the
# generalist and specialist numbers in 3 different habitat types
# grassland, suburban areas, and urban areas. The goal here is to 
# understand how urban development changes plant-pollinator networks

# --------------------------------------------
# Grassland Network -------

# Import data as dataframe
grass <- read.csv("Grasslands.csv", row.names = 1)

#View(grass)

#As a matrix
grass_mat <- as.matrix(grass)

#As a network
grass_net <- network(grass_mat, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weight")

#As an igraph object
grass_igraph <- graph_from_biadjacency_matrix(grass_mat, mode = "all", weighted = TRUE)

#Assign labels to nodes
n_pollinators <- nrow(grass_mat)
n_plants <- ncol(grass_mat)

# Assign vertex names (species labels)
network.vertex.names(grass_net) <- c(rownames(grass_mat), colnames(grass_mat))

# Initial plots
plot(grass_igraph)
heatmap(grass_mat, scale = "none")

# A good heatmap

#Set margins
par(mar = c(2, 2, 2, 2))

# Set colorscale
colorscale_grass <- turbo(1+max(igraph::degree(grass_igraph, mode = "total")))
g_map <- heatmap(grass_mat
                 , col = colorscale_grass
                 , scale = "none"
                 , cexRow = 0.5
                 , cexCol = 0.5
)

# Add a title
title(main = paste("Grassland Interaction Heatmap"), cex.main = 1)  

legend_labels <- c("Low Degree", "Medium Degree", "High Degree")
legend_colors <- colorscale_grass[c(1, 50, 100)]  

# Add a legend

legend("topleft",
       legend = legend_labels,
       fill = legend_colors,
       title = "Degree Level",
       border = "black",
       cex = 0.8)

# This network has a lot of space with no interactions. For the sake of 
# visualization, I am going to take a subset of the data. 

# Keep only rows (pollinators) with at least one interaction
grass_subset <- grass_mat[rowSums(grass_mat) > 100, ]

# Then keep only columns (plants) with at least one interaction
grass_subset <- grass_subset[, colSums(grass_subset) > 100]

class(grass_subset)

#As an igraph object
grass_ss_igraph <- graph_from_biadjacency_matrix(grass_subset, mode = "all", weighted = TRUE)

#As a network
grass_ss_net <- network(grass_subset, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weight")


# Heatmap with grass_subset

#Set margins
par(mar = c(2, 2, 2, 2))

# Set colorscale
colorscale_grass_ss <- turbo(1+max(igraph::degree(grass_ss_igraph, mode = "total")))
g_map <- heatmap(grass_subset
                 , col = colorscale_grass_ss
                 , scale = "none"
                 , cexRow = 0.5
                 , cexCol = 0.5
)

# Add a title
title(main = paste("Grassland Subset Interaction Heatmap"), cex.main = 1)  

legend_labels <- c("Low Degree", "Medium Degree", "High Degree")
legend_colors <- colorscale_grass_ss[c(1, 5, 10)]  

# Add a legend

legend("topleft",
       legend = legend_labels,
       fill = legend_colors,
       title = "Degree Level",
       border = "black",
       cex = 0.8)


# Interactive network - network3D

# Ensure 'type' is defined and logical (bipartite: TRUE/FALSE)
V(grass_igraph)$type <- bipartite_mapping(grass_igraph)$type

# Convert igraph object to networkD3 format
net_d3 <- igraph_to_networkD3(grass_igraph, group = as.numeric(V(grass_igraph)$type))

forceNetwork(Links = net_d3$links, Nodes = net_d3$nodes,
             Source = "source", Target = "target",
             NodeID = "name", Group = "group",
             opacity = 0.9, zoom = TRUE)

# Bipartite layout

# Define layout
layout(matrix(1:2, ncol = 2), widths = c(4, 1))

# Set margins
par(mar = c(5, 4, 4, 1))

# Reorder rows and columns by their total interaction strength
row_order <- order(rowSums(grass_subset), decreasing = TRUE)
col_order <- order(colSums(grass_subset), decreasing = TRUE)

# Apply the order to the matrix
grass_ss_sorted <- grass_subset[row_order, col_order]

# Calculate degree (number of connections) for each species
pollinator_deg <- rowSums(grass_ss_sorted)
plant_deg <- colSums(grass_ss_sorted)

# Map degrees to colors
pollinator_colors_grass <- viridis(length(pollinator_deg))[rank(pollinator_deg)]
plant_colors_grass <- viridis(length(plant_deg))[rank(plant_deg)]

# Plot with degree-based colors
plotweb(grass_ss_sorted,
        method = "normal",
        text.rot = 90,
        col.low = pollinator_colors_grass,
        col.high = plant_colors_grass,
        labsize = 0.7,
        y.width.low = 0.1,
        y.width.high = 0.1)

# Add a title
title(main = paste("Grassland Subset Bipartite Network"), cex.main = 1)  

# Add a legend

#Calculate degree
pollinator_deg <- rowSums(grass_subset)
plant_deg <- colSums(grass_subset)

#Cut into bins label
bin_labels <- c("Low", "Medium", "High")
pollinator_bins <- cut(pollinator_deg, breaks = 3, labels = bin_labels, include.lowest = TRUE)
plant_bins <- cut(plant_deg, breaks = 3, labels = bin_labels, include.lowest = TRUE)

#Assign colors
color_palette <- viridis(3)
pollinator_colors <- color_palette[as.numeric(pollinator_bins)]
plant_colors <- color_palette[as.numeric(plant_bins)]

par(mar = c(5, 0, 4, 2))  # no left margin, room on right
plot.new()
legend("center", legend = bin_labels, fill = color_palette,
       title = "Node Degree", border = NA)


# DOPi Grassland Network Measures ----------

# calculate degree
grass_deg <- igraph::degree(grass_igraph, mode = "all")
#total degree
total_deg <- sum(grass_deg)
print(total_deg)
# make this measure numeric for calculating the mean
num_grass_deg <- as.numeric(grass_deg)
# mean degree 
grass_avg_deg <- mean(grass_deg)
# calculate and print
cat("Average degree:", grass_avg_deg, "\n")


# Nestedness for DOPi Grassland ----------

compute_nestedness <- function(bipartite_network, mode = "total"){
  # Get number of rows and columns
  nrows <- nrow(bipartite_network)
  ncols <- ncol(bipartite_network)
  # Compute nestedness of rows
  nestedness_rows <- 0
  for(i in 1:(nrows-1)){
    for(j in (i+1): nrows){
      c_ij <- sum(bipartite_network[i,] * bipartite_network[j,])      # Number of interactions shared by i and j
      k_i <- sum(bipartite_network[i,])               # Degree of node i
      k_j <- sum(bipartite_network[j,])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      nestedness_rows <- nestedness_rows + o_ij
    }
  }
  
  # Compute nestedness of columns
  nestedness_cols <- 0
  for(i in 1: (ncols-1)){
    for(j in (i+1): ncols){
      c_ij <- sum(bipartite_network[,i] * bipartite_network[,j])      # Number of interactions shared by i and j
      k_i <- sum(bipartite_network[,i])               # Degree of node i
      k_j <- sum(bipartite_network[,j])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected.
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      nestedness_cols <- nestedness_cols + o_ij         
    }
  }
  
  # Compute nestedness of the network
  nestedness <- (nestedness_rows + nestedness_cols) / ((nrows * (nrows - 1) / 2) + (ncols * (ncols - 1) / 2))
  nestedness_rows <- nestedness_rows / (nrows*(nrows-1)/2)
  nestedness_cols <- nestedness_cols / (ncols*(ncols-1)/2)
  if(mode=="rows") {return(nestedness_rows)}
  if(mode=="cols") {return(nestedness_cols)}
  return(nestedness)
}

# Nestedness reduced network for DOPi Grassland ----

class(grass_igraph)

# Create a unique list of vertex names from both columns of the edge list
vertex_names <- unique(c(as.character(grass_mat[,1]), as.character(grass_mat[,2])))

# Create a proper vertex data frame
vertices_df <- data.frame(name = vertex_names)

# Create the bipartite igraph object
g_grass <- graph_from_data_frame(grass_mat, vertices = vertices_df, directed = FALSE)

# identify pollinators and plants based on your assumption
pollinators <- unique(grass_mat[,1])
plants <- unique(grass_mat[,2])

# remove edges where both ends are pollinators or both are plants
grass_m_clean <- grass_mat[!(grass_mat[,1] %in% plants & grass_mat[,2] %in% plants) &
                               !(grass_mat[,1] %in% pollinators & grass_mat[,2] %in% pollinators), ]

# recreate graph from cleaned data
vertex_names_g <- unique(c(as.character(grass_m_clean[,1]), as.character(grass_m_clean[,2])))
vertices_df_g <- data.frame(name = vertex_names_g)
g_grass <- graph_from_data_frame(grass_m_clean, vertices = vertices_df_g, directed = FALSE)

# assign types again (pollinators = TRUE, plants = FALSE)
V(g_grass)$type <- V(g_grass)$name %in% pollinators



# Generate the matrix (Cargo as rows, Carrier as columns)
grass_matrix <- as_biadjacency_matrix(grass_igraph, types = V(grass_igraph)$type, sparse = FALSE)
#View(pathogen_matrix_red)
unweighed_grass <- matrix(0, nrow = nrow(grass_matrix), ncol = ncol(grass_matrix))
unweighed_grass[grass_matrix>0] <- 1
colnames(unweighed_grass) <- colnames(grass_matrix)
rownames(unweighed_grass) <- rownames(grass_matrix)

unweighed_grass <- as.matrix(unweighed_grass)

dim(unweighed_grass)

# compute nestedness for plants, pollinators, and total
compute_nestedness(unweighed_grass, "cols")
compute_nestedness(unweighed_grass, "rows")
compute_nestedness(unweighed_grass, "total")

# Assume 'mat' is your bipartite adjacency matrix

grass_mat <- as.matrix(as_adjacency_matrix(grass_igraph, sparse = FALSE))

image(t(grass_mat[nrow(grass_mat):1, ]), col = viridis(2), axes = FALSE)

# Basic matrix with labels
# Set wider margins: bottom, left, top, right
par(mar = c(5, 10, 20, 2))  # Adjust as needed for label length

row_order <- order(rowSums(grass_mat), decreasing = TRUE)
col_order <- order(colSums(grass_mat), decreasing = TRUE)

mat_ordered <- grass_mat[row_order, col_order]

image(t(mat_ordered[nrow(mat_ordered):1, ]),
      col = viridis(2), axes = FALSE)

# Add box
box()

# Add column labels (top axis)
axis(3, at = seq(0, 1, length.out = ncol(mat_ordered)),
     labels = colnames(mat_ordered)[col_order], las = 2, cex.axis = 0.6)

# Add row labels (left axis)
axis(2, at = seq(0, 1, length.out = nrow(mat_ordered)),
     labels = rev(rownames(mat_ordered)[row_order]), las = 2, cex.axis = 0.6)


# Null Model Comparison Fixed-Fixed (FF) Preserves row and column sums --------------------

# shuffle while preserving row and column sums
randomize_matrix_ff <- function(mat, swaps = 1000) {
  for (s in 1:swaps) {
    # Pick two rows and two columns randomly
    r <- sample(1:nrow(mat), 2)
    c <- sample(1:ncol(mat), 2)
    
    # Get 2x2 submatrix
    sub <- mat[r, c]
    
    # Check for a swap pattern: [1,0][0,1] or [0,1][1,0]
    if ((sub[1,1] == 1 && sub[2,2] == 1 && sub[1,2] == 0 && sub[2,1] == 0) ||
        (sub[1,1] == 0 && sub[2,2] == 0 && sub[1,2] == 1 && sub[2,1] == 1)) {
      
      # Swap: toggle the 2x2 square
      mat[r[1], c[1]] <- 1 - mat[r[1], c[1]]
      mat[r[2], c[2]] <- 1 - mat[r[2], c[2]]
      mat[r[1], c[2]] <- 1 - mat[r[1], c[2]]
      mat[r[2], c[1]] <- 1 - mat[r[2], c[1]]
    }
  }
  return(mat)
}

# Run null model comparison
n_reps <- 100
null_nestedness <- numeric(n_reps)

for (i in 1:n_reps) {
  rand_mat <- randomize_matrix_ff(unweighed_grass, swaps = 5000)
  null_nestedness[i] <- compute_nestedness(rand_mat, "total")
}

# Compare to observed
observed <- compute_nestedness(unweighed_grass, "total")

# Plot
hist(null_nestedness, main = "Null Model Distribution of Nestedness",
     xlab = "Nestedness", col = "lightgray", breaks = 20)
abline(v = observed, col = "red", lwd = 2)
legend("topright", legend = paste("Observed =", round(observed, 3)), col = "red", lwd = 2)

# Calculate p-value
p_value <- mean(null_nestedness >= observed)
cat("p-value:", p_value, "\n")
# Robustness for DOPI Grassland -----------

simulate_robustness <- function(mat, remove_from = "columns") {
  mat <- mat[rowSums(mat) > 0, colSums(mat) > 0]  # Clean matrix
  
  if (remove_from == "columns") {
    guild <- colnames(mat)
  } else {
    guild <- rownames(mat)
  }
  
  extinct_secondary <- numeric(length(guild))
  
  for (i in seq_along(guild)) {
    # Remove one species
    if (remove_from == "columns") {
      mat <- mat[, !(colnames(mat) %in% guild[i]), drop = FALSE]
    } else {
      mat <- mat[!(rownames(mat) %in% guild[i]), , drop = FALSE]
    }
    
    # Count secondaries: rows or columns that are now all zeros
    if (remove_from == "columns") {
      extinct <- sum(rowSums(mat) == 0)
    } else {
      extinct <- sum(colSums(mat) == 0)
    }
    
    extinct_secondary[i] <- extinct
  }
  
  # Plot robustness curve
  plot(extinct_secondary, type = "s", lwd = 2,
       xlab = paste("Number of pollinators removed"),
       ylab = "Cumulative plant extinctions",
       main = "Network Robustness of DOPi Grassland network")
}

# Measure Grassland Robustness --------------

simulate_robustness(grass_mat, remove_from = "columns")


# --------------------------------------------
# Suburban Network --------------

# Import data as dataframe
sub <- read.csv("Suburban.csv", row.names = 1)

#View(sub)

#As a matrix
sub_mat <- as.matrix(sub)

#As a network
sub_net <- network(sub_mat, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weight")

#As an igraph object
sub_igraph <- graph_from_biadjacency_matrix(sub_mat, mode = "all", weighted = TRUE)

#Assign labels to nodes
n_pollinators <- nrow(sub_mat)
n_plants <- ncol(sub_mat)

# Assign vertex names (species labels)
network.vertex.names(sub_net) <- c(rownames(sub_mat), colnames(sub_mat))

# Initial plots
plot(sub_igraph)
heatmap(sub_mat, scale = "none")

# A good heatmap

#Set margins
par(mar = c(2, 2, 2, 2))

# Set colorscale
colorscale_sub <- turbo(1+max(igraph::degree(sub_igraph, mode = "total")))
s_map <- heatmap(sub_mat
                 , col = colorscale_sub
                 , scale = "none"
                 , cexRow = 0.5
                 , cexCol = 0.5
)

# Add a title
title(main = paste("Suburban Interaction Heatmap"), cex.main = 1)  

legend_labels <- c("Low Degree", "Medium Degree", "High Degree")
legend_colors <- colorscale_sub[c(1, 50, 100)]  

# Add a legend

legend("topleft",
       legend = legend_labels,
       fill = legend_colors,
       title = "Degree Level",
       border = "black",
       cex = 0.8)

# This network has a lot of space with no interactions. For the sake of 
# visualization, I am going to take a subset of the data. 

# Keep only rows (pollinators) with at least one interaction
sub_subset <- sub_mat[rowSums(sub_mat) > 100, ]

# Then keep only columns (plants) with at least one interaction
sub_subset <- sub_subset[, colSums(sub_subset) > 100]

class(sub_subset)

#As an igraph object
sub_ss_igraph <- graph_from_biadjacency_matrix(sub_subset, mode = "all", weighted = TRUE)

#As a network
sub_ss_net <- network(sub_subset, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weight")

# Heatmap with grass_subset

#Set margins
par(mar = c(2, 2, 2, 2))

# Set colorscale
colorscale_sub_ss <- turbo(1+max(igraph::degree(sub_ss_igraph, mode = "total")))
sub_map <- heatmap(sub_subset
                 , col = colorscale_sub_ss
                 , scale = "none"
                 , cexRow = 0.5
                 , cexCol = 0.5
)

# Add a title
title(main = paste("Suburban Subset Interaction Heatmap"), cex.main = 1)  

legend_labels <- c("Low Degree", "Medium Degree", "High Degree")
legend_colors <- colorscale_sub_ss[c(1, 12, 25)]  

# Add a legend

legend("topleft",
       legend = legend_labels,
       fill = legend_colors,
       title = "Degree Level",
       border = "black",
       cex = 0.8)


# Interactive network - network3D

# Ensure 'type' is defined and logical (bipartite: TRUE/FALSE)
V(sub_igraph)$type <- bipartite_mapping(sub_igraph)$type

# Convert igraph object to networkD3 format
net_d3 <- igraph_to_networkD3(sub_igraph, group = as.numeric(V(sub_igraph)$type))

forceNetwork(Links = net_d3$links, Nodes = net_d3$nodes,
             Source = "source", Target = "target",
             NodeID = "name", Group = "group",
             opacity = 0.9, zoom = TRUE)

# Bipartite layout

# Define layout
layout(matrix(1:2, ncol = 2), widths = c(4, 1))

# Set margins
par(mar = c(5, 4, 4, 1))

# Reorder rows and columns by their total interaction strength
row_order <- order(rowSums(sub_subset), decreasing = TRUE)
col_order <- order(colSums(sub_subset), decreasing = TRUE)

# Apply the order to the matrix
sub_ss_sorted <- sub_subset[row_order, col_order]

# Calculate degree (number of connections) for each species
pollinator_deg <- rowSums(sub_ss_sorted)
plant_deg <- colSums(sub_ss_sorted)

# Map degrees to colors
pollinator_colors_s <- viridis(length(pollinator_deg))[rank(pollinator_deg)]
plant_colors_s <- viridis(length(plant_deg))[rank(plant_deg)]

# Plot with degree-based colors
plotweb(sub_ss_sorted,
        method = "normal",
        text.rot = 90,
        col.low = pollinator_colors_s,
        col.high = plant_colors_s,
        labsize = 0.7,
        y.width.low = 0.1,
        y.width.high = 0.1)

# Add a title
title(main = paste("Suburban Subset Bipartite Network"), cex.main = 1)  

# Add a legend

#Calculate degree
pollinator_deg <- rowSums(sub_subset)
plant_deg <- colSums(sub_subset)

#Cut into bins label
bin_labels <- c("Low", "Medium", "High")
pollinator_bins <- cut(pollinator_deg, breaks = 3, labels = bin_labels, include.lowest = TRUE)
plant_bins <- cut(plant_deg, breaks = 3, labels = bin_labels, include.lowest = TRUE)

#Assign colors
color_palette <- viridis(3)
pollinator_colors <- color_palette[as.numeric(pollinator_bins)]
plant_colors <- color_palette[as.numeric(plant_bins)]

par(mar = c(5, 0, 4, 2))  # no left margin, room on right
plot.new()
legend("center", legend = bin_labels, fill = color_palette,
       title = "Node Degree", border = NA)

# DOPi Suburban Network Measures ----------


# calculate degree
sub_deg <- igraph::degree(sub_igraph, mode = "all")
# total degree
total_deg <- sum(sub_deg)
print(total_deg)
# make this measure numeric for calculating the mean
num_sub_deg <- as.numeric(sub_deg)
# mean degree 
sub_avg_deg <- mean(sub_deg)
# calculate and print
cat("Average degree:", sub_avg_deg, "\n")


# Nestedness for DOPi Suburban ----------

compute_nestedness <- function(bipartite_network, mode = "total"){
  # Get number of rows and columns
  nrows <- nrow(bipartite_network)
  ncols <- ncol(bipartite_network)
  # Compute nestedness of rows
  nestedness_rows <- 0
  for(i in 1:(nrows-1)){
    for(j in (i+1): nrows){
      c_ij <- sum(bipartite_network[i,] * bipartite_network[j,])      # Number of interactions shared by i and j
      k_i <- sum(bipartite_network[i,])               # Degree of node i
      k_j <- sum(bipartite_network[j,])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      nestedness_rows <- nestedness_rows + o_ij
    }
  }
  
  # Compute nestedness of columns
  nestedness_cols <- 0
  for(i in 1: (ncols-1)){
    for(j in (i+1): ncols){
      c_ij <- sum(bipartite_network[,i] * bipartite_network[,j])      # Number of interactions shared by i and j
      k_i <- sum(bipartite_network[,i])               # Degree of node i
      k_j <- sum(bipartite_network[,j])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected.
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      nestedness_cols <- nestedness_cols + o_ij         
    }
  }
  
  # Compute nestedness of the network
  nestedness <- (nestedness_rows + nestedness_cols) / ((nrows * (nrows - 1) / 2) + (ncols * (ncols - 1) / 2))
  nestedness_rows <- nestedness_rows / (nrows*(nrows-1)/2)
  nestedness_cols <- nestedness_cols / (ncols*(ncols-1)/2)
  if(mode=="rows") {return(nestedness_rows)}
  if(mode=="cols") {return(nestedness_cols)}
  return(nestedness)
}

# Nestedness reduced network for DOPi Suburban  ----

class(sub_igraph)

# Create a unique list of vertex names from both columns of the edge list
vertex_names <- unique(c(as.character(sub_mat[,1]), as.character(sub_mat[,2])))

# Create a proper vertex data frame
vertices_df <- data.frame(name = vertex_names)

# Create the bipartite igraph object
g_sub <- graph_from_data_frame(sub_mat, vertices = vertices_df, directed = FALSE)

# identify pollinators and plants based on your assumption
pollinators <- unique(sub_mat[,1])
plants <- unique(sub_mat[,2])

# remove edges where both ends are pollinators or both are plants
sub_m_clean <- sub_mat[!(sub_mat[,1] %in% plants & sub_mat[,2] %in% plants) &
                               !(sub_mat[,1] %in% pollinators & sub_mat[,2] %in% pollinators), ]

# recreate graph from cleaned data
vertex_names <- unique(c(as.character(sub_m_clean[,1]), as.character(sub_m_clean[,2])))
vertices_df <- data.frame(name = vertex_names)
g_sub <- graph_from_data_frame(sub_m_clean, vertices = vertices_df, directed = FALSE)

# assign types again (pollinators = TRUE, plants = FALSE)
V(g_sub)$type <- V(g_sub)$name %in% pollinators


# Generate the matrix (Cargo as rows, Carrier as columns)
sub_matrix <- as_biadjacency_matrix(sub_igraph, types = V(sub_igraph)$type, sparse = FALSE)
#View(pathogen_matrix_red)
weighed_sub <- matrix(0, nrow = nrow(sub_matrix), ncol = ncol(sub_matrix))
weighed_sub[sub_matrix>0] <- 1
colnames(weighed_sub) <- colnames(sub_matrix)
rownames(weighed_sub) <- rownames(sub_matrix)

weighed_sub <- as.matrix(weighed_sub)

dim(weighed_sub)

# compute nestedness of plants, pollinators, and total
compute_nestedness(weighed_sub, "cols")
compute_nestedness(weighed_sub, "rows")
compute_nestedness(weighed_sub, "total")

# Assume 'mat' is your bipartite adjacency matrix

sub_mat <- as.matrix(as_adjacency_matrix(sub_igraph, sparse = FALSE))

image(t(sub_mat[nrow(sub_mat):1, ]), col = viridis(2), axes = FALSE)

# Basic matrix with labels
# Set wider margins: bottom, left, top, right
par(mar = c(5, 8, 10, 2))  # Adjust as needed for label length


row_order <- order(rowSums(sub_mat), decreasing = TRUE)
col_order <- order(colSums(sub_mat), decreasing = TRUE)

mat_ordered <- sub_mat[row_order, col_order]

image(t(mat_ordered[nrow(mat_ordered):1, ]),
      col = viridis(2), axes = FALSE)

# Add box
box()

# Add column labels (top axis)
axis(3, at = seq(0, 1, length.out = ncol(mat_ordered)),
     labels = colnames(mat_ordered)[col_order], las = 2, cex.axis = 0.6)

# Add row labels (left axis)
axis(2, at = seq(0, 1, length.out = nrow(mat_ordered)),
     labels = rev(rownames(mat_ordered)[row_order]), las = 2, cex.axis = 0.6)

# Null Model Comparison Fixed-Fixed (FF) Preserves row and column sums --------------------

# shuffle while preserving row and column sums
randomize_matrix_ff <- function(mat, swaps = 1000) {
  for (s in 1:swaps) {
    # Pick two rows and two columns randomly
    r <- sample(1:nrow(mat), 2)
    c <- sample(1:ncol(mat), 2)
    
    # Get 2x2 submatrix
    sub <- mat[r, c]
    
    # Check for a swap pattern: [1,0][0,1] or [0,1][1,0]
    if ((sub[1,1] == 1 && sub[2,2] == 1 && sub[1,2] == 0 && sub[2,1] == 0) ||
        (sub[1,1] == 0 && sub[2,2] == 0 && sub[1,2] == 1 && sub[2,1] == 1)) {
      
      # Swap: toggle the 2x2 square
      mat[r[1], c[1]] <- 1 - mat[r[1], c[1]]
      mat[r[2], c[2]] <- 1 - mat[r[2], c[2]]
      mat[r[1], c[2]] <- 1 - mat[r[1], c[2]]
      mat[r[2], c[1]] <- 1 - mat[r[2], c[1]]
    }
  }
  return(mat)
}

# Run null model comparison
n_reps <- 100
null_nestedness <- numeric(n_reps)

for (i in 1:n_reps) {
  rand_mat <- randomize_matrix_ff(weighed_sub, swaps = 5000)
  null_nestedness[i] <- compute_nestedness(rand_mat, "total")
}

# Compare to observed
observed <- compute_nestedness(weighed_sub, "total")

# Plot
hist(null_nestedness, main = "Null Model Distribution of Nestedness",
     xlab = "Nestedness", col = "lightgray", breaks = 20)
abline(v = observed, col = "red", lwd = 2)
legend("topright", legend = paste("Observed =", round(observed, 3)), col = "red", lwd = 2)

# Calculate p-value
p_value <- mean(null_nestedness >= observed)
cat("p-value:", p_value, "\n")
# Robustness for DOPI Suburban -----------

simulate_robustness <- function(mat, remove_from = "columns") {
  mat <- mat[rowSums(mat) > 0, colSums(mat) > 0]  # Clean matrix
  
  if (remove_from == "columns") {
    guild <- colnames(mat)
  } else {
    guild <- rownames(mat)
  }
  
  extinct_secondary <- numeric(length(guild))
  
  for (i in seq_along(guild)) {
    # Remove one species
    if (remove_from == "columns") {
      mat <- mat[, !(colnames(mat) %in% guild[i]), drop = FALSE]
    } else {
      mat <- mat[!(rownames(mat) %in% guild[i]), , drop = FALSE]
    }
    
    # Count secondaries: rows or columns that are now all zeros
    if (remove_from == "columns") {
      extinct <- sum(rowSums(mat) == 0)
    } else {
      extinct <- sum(colSums(mat) == 0)
    }
    
    extinct_secondary[i] <- extinct
  }
  
  # Plot robustness curve
  plot(extinct_secondary, type = "s", lwd = 2,
       xlab = paste("Number of pollinators removed"),
       ylab = "Cumulative plant extinctions",
       main = "Network Robustness of DOPi Suburban network")
}

# Measure Suburban Robustness --------------

simulate_robustness(sub_mat, remove_from = "columns")


# --------------------------------------------
# Urban Network --------------

# Import data as dataframe
urb <- read.csv("Urban.csv", row.names = 1)

#View(urb)

#As a matrix
urb_mat <- as.matrix(urb)

#As a network
urb_net <- network(urb_mat, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weight")

#As an igraph object
urb_igraph <- graph_from_biadjacency_matrix(urb_mat, mode = "all", weighted = TRUE)

#Assign labels to nodes
n_pollinators <- nrow(urb_mat)
n_plants <- ncol(urb_mat)

# Assign vertex names (species labels)
network.vertex.names(urb_net) <- c(rownames(urb_mat), colnames(urb_mat))

# Initial plots #
plot(urb_igraph)
heatmap(urb_mat, scale = "none")

# A good heatmap #

#Set margins
par(mar = c(2, 2, 2, 2))

# Set colorscale
colorscale_urb <- turbo(1+max(igraph::degree(urb_igraph, mode = "total")))
u_map <- heatmap(urb_mat
                 , col = colorscale_urb
                 , scale = "none"
                 , cexRow = 0.5
                 , cexCol = 0.5
)

# Add a title
title(main = paste("Urban Interaction Heatmap"), cex.main = 1)  

legend_labels <- c("Low Degree", "Medium Degree", "High Degree")
legend_colors <- colorscale_urb[c(1, 50, 100)]  

# Add a legend

legend("topleft",
       legend = legend_labels,
       fill = legend_colors,
       title = "Degree Level",
       border = "black",
       cex = 0.8)

# This network has a lot of space with no interactions. For the sake of 
# visualization, I am going to take a subset of the data. 

# Keep only rows (pollinators) with at least one interaction
urb_subset <- urb_mat[rowSums(urb_mat) > 100, ]

# Then keep only columns (plants) with at least one interaction
urb_subset <- urb_subset[, colSums(urb_subset) > 100]

class(urb_subset)

#As an igraph object
u_ss_igraph <- graph_from_biadjacency_matrix(urb_subset, mode = "all", weighted = TRUE)

#As a network
u_ss_net <- network(urb_subset, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weight")


# Heatmap with grass_subset #

#Set margins
par(mar = c(2, 2, 2, 2))

# Set colorscale
colorscale_urb_ss <- turbo(1+max(igraph::degree(u_ss_igraph, mode = "total")))
u_map <- heatmap(urb_subset
                 , col = colorscale_urb_ss
                 , scale = "none"
                 , cexRow = 0.5
                 , cexCol = 0.5
)

# Add a title
title(main = paste("Urban Subset Interaction Heatmap"), cex.main = 1)  

legend_labels <- c("Low Degree", "Medium Degree", "High Degree")
legend_colors <- colorscale_urb_ss[c(1, 5, 10)]  

# Add a legend

legend("topleft",
       legend = legend_labels,
       fill = legend_colors,
       title = "Degree Level",
       border = "black",
       cex = 0.8)


# Interactive network - network3D #

# Ensure 'type' is defined and logical (bipartite: TRUE/FALSE)
V(urb_igraph)$type <- bipartite_mapping(urb_igraph)$type

# Convert igraph object to networkD3 format
net_d3 <- igraph_to_networkD3(urb_igraph, group = as.numeric(V(urb_igraph)$type))

forceNetwork(Links = net_d3$links, Nodes = net_d3$nodes,
             Source = "source", Target = "target",
             NodeID = "name", Group = "group",
             opacity = 0.9, zoom = TRUE)

# Bipartite layout

# Define layout
layout(matrix(1:2, ncol = 2), widths = c(4, 1))

# Set margins
par(mar = c(5, 4, 4, 1))

# Reorder rows and columns by their total interaction strength
row_order <- order(rowSums(urb_subset), decreasing = TRUE)
col_order <- order(colSums(urb_subset), decreasing = TRUE)

# Apply the order to the matrix
u_ss_sorted <- urb_subset[row_order, col_order]

# Calculate degree (number of connections) for each species
pollinator_deg <- rowSums(u_ss_sorted)
plant_deg <- colSums(u_ss_sorted)

# Map degrees to colors
pollinator_colors_u <- viridis(length(pollinator_deg))[rank(pollinator_deg)]
plant_colors_u <- viridis(length(plant_deg))[rank(plant_deg)]

# Plot with degree-based colors
plotweb(u_ss_sorted,
        method = "normal",
        text.rot = 90,
        col.low = pollinator_colors_u,
        col.high = plant_colors_u,
        labsize = 0.7,
        y.width.low = 0.1,
        y.width.high = 0.1)

# Add a title
title(main = paste("Urban Subset Bipartite Network"), cex.main = 1)  

# Add a legend

#Calculate degree
pollinator_deg <- rowSums(urb_subset)
plant_deg <- colSums(urb_subset)

#Cut into bins label
bin_labels <- c("Low", "Medium", "High")
pollinator_bins <- cut(pollinator_deg, breaks = 3, labels = bin_labels, include.lowest = TRUE)
plant_bins <- cut(plant_deg, breaks = 3, labels = bin_labels, include.lowest = TRUE)

#Assign colors
color_palette <- viridis(3)
pollinator_colors <- color_palette[as.numeric(pollinator_bins)]
plant_colors <- color_palette[as.numeric(plant_bins)]

par(mar = c(5, 0, 4, 2))  # no left margin, room on right
plot.new()
legend("center", legend = bin_labels, fill = color_palette,
       title = "Node Degree", border = NA)

# DOPi Urban Network Measures ----------


# calculate degree
urb_deg <- igraph::degree(urb_igraph, mode = "all")
# total degree
total_deg <- sum(urb_deg)
print(total_deg)
# make this measure numeric for calculating the mean
num_urb_deg <- as.numeric(urb_deg)
# mean degree 
urb_avg_deg <- mean(urb_deg)
# calculate and print
cat("Average degree:", urb_avg_deg, "\n")

# Nestedness for DOPi Urban ----------

compute_nestedness <- function(bipartite_network, mode = "total"){
  # Get number of rows and columns
  nrows <- nrow(bipartite_network)
  ncols <- ncol(bipartite_network)
  # Compute nestedness of rows
  nestedness_rows <- 0
  for(i in 1:(nrows-1)){
    for(j in (i+1): nrows){
      c_ij <- sum(bipartite_network[i,] * bipartite_network[j,])      # Number of interactions shared by i and j
      k_i <- sum(bipartite_network[i,])               # Degree of node i
      k_j <- sum(bipartite_network[j,])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      nestedness_rows <- nestedness_rows + o_ij
    }
  }
  
  # Compute nestedness of columns
  nestedness_cols <- 0
  for(i in 1: (ncols-1)){
    for(j in (i+1): ncols){
      c_ij <- sum(bipartite_network[,i] * bipartite_network[,j])      # Number of interactions shared by i and j
      k_i <- sum(bipartite_network[,i])               # Degree of node i
      k_j <- sum(bipartite_network[,j])               # Degree of node j
      if (k_i == 0 || k_j==0) {next}  # Handle case if a node is disconnected.
      o_ij <- c_ij / min(k_i, k_j)    # Overlap between i and j
      nestedness_cols <- nestedness_cols + o_ij         
    }
  }
  
  # Compute nestedness of the network
  nestedness <- (nestedness_rows + nestedness_cols) / ((nrows * (nrows - 1) / 2) + (ncols * (ncols - 1) / 2))
  nestedness_rows <- nestedness_rows / (nrows*(nrows-1)/2)
  nestedness_cols <- nestedness_cols / (ncols*(ncols-1)/2)
  if(mode=="rows") {return(nestedness_rows)}
  if(mode=="cols") {return(nestedness_cols)}
  return(nestedness)
}

# Nestedness reduced network for DOPi Urban  ----

class(urb_igraph)

# Create a unique list of vertex names from both columns of the edge list
vertex_names <- unique(c(as.character(urb_mat[,1]), as.character(urb_mat[,2])))

# Create a proper vertex data frame
vertices_df <- data.frame(name = vertex_names)

# Create the bipartite igraph object
g_urb <- graph_from_data_frame(urb_mat, vertices = vertices_df, directed = FALSE)

# Identify pollinators and plants based on your assumption
pollinators <- unique(urb_mat[,1])
plants <- unique(urb_mat[,2])

# Remove edges where both ends are pollinators or both are plants
urb_m_clean <- urb_mat[!(urb_mat[,1] %in% plants & urb_mat[,2] %in% plants) &
                               !(urb_mat[,1] %in% pollinators & urb_mat[,2] %in% pollinators), ]

# Recreate graph from cleaned data
vertex_names <- unique(c(as.character(urb_m_clean[,1]), as.character(urb_m_clean[,2])))
vertices_df <- data.frame(name = vertex_names)
g_urb <- graph_from_data_frame(urb_m_clean, vertices = vertices_df, directed = FALSE)

# Assign types again (pollinators = TRUE, plants = FALSE)
V(g_urb)$type <- V(g_urb)$name %in% pollinators

# Generate the matrix (Cargo as rows, Carrier as columns)
urb_matrix <- as_biadjacency_matrix(urb_igraph, types = V(urb_igraph)$type, sparse = FALSE)
#View(pathogen_matrix_red)
weighed_urb <- matrix(0, nrow = nrow(urb_matrix), ncol = ncol(urb_matrix))
weighed_urb[urb_matrix>0] <- 1
colnames(weighed_urb) <- colnames(urb_matrix)
rownames(weighed_urb) <- rownames(urb_matrix)

weighed_urb <- as.matrix(weighed_urb)

dim(weighed_urb)

# compute nestedness for plants, pollinators, and total 
compute_nestedness(weighed_urb, "cols")
compute_nestedness(weighed_urb, "rows")
compute_nestedness(weighed_urb, "total")

# Visualize

urban_mat <- as.matrix(as_adjacency_matrix(urb_igraph, sparse = FALSE))

image(t(urban_mat[nrow(urban_mat):1, ]), col = viridis(2), axes = FALSE)

# Basic matrix with labels
# Set wider margins: bottom, left, top, right
par(mar = c(5, 8, 10, 2))  # Adjust as needed for label length

#Order the nodes in the rows and columns in decreasing order
row_order <- order(rowSums(urban_mat), decreasing = TRUE)
col_order <- order(colSums(urban_mat), decreasing = TRUE)

mat_ordered <- urban_mat[row_order, col_order]

image(t(mat_ordered[nrow(mat_ordered):1, ]),
      col = viridis(2), axes = FALSE)

# Add box
box()

# Add column labels (top axis)
axis(3, at = seq(0, 1, length.out = ncol(mat_ordered)),
     labels = colnames(mat_ordered)[col_order], las = 2, cex.axis = 0.6)

# Add row labels (left axis)
axis(2, at = seq(0, 1, length.out = nrow(mat_ordered)),
     labels = rev(rownames(mat_ordered)[row_order]), las = 2, cex.axis = 0.6)

# Null Model Comparison Fixed-Fixed (FF) Preserves row and column sums --------------------

# shuffle while preserving row and column sums
randomize_matrix_ff <- function(mat, swaps = 1000) {
  for (s in 1:swaps) {
    # Pick two rows and two columns randomly
    r <- sample(1:nrow(mat), 2)
    c <- sample(1:ncol(mat), 2)
    
    # Get 2x2 submatrix
    sub <- mat[r, c]
    
    # Check for a swap pattern: [1,0][0,1] or [0,1][1,0]
    if ((sub[1,1] == 1 && sub[2,2] == 1 && sub[1,2] == 0 && sub[2,1] == 0) ||
        (sub[1,1] == 0 && sub[2,2] == 0 && sub[1,2] == 1 && sub[2,1] == 1)) {
      
      # Swap: toggle the 2x2 square
      mat[r[1], c[1]] <- 1 - mat[r[1], c[1]]
      mat[r[2], c[2]] <- 1 - mat[r[2], c[2]]
      mat[r[1], c[2]] <- 1 - mat[r[1], c[2]]
      mat[r[2], c[1]] <- 1 - mat[r[2], c[1]]
    }
  }
  return(mat)
}

# Run null model comparison
n_reps <- 100
null_nestedness <- numeric(n_reps)

for (i in 1:n_reps) {
  rand_mat <- randomize_matrix_ff(weighed_urb, swaps = 5000)
  null_nestedness[i] <- compute_nestedness(rand_mat, "total")
}

# Compare to observed
observed <- compute_nestedness(weighed_urb, "total")

# Plot
hist(null_nestedness, main = "Null Model Distribution of Nestedness",
     xlab = "Nestedness", col = "lightgray", breaks = 20)
abline(v = observed, col = "red", lwd = 2)
legend("topright", legend = paste("Observed =", round(observed, 3)), col = "red", lwd = 2)

# Calculate p-value
p_value <- mean(null_nestedness >= observed)
cat("p-value:", p_value, "\n")

# Robustness for DOPI Urban -----------

simulate_robustness <- function(mat, remove_from = "columns") {
  mat <- mat[rowSums(mat) > 0, colSums(mat) > 0]  # Clean matrix
  
  if (remove_from == "columns") {
    guild <- colnames(mat)
  } else {
    guild <- rownames(mat)
  }
  
  extinct_secondary <- numeric(length(guild))
  
  for (i in seq_along(guild)) {
    # Remove one species
    if (remove_from == "columns") {
      mat <- mat[, !(colnames(mat) %in% guild[i]), drop = FALSE]
    } else {
      mat <- mat[!(rownames(mat) %in% guild[i]), , drop = FALSE]
    }
    
    # Count secondaries: rows or columns that are now all zeros
    if (remove_from == "columns") {
      extinct <- sum(rowSums(mat) == 0)
    } else {
      extinct <- sum(colSums(mat) == 0)
    }
    
    extinct_secondary[i] <- extinct
  }
  
  # Plot robustness curve
  plot(extinct_secondary, type = "s", lwd = 2,
       xlab = paste("Number of pollinators removed"),
       ylab = "Cumulative plant extinctions",
       main = "Network Robustness of DOPi Urban Network")
}

# Measure Urban Robustness --------------

simulate_robustness(urb_mat, remove_from = "columns")

# --------------------------------------------
# 3 Network Comparison -----------

# Clear the environment and reload data

# Grasslands
grass <- read.csv("Grasslands.csv", row.names = 1)
grass_mat <- as.matrix(grass)
grass_net <- network(grass_mat, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weight")
grass_igraph <- graph_from_biadjacency_matrix(grass_mat, mode = "all", weighted = TRUE)
g_pollinators <- nrow(grass_mat)
g_plants <- ncol(grass_mat)
network.vertex.names(grass_net) <- c(rownames(grass_mat), colnames(grass_mat))

# Suburban
sub <- read.csv("Suburban.csv", row.names = 1)
sub_mat <- as.matrix(sub)
sub_net <- network(sub_mat, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weight")
sub_igraph <- graph_from_biadjacency_matrix(sub_mat, mode = "all", weighted = TRUE)
s_pollinators <- nrow(sub_mat)
s_plants <- ncol(sub_mat)
network.vertex.names(sub_net) <- c(rownames(sub_mat), colnames(sub_mat))

# Urban
urb <- read.csv("Urban.csv", row.names = 1)
urb_mat <- as.matrix(urb)
urb_net <- network(urb_mat, matrix.type = "bipartite", ignore.eval = FALSE, names.eval = "weight")
urb_igraph <- graph_from_biadjacency_matrix(urb_mat, mode = "all", weighted = TRUE)
u_pollinators <- nrow(urb_mat)
u_plants <- ncol(urb_mat)
network.vertex.names(urb_net) <- c(rownames(urb_mat), colnames(urb_mat))


# Degree Distribution ------

# Calculate degrees
# Calculate degree vectors
deg_grass <- c(rowSums(grass_mat), colSums(grass_mat))
deg_suburb <- c(rowSums(sub_mat), colSums(sub_mat))
deg_urb <- c(rowSums(urb_mat), colSums(urb_mat))

# Combine degree vectors
deg_list <- list(
  Grassland = c(rowSums(grass_mat), colSums(grass_mat)),
  Suburban  = c(rowSums(sub_mat), colSums(sub_mat)),
  Urban     = c(rowSums(urb_mat), colSums(urb_mat))
)

# Set colors
box_colors <- rev(viridis(3))

# Use log scale for y-axis by transforming the data
log_deg_list <- lapply(deg_list, function(x) log10(x[x > 0]))  # exclude zeros to avoid -Inf

# Plot
boxplot(log_deg_list,
        col = box_colors,
        main = "Degree Distributions by Network",
        ylab = "Log10(Degree)",
        xlab = "Network Type",
        outline = TRUE)

# Degree Distribution of generalists, intermediate, and specialists ------------

# Generalist: visits 15+ plant nodes
# Intermediate: visits 4-14 plant nodes
# Specialist: visits 1-3 plant nodes

# Grassland Network ---------------

# Categorize pollinators in the grassland network
pollinator_deg <- rowSums(grass_mat)

# Create categories
pollinator_category <- cut(
  pollinator_deg,
  breaks = c(-Inf, 3, 15, Inf),
  labels = c("Specialist", "Intermediate", "Generalist")
)

# Combine into a data frame for review
grassland_pollinator_summary <- data.frame(
  Pollinator = rownames(grass_mat),
  Degree = pollinator_deg,
  Category = pollinator_category
)

# View summary
print(grassland_pollinator_summary)

# Count how many pollinators fall into each category
category_counts <- table(grassland_pollinator_summary$Category)

print(category_counts)

# Create a color palette
category_colors <- viridis(length(category_counts))

# Plot the barplot
barplot(
  category_counts,
  col = category_colors,
  main = "Grassland Pollinator Categories",
  ylab = "Number of Pollinators",
  xlab = "Category"
)


# Suburban Network --------------

# Categorize pollinators in the suburban network
pollinator_deg_sub <- rowSums(sub_mat)

# Create categories
pollinator_category_sub <- cut(
  pollinator_deg_sub,
  breaks = c(-Inf, 3, 15, Inf),
  labels = c("Specialist", "Intermediate", "Generalist")
)

# Combine into a data frame for review
suburban_pollinator_summary <- data.frame(
  Pollinator = rownames(sub_mat),
  Degree = pollinator_deg_sub,
  Category = pollinator_category_sub
)

# View summary
print(suburban_pollinator_summary)

# Count how many pollinators fall into each category
category_counts_sub <- table(suburban_pollinator_summary$Category)

print(category_counts_sub)

# Create a color palette
category_colors <- viridis(length(category_counts_sub))

# Plot the barplot
barplot(
  category_counts_sub,
  col = category_colors,
  main = "Suburban Pollinator Categories",
  ylab = "Number of Pollinators",
  xlab = "Category"
)


# Urban Network ---------------

# Categorize pollinators in the Urban network
pollinator_deg_urb <- rowSums(urb_mat)

# Create categories
pollinator_category_urb <- cut(
  pollinator_deg_urb,
  breaks = c(-Inf, 3, 15, Inf),
  labels = c("Specialist", "Intermediate", "Generalist")
)

# Combine into a data frame for review
Urban_pollinator_summary <- data.frame(
  Pollinator = rownames(urb_mat),
  Degree = pollinator_deg_urb,
  Category = pollinator_category_urb
)

# View summary
print(Urban_pollinator_summary)

# Count how many pollinators fall into each category
category_counts_urb <- table(Urban_pollinator_summary$Category)

print(category_counts_urb)

# Create a color palette
category_colors <- viridis(length(category_counts_urb))

# Plot the barplot
barplot(
  category_counts_urb,
  col = category_colors,
  main = "Urban Pollinator Categories",
  ylab = "Number of Pollinators",
  xlab = "Category"
)

# Test for significance with a chi-squared test ----------

## This is not the correct use of the chi-squared test for my question
## the code has been left in because I use parts of it in the correst
## test below

# Classify pollinators by types

classify_pollinators <- function(mat) {
  deg <- rowSums(mat)  # pollinators are in rows
  cut(deg,
      breaks = c(-1, 3, 15, Inf),
      labels = c("Specialist", "Intermediate", "Generalist"))
}

# Apply to networks

grass_types <- classify_pollinators(grass_mat)
sub_types   <- classify_pollinators(sub_mat)
urb_types   <- classify_pollinators(urb_mat)

# Make a table with the counts

grass_tab <- table(grass_types)
sub_tab   <- table(sub_types)
urb_tab   <- table(urb_types)

# Combine into one table

all_tab <- rbind(Grassland = grass_tab,
                 Suburban  = sub_tab,
                 Urban     = urb_tab)
print(all_tab)

# Run the chi-squared test

chisq.test(all_tab)

# Chi-squared test for each category of pollinator 

# Reuse the contingency table
all_tab <- rbind(Grassland = table(classify_pollinators(grass_mat)),
                 Suburban  = table(classify_pollinators(sub_mat)),
                 Urban     = table(classify_pollinators(urb_mat)))

# Transpose to have categories as rows
all_tab_t <- t(all_tab)

# Run chi-squared test for each category
for (category in rownames(all_tab_t)) {
  cat("\nChi-squared test for category:", category, "\n")
  test_result <- chisq.test(all_tab_t[category, ])
  print(test_result)
}

# Pairwise chi-squared test ------------------------

# Measure the differences between pollinator categories across the urbanization
# gradient (grassland vs suburban, grassland vs urban, suburban vs urban)

# Combine counts into a dataframe
pollinator_class_df <- data.frame(
  Network = rep(c("Grassland", "Suburban", "Urban"), each = 3),
  Type = rep(c("Specialist", "Intermediate", "Generalist"), times = 3),
  Count = c(grass_tab, sub_tab, urb_tab)
)

# Expand the data so each pollinator is a row
expanded_df <- pollinator_class_df[rep(1:nrow(pollinator_class_df), pollinator_class_df$Count), 1:2]

# Create the contingency table of counts
pollinator_counts <- table(expanded_df$Network, expanded_df$Type)

# Function to run chi-squared test for a single pollinator type
run_pairwise_chi_sq <- function(type) {
  message("\nChi-squared test for pollinator type: ", type)
  
  # Subset the data for the specific type
  type_data <- pollinator_counts[, type]
  
  # Create a 2xN table for each pairwise comparison
  habitats <- rownames(pollinator_counts)
  
  for (i in 1:(length(habitats) - 1)) {
    for (j in (i + 1):length(habitats)) {
      test_matrix <- rbind(
        c(type_data[habitats[i]], sum(pollinator_counts[habitats[i], ]) - type_data[habitats[i]]),
        c(type_data[habitats[j]], sum(pollinator_counts[habitats[j], ]) - type_data[habitats[j]])
      )
      rownames(test_matrix) <- c(habitats[i], habitats[j])
      colnames(test_matrix) <- c(type, "Other")
      
      cat("\nComparing", habitats[i], "vs", habitats[j], "\n")
      print(test_matrix)
      print(chisq.test(test_matrix))
    }
  }
}

# Run for each category
pollinator_types <- colnames(pollinator_counts)
lapply(pollinator_types, run_pairwise_chi_sq)



# Measuring Robustness of each network -------------

# Reset Environment between each robustness measure 
# and run # 3 network comparison block

# Need to input a binary adj matrix
#class(grass_mat)
#str(grass_mat)

# ---------------------------------------------
# Grassland Network - random extinction -------------

# Simulate extinction function
simulate_extinction <- function(net, order = NULL) {
  if (is.null(order)) {
    order <- sample(1:nrow(net))  # random pollinator removal
  }
  secondary_ext <- numeric(length(order))
  temp_net <- net
  for (i in seq_along(order)) {
    temp_net[order[i], ] <- 0  # remove one pollinator
    secondary_ext[i] <- sum(colSums(temp_net) == 0)  # count plant extinctions
  }
  return(secondary_ext)
}

# Number of simulations
n_sim <- 100

# Run simulations on grass_mat
set.seed(123)
results <- matrix(0, nrow = n_sim, ncol = nrow(grass_mat))
for (i in 1:n_sim) {
  results[i, ] <- simulate_extinction(grass_mat)
}

# Calculate average secondary extinctions
mean_ext <- colMeans(results)

# Plot the robustness curve
plot(mean_ext, type = "l", lwd = 2, col = "forestgreen",
     xlab = "Number of Pollinators Removed",
     ylab = "Cumulative Plant Extinctions",
     main = "Grassland Network Robustness Curve - Random Removal Order")

# Calculate a robustness index (area under curve, normalized)
robustness_index <- sum(mean_ext) / (nrow(grass_mat) * ncol(grass_mat))
cat("Grassland Robustness Index:", robustness_index, "\n")



# Grassland Network - ordered extinction ----------------

# Simulate extinction function, removing from high degree nodes first ---------
simulate_extinction <- function(net, order = NULL) {
  if (is.null(order)) {
    deg <- rowSums(net)  # Degree for pollinators
    order <- order(deg, decreasing = TRUE) # Decreasing from high to low
  }
  secondary_ext <- numeric(length(order))
  temp_net <- net
  for (i in seq_along(order)) {
    temp_net[order[i], ] <- 0  # remove one pollinator
    secondary_ext[i] <- sum(colSums(temp_net) == 0)  # count plant extinctions
  }
  return(secondary_ext)
}

# Number of simulations
n_sim <- 100

# Run simulations on grass_mat
set.seed(123)
results <- matrix(0, nrow = n_sim, ncol = nrow(grass_mat))
for (i in 1:n_sim) {
  results[i, ] <- simulate_extinction(grass_mat)
}

# Calculate average secondary extinctions
mean_ext <- colMeans(results)

# Plot the robustness curve -------
plot(mean_ext, type = "l", lwd = 2, col = "forestgreen",
     xlab = "Number of Pollinators Removed",
     ylab = "Cumulative Plant Extinctions",
     main = "Grassland Network Robustness Curve - High to Low Removal Order")

# Calculate a robustness index (area under curve, normalized)
robustness_index <- sum(mean_ext) / (nrow(grass_mat) * ncol(grass_mat))
cat("Grassland Robustness Index:", robustness_index, "\n")


# Simulate extinction function, removing from low degree nodes first -----------
simulate_extinction <- function(net, order = NULL) {
  if (is.null(order)) {
    deg <- rowSums(net)  # Degree for pollinators
    order <- order(deg, decreasing = FALSE) # Decreasing from high to low
  }
  secondary_ext <- numeric(length(order))
  temp_net <- net
  for (i in seq_along(order)) {
    temp_net[order[i], ] <- 0  # remove one pollinator
    secondary_ext[i] <- sum(colSums(temp_net) == 0)  # count plant extinctions
  }
  return(secondary_ext)
}

# Number of simulations
n_sim <- 100

# Run simulations on grass_mat
set.seed(123)
results <- matrix(0, nrow = n_sim, ncol = nrow(grass_mat))
for (i in 1:n_sim) {
  results[i, ] <- simulate_extinction(grass_mat)
}

# Calculate average secondary extinctions
mean_ext <- colMeans(results)

# Plot the robustness curve --------------------
plot(mean_ext, type = "l", lwd = 2, col = "forestgreen",
     xlab = "Number of Pollinators Removed",
     ylab = "Cumulative Plant Extinctions",
     main = "Grassland Network Robustness Curve - Low to High Removal Order")

# Calculate a robustness index (area under curve, normalized)
robustness_index <- sum(mean_ext) / (nrow(grass_mat) * ncol(grass_mat))
cat("Grassland Robustness Index:", robustness_index, "\n")


# ---------------------------------------------
# Suburban Network - random extinction -------------

# Simulate extinction function
simulate_extinction <- function(net, order = NULL) {
  if (is.null(order)) {
    order <- sample(1:nrow(net))  # random pollinator removal
  }
  secondary_ext <- numeric(length(order))
  temp_net <- net
  for (i in seq_along(order)) {
    temp_net[order[i], ] <- 0  # remove one pollinator
    secondary_ext[i] <- sum(colSums(temp_net) == 0)  # count plant extinctions
  }
  return(secondary_ext)
}


# Number of simulations
n_sim <- 100

# Run simulations on sub_mat
set.seed(123)
results <- matrix(0, nrow = n_sim, ncol = nrow(sub_mat))
for (i in 1:n_sim) {
  results[i, ] <- simulate_extinction(sub_mat)
}

# Calculate average secondary extinctions
mean_ext <- colMeans(results)

# Plot the robustness curve -------
plot(mean_ext, type = "l", lwd = 2, col = "forestgreen",
     xlab = "Number of Pollinators Removed",
     ylab = "Cumulative Plant Extinctions",
     main = "Suburban Network Robustness Curve - Random Removal Order")

# Calculate a robustness index (area under curve, normalized)
robustness_index <- sum(mean_ext) / (nrow(sub_mat) * ncol(sub_mat))
cat("Suburban Robustness Index:", robustness_index, "\n")


# Suburban Network - ordered extinction ----------------


# Simulate extinction function, removing from high degree nodes first ----------
simulate_extinction <- function(net, order = NULL) {
  if (is.null(order)) {
    deg <- rowSums(net)  # Degree for pollinators
    order <- order(deg, decreasing = TRUE) # Decreasing from high to low
  }
  secondary_ext <- numeric(length(order))
  temp_net <- net
  for (i in seq_along(order)) {
    temp_net[order[i], ] <- 0  # remove one pollinator
    secondary_ext[i] <- sum(colSums(temp_net) == 0)  # count plant extinctions
  }
  return(secondary_ext)
}

# Number of simulations
n_sim <- 100

# Run simulations on sub_mat
set.seed(123)
results <- matrix(0, nrow = n_sim, ncol = nrow(sub_mat))
for (i in 1:n_sim) {
  results[i, ] <- simulate_extinction(sub_mat)
}

# Calculate average secondary extinctions
mean_ext <- colMeans(results)

# Plot the robustness curve -------
plot(mean_ext, type = "l", lwd = 2, col = "forestgreen",
     xlab = "Number of Pollinators Removed",
     ylab = "Cumulative Plant Extinctions",
     main = "Suburban Network Robustness Curve - High to Low Removal Order")

# Calculate a robustness index (area under curve, normalized)
robustness_index <- sum(mean_ext) / (nrow(sub_mat) * ncol(sub_mat))
cat("Suburban Robustness Index:", robustness_index, "\n")

# Simulate extinction function, removing from low degree nodes first -----------
simulate_extinction <- function(net, order = NULL) {
  if (is.null(order)) {
    deg <- rowSums(net)  # Degree for pollinators
    order <- order(deg, decreasing = FALSE) # Decreasing from low to high
  }
  secondary_ext <- numeric(length(order))
  temp_net <- net
  for (i in seq_along(order)) {
    temp_net[order[i], ] <- 0  # remove one pollinator
    secondary_ext[i] <- sum(colSums(temp_net) == 0)  # count plant extinctions
  }
  return(secondary_ext)
}

# Number of simulations
n_sim <- 100

# Run simulations on sub_mat
set.seed(123)
results <- matrix(0, nrow = n_sim, ncol = nrow(sub_mat))
for (i in 1:n_sim) {
  results[i, ] <- simulate_extinction(sub_mat)
}

# Calculate average secondary extinctions
mean_ext <- colMeans(results)

# Plot the robustness curve -------
plot(mean_ext, type = "l", lwd = 2, col = "forestgreen",
     xlab = "Number of Pollinators Removed",
     ylab = "Cumulative Plant Extinctions",
     main = "Suburban Network Robustness Curve - Low to High Removal Order")

# Calculate a robustness index (area under curve, normalized)
robustness_index <- sum(mean_ext) / (nrow(sub_mat) * ncol(sub_mat))
cat("Suburban Robustness Index:", robustness_index, "\n")

# ---------------------------------------------
# Urban Network - random extinction -------------

# Simulate extinction function
simulate_extinction <- function(net, order = NULL) {
  if (is.null(order)) {
    order <- sample(1:nrow(net))  # random pollinator removal
  }
  secondary_ext <- numeric(length(order))
  temp_net <- net
  for (i in seq_along(order)) {
    temp_net[order[i], ] <- 0  # remove one pollinator
    secondary_ext[i] <- sum(colSums(temp_net) == 0)  # count plant extinctions
  }
  return(secondary_ext)
}


# Number of simulations
n_sim <- 100

# Run simulations on urb_mat
set.seed(123)
results <- matrix(0, nrow = n_sim, ncol = nrow(urb_mat))
for (i in 1:n_sim) {
  results[i, ] <- simulate_extinction(urb_mat)
}

# Calculate average secondary extinctions
mean_ext <- colMeans(results)

# Plot the robustness curve -------
plot(mean_ext, type = "l", lwd = 2, col = "forestgreen",
     xlab = "Number of Pollinators Removed",
     ylab = "Cumulative Plant Extinctions",
     main = "Urban Network Robustness Curve - Random Removal Order")

# Calculate a robustness index (area under curve, normalized)
robustness_index <- sum(mean_ext) / (nrow(urb_mat) * ncol(urb_mat))
cat("Urban Robustness Index:", robustness_index, "\n")


# Urban Network - ordered extinction ----------------


# Simulate extinction function, removing from high degree nodes first ----------
simulate_extinction <- function(net, order = NULL) {
  if (is.null(order)) {
    deg <- rowSums(net)  # Degree for pollinators
    order <- order(deg, decreasing = TRUE) # Decreasing from high to low
  }
  secondary_ext <- numeric(length(order))
  temp_net <- net
  for (i in seq_along(order)) {
    temp_net[order[i], ] <- 0  # remove one pollinator
    secondary_ext[i] <- sum(colSums(temp_net) == 0)  # count plant extinctions
  }
  return(secondary_ext)
}

# Number of simulations
n_sim <- 100

# Run simulations on urb_mat
set.seed(123)
results <- matrix(0, nrow = n_sim, ncol = nrow(urb_mat))
for (i in 1:n_sim) {
  results[i, ] <- simulate_extinction(urb_mat)
}

# Calculate average secondary extinctions
mean_ext <- colMeans(results)

# Plot the robustness curve -------
plot(mean_ext, type = "l", lwd = 2, col = "forestgreen",
     xlab = "Number of Pollinators Removed",
     ylab = "Cumulative Plant Extinctions",
     main = "Urban Network Robustness Curve - High to Low Removal Order")

# Calculate a robustness index (area under curve, normalized)
robustness_index <- sum(mean_ext) / (nrow(urb_mat) * ncol(urb_mat))
cat("Urban Robustness Index:", robustness_index, "\n")

# Simulate extinction function, removing from low degree nodes first -----------
simulate_extinction <- function(net, order = NULL) {
  if (is.null(order)) {
    deg <- rowSums(net)  # Degree for pollinators
    order <- order(deg, decreasing = FALSE) # Decreasing from low to high
  }
  secondary_ext <- numeric(length(order))
  temp_net <- net
  for (i in seq_along(order)) {
    temp_net[order[i], ] <- 0  # remove one pollinator
    secondary_ext[i] <- sum(colSums(temp_net) == 0)  # count plant extinctions
  }
  return(secondary_ext)
}

# Number of simulations
n_sim <- 100

# Run simulations on urb_mat
set.seed(123)
results <- matrix(0, nrow = n_sim, ncol = nrow(urb_mat))
for (i in 1:n_sim) {
  results[i, ] <- simulate_extinction(urb_mat)
}

# Calculate average secondary extinctions
mean_ext <- colMeans(results)

# Plot the robustness curve -------
plot(mean_ext, type = "l", lwd = 2, col = "forestgreen",
     xlab = "Number of Pollinators Removed",
     ylab = "Cumulative Plant Extinctions",
     main = "Urban Network Robustness Curve - Low to High Removal Order")

# Calculate a robustness index (area under curve, normalized)
robustness_index <- sum(mean_ext) / (nrow(urb_mat) * ncol(urb_mat))
cat("Urban Robustness Index:", robustness_index, "\n")




