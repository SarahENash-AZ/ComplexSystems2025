# Header ------------- 
# Spring 2025 Complex Systems R Script
# Sarah Nash
# sarahenash@arizona.edu
# With help from Anna Dornhaus and Emiliano Calvo-Alcaniz

# Please note: with my limited R coding experience, I was helped immensely by Emiliano,
# much of my code was influenced by how he wrote his. As such I need to give proper credit. 
# I would not have been able to complete this project without his help. 

# Set working directory --------------

 setwd("C:/Users/sarah.nash/OneDrive - USDA/Desktop/ComplexSystems2025/")
# setwd("C:/Users/senas/OneDrive - University of Arizona/Desktop/Complex Systems")


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
library(network) 
library(circlize)


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


# Create an object that holds the number of communications each dealer has recieved aka edge weight
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

# calculate closeness _ HELP
coke_closeness <- igraph::closeness(cokeg)
# mean of closeness
coke_avg_closeness <- mean(coke_closeness)
# calculate and print
cat("Average closeness:", coke_avg_closeness, "\n")

# -- Betweenness -- Also help
coke_betweenness <- igraph::betweenness(cokeg)
# mean of the betweenness 
coke_avg_betweenness <- mean(coke_betweenness)
# Calculate it and print it out
cat("Average betweenness:", coke_avg_betweenness, "\n")

## -- clustering coefficient -- 

# Calculate the clustering coefficient
coke_cc <- transitivity(cokeg, type = "global")
# print it out
cat("Clustering coefficient:", coke_cc, "\n")

# Create a random network [sample_gnm()] that has the same number of nodes 
# [vcount()] and edges [ecount()]
randomcaviar_cc1a <- sample_gnm(vcount(simplecaviar_igraph), ecount(caviar_igraph)  
                                , directed = TRUE, loops = FALSE)
# Calculate the Clustering coefficient of the random network
random_caviar_cc1 <- transitivity(randomcaviar_cc1a, type = "global")
# Calculate it and print it out
cat("Clustering coefficient of a RANDOM network:", random_caviar_cc1, "\n")



## MOTIFS

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
        # color it using the mako color scale
        , col = color_distributions
        # idk what this does but it's necessary
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

## ASSORTATIVITY

# calculate the assortativity of the degree
caviar_assortativity <- assortativity(simplecaviar_igraph, values = caviar_deg, directed = TRUE)
# Calculate it and print it out
cat("Assortativity of this network:", caviar_assortativity, "\n")

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

hen_sna <- asNetwork(hen)

# Initial Plots

heatmap(hen_adj_matrix)

plot(hen)

plot(hen_sna)


# Initial plots

heatmap(as.matrix(hen_adj_matrix)
        , Rowv = NA
        , Colv = NA
        , col = magma(50)
        , scale = "none"
        , margins = c(8,6)
)

# The above does not show a legend. This is the basic version, but note that if
# the values in the matrix are not scaled, the 'middle' value is not the average 
# or median of the matrix values, just the middle color. 
legend("bottomright"
       , legend = c("min", "middle", "max")
       , fill = magma(3)
)


gplot(hen_sna, diag=TRUE, vertex.col=1, edge.lwd=1, loop.cex=2)

# Measure the degree 
hendegree <- igraph::degree(hen, mode = "in") 
# Transform to numeric so that colors can be assigned later
hendegree_numeric <- as.numeric(unlist(hendegree))  
# scaling the degree in order to color nodes
hen_max <- max(hendegree_numeric)
h_scaled_calculations <- hendegree/hen_max
h_scaled <- as.numeric(ceiling(h_scaled_calculations * 100))
hen_color <- rev(magma(10))
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

legend_labels <- c("Low Degree", "Medium Degree", "High Degree")
legend_colors <- hen_color[c(1, 5, 10)]  

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
                                 , vertex.label.color = "gray40"
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

legend_labels <- c("Low Communication", "Medium Communication", "High Communication")
legend_colors <- coke_color[c(1, 50, 100)] 

legend("bottomright",
       legend = legend_labels,
       fill = legend_colors,
       title = "Communication Level",
       border = "black",
       cex = 0.8)


# Hen Network Degree Measures ---------


# -- Degree -- 

# calculate degree
hen_deg <- igraph::degree(hen, mode = "all")
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

# Calculate the clustering coefficient
hen_cc <- transitivity(hen, type = "global")
# print it out
cat("Clustering coefficient:", hen_cc, "\n")

# Create a random network [sample_gnm()] that has the same number of nodes 
# [vcount()] and edges [ecount()]
randomhen_cc1a <- sample_gnm(vcount(hen), ecount(hen)  
                                , directed = TRUE, loops = FALSE)
# Calculate the Clustering coefficient of the random network
randomhen_cc1 <- transitivity(randomhen_cc1a, type = "global")
# Calculate it and print it out
cat("Clustering coefficient of a RANDOM network:", randomhen_cc1, "\n")



## MOTIFS

# Calculate the motifs of the dataset for groups of 3 individuals
hen_mot <- motifs(hen, size = 3)
# Set the aesthetic parameters & layout
par(mar=c(0,1,0,1), oma = c(1,2,1,1), xpd=TRUE)# bottom, left, top, right
layout(matrix(c(1,1,1,1,0,1,0,0,1,2,1,4,1,1,3,1,1,1,0,2,2,4,5,0,7,2,9,1,11,2,13,14,15,22,26,1)
#              , nrow = 2, ncol = 36, byrow = T)
#       , heights=c(4,1)) 
# set the colors for this graph, matching the color scale used earlier
color_distributions <- magma(50)
# create a barplot showing the number of motifs out of each possible motif
barplot(hen_mot
        # color it using the mako color scale
        , col = color_distributions
        # idk what this does but it's necessary
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

## ASSORTATIVITY

# calculate the assortativity of the degree
caviar_assortativity <- assortativity(simplecaviar_igraph, values = caviar_deg, directed = TRUE)
# Calculate it and print it out
cat("Assortativity of this network:", caviar_assortativity, "\n")

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

# -- Diameter

# Calculate the diameter
coke_diameter <- diameter(cokeg, directed = TRUE)
# Print it out
cat("Diameter of this network:", coke_diameter, "\n")



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

diptera_m <- as.matrix(diptera_pollinators)

#diptera_sna <- asNetwork(diptera_m) - # Help

#chordDiagram(as.matrix(diptera_pollinators)
#             , cex = 0.5
#             )

Dipterans <- graph_from_biadjacency_matrix(as.matrix(diptera_pollinators), directed = FALSE, weighted = TRUE)

plot(Dipterans)


#Heatmap of Dipteran-Plant interactions

par(mar = c(2, 2, 2, 2))

colorscale_dips <- turbo(1+max(igraph::degree(Dipterans, mode = "total")))
d_map <- heatmap(diptera_m
                 , col = colorscale_dips
                 , scale = "none"
                 , cexRow = 0.5
                 , cexCol = 0.5
                # , Rowv = TRUE
                 #, Colv = TRUE
                 #, revC = TRUE
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

# Nestedness 

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

# Nestedness reduced network-----

class(diptera_m)

# Create the bipartite igraph object
g_dipt <- graph_from_data_frame(diptera_m, 
                               vertices = data.frame(diptera_m),
                               directed = FALSE)

# Generate the matrix (Cargo as rows, Carrier as columns)
pathogen_matrix_red <- as_biadjacency_matrix(g_red, types = V(g_red)$type, sparse = FALSE)
#View(pathogen_matrix_red)
unweighed_poll_red <- matrix(0, nrow = nrow(pathogen_matrix_red), ncol = ncol(pathogen_matrix_red))
unweighed_poll_red[pathogen_matrix_red>0] <- 1
colnames(unweighed_poll_red) <- colnames(pathogen_matrix_red)
rownames(unweighed_poll_red) <- rownames(pathogen_matrix_red)

compute_nestedness(unweighed_poll_red, "cols")
compute_nestedness(unweighed_poll_red, "rows")
compute_nestedness(unweighed_poll_red, "total")



# Dopi Focus Network ------------



#Nestedness
SpeciesI <- read_xlsx("SpeciesInteractions_EID2.xlsx")
data <- SpeciesI
edges_red <- data %>%
  select(`Cargo classification`, `Carrier classification`)
#Renaming Columns
names(edges_red)[names(edges_red) == "Cargo classification"] <- "Cargo.classification"
names(edges_red)[names(edges_red) == "Carrier classification"] <- "Carrier.classification"

#Get Uniques
all_nodes_red <- unique(c(edges_red$Cargo.classification, edges_red$Carrier.classification))

# Define node types: TRUE for Cargo (rows), FALSE for Carrier (columns)
node_types_red <- ifelse(all_nodes_red %in% edges_red$Cargo.classification, TRUE, FALSE)



