## This script is created for infoOmics course for Network data analysis module
## We are going to analyse contact network of students in the elementary school
# Install packages
install.packages("igraphinshiny")
library(igraphinshiny)
plotDemo()
install.packages(c("igraph","readr","tidyr","RColorBrewer"))
#==================================================================#
#==================================================================#
# In this section, you will retrieve csv format network data file  #
# to the R studio as a dataframe and convert it into the igraph    #
# object.                                                          #
#==================================================================#

#1. Read data (csv files) from your computer
setwd("Desktop/class/data")
library(readr)
contacts <- read_csv("contacts.csv")
students <- read_csv("nodes.csv")
#==================================================================#
#2. Create edge list
edge_list <- as.data.frame(table(contacts)) # Create an edge weight column named "Freq"
edge_list <- subset(edge_list,Freq > 0) # Delete all the edges having weight equal to 0
#==================================================================#
#3. Create an igraph object from the dataframes
library(igraph) #retrieve package

school_net <- graph_from_data_frame(edge_list, directed = FALSE, vertices = students)
class(school_net)
E(school_net)$weight <- E(school_net)$Freq # Assigning edge attribute to each edge
#==================================================================#
#=================== Explore Your igraph Data =====================#
#==================================================================#
# In this section, you will explore the igraph object named        #
# "school_net". You will see the summary of the igraph object with    #
# its data structure.                                              #
#==================================================================#

#1. igraph summary
gsize(school_net)
gorder(school_net)

#4. Attributes
V(school_net)$Gender
V(school_net)$Gender[V(school_net)$Gender=='Unknown'] <- NA
V(school_net)$Class
E(school_net)$weight


#5. Adjacency matrix
school_net[c(1:10),c(1:10)]

#6. Network Density
edge_density(school_net) # Global density
A1<-induced_subgraph(school_net, V(school_net)[Class=="1A"], impl=c("auto")) # Subgraphing into each class
edge_density(A1) # Class level density

#==================================================================#
#===================== Measuring Centrality =======================#
#==================================================================#
# In this section, you will measure the centrality of the igraph   #
# object, "school_net". You will be able to see how the theoretical   #
# concept of each centrality such as degree, closeness, and      #
# betweenness centrality is measured by the igraph.                #
#==================================================================#

#1. Degree centrality
school_net_deg <- degree(school_net,mode=c("All"))
V(school_net)$degree <- school_net_deg
V(school_net)$degree
which.max(school_net_deg)

#2.Closeness centrality: 
#Closeness centrality indicates how close a node is to all other nodes in the network. 
#It is calculated as the average of the shortest path length from the node to every other node in the network.
school_net_clos <- closeness(school_net)
V(school_net)$close <- school_net_clos
which.max(school_net_clos)

#3. Betweenness centrality
#Each node receives a score, based on the number of shortest paths that pass through the node. 
#Nodes that more frequently lie on shortest paths between other nodes will have higher betweenness centrality scores
school_net_bw <- betweenness(school_net, directed = FALSE)
V(school_net)$betweenness <- school_net_bw
which.max(school_net_bw)

DF <- as_long_data_frame(school_net) #convert igraph object into data-frame


#==================================================================#
#===================== Network Visualization ======================#
#==================================================================#

#1. Plotting a network with the degree centrality

set.seed(1001)
library(RColorBrewer) # This is the color library
pal <- brewer.pal(length(unique(V(school_net)$Class)), "Set3") # Vertex color assigned per each class number
plot(school_net,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(school_net, "Class")))],
     vertex.size = sqrt(school_net_deg)/3, edge.width=sqrt(E(school_net)$weight/800),
     layout = layout.fruchterman.reingold)

#1. Plotting a network with the closeness centrality

set.seed(1001)
plot(school_net,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(school_net, "Class")))],
     vertex.size = sqrt(school_net_clos)*50, edge.width=sqrt(E(school_net)$weight/800),
     layout = layout.fruchterman.reingold)

#2. Plotting a network with the betweenness centrality

set.seed(1001)
plot(school_net,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(school_net, "Class")))],
     vertex.size = sqrt(school_net_bw)/3, edge.width=sqrt(E(school_net)$weight/800),
     layout = layout.fruchterman.reingold)

#3. Plotting a scatter plot to see the correlation

#3.1. between degree and betweenness centrality

plot(V(school_net)$degree, V(school_net)$betweenness)

#3.2. between degree and eigenvector centrality

plot(V(school_net)$degree, V(school_net)$close)

#==================================================================#
#====================== Community Detection =======================#
#==================================================================#
#Community structure is an important feature of complex networks, 
#which indicates the fact that nodes are gathered into several groups. 
#Each group is a community of nodes where the density of edges within communities 
#is higher than among communities
#divides network into groups of nodes with dense connections internally and sparser connections between groups.
#1. Louvain clustering
lc <- cluster_louvain(school_net) # Create a cluster based on the Louvain method
communities(lc) # You can check which vertices belongs to which clusters.

#2. Plotting the Betweenness Centrality network with the community detection

set.seed(1001) # To duplicate the computer process and create exactly the same network repetitively you should set the seed.
plot(lc, school_net, edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(school_net, "Class")))],
     vertex.size = sqrt(school_net_bw)/3, edge.width=sqrt(E(school_net)$weight/800),
     layout = layout.fruchterman.reingold)
