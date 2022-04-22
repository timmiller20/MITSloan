#########################
# TIM MILLER
# ANALYTICS EDGE
# HW7
#########################

#----------------------------- PROBLEM 1A -----------------------------#

# Packages
# install.packages("maps")
library(dplyr)
library(maps)
library(igraph)

# Load data
senators <- read.csv("senators.csv")
senatorLinks <- read.csv("senateCosponsorship.csv", stringsAsFactors=FALSE)
G <- graph.data.frame(senatorLinks, directed=FALSE, senators)
# add Senators' names to edges
G = set_edge_attr(G, "name1", index = E(G), senators$name[(senatorLinks$V1)])
G = set_edge_attr(G, "name2", index = E(G), senators$name[(senatorLinks$V2)])
# add Senators' party to edges
G = set_edge_attr(G, "party1", index = E(G), senators$party[(senatorLinks$V1)])
G = set_edge_attr(G, "party2", index = E(G), senators$party[(senatorLinks$V2)])

# Extract connected sub-graph
comp = components(G)
in.max.comp = comp$membership == which.max(comp$csize)
sg = induced_subgraph(G, in.max.comp)
sg.Senators = senators[in.max.comp,]

# Adjust colors

col <- ifelse(V(sg)$party == "R", "red", 
              ifelse(V(sg)$party == "D", "blue", 
              ifelse(V(sg)$party == "I", "yellow","black")))

# Basic plot with updated colors
plot(sg, 
     vertex.size = 50, # you can modify this so that the points are visible
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     vertex.color = col, # update vertex colors based on party affiliation
     edge.curved = TRUE, # make the edges curved
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y) # fix the upper/lower bounds of the plot
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states

#----------------------------- PROBLEM 1B -----------------------------#

threshold = 70

# Basic plot with updated edges
plot(sg, 
     vertex.size = 50, # you can modify this so that the points are visible
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     vertex.color = col, # update vertex colors based on party affiliation
     edge.lty = ifelse(E(sg)$n>=threshold , "solid","blank"), # solid lines for edges with more than 'threshold' cosponsorships
     edge.curved = TRUE, # make the edges curved
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y) # fix the upper/lower bounds of the plot
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states

#----------------------------- PROBLEM 1C -----------------------------#

# Independent Senators
I.senators = senators$name[senators$party=="I"]

# Basic plot with updated edges
plot(sg, 
     vertex.size = 50, # you can modify this so that the points are visible
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     edge.lty = ifelse("I"==E(sg)$party1|"I"==E(sg)$party2, "solid", "blank"), # solid lines for edges that touch an "Independent" vertex, other edges are blank
     edge.color = ifelse(E(sg)$name1==I.senators[1]|E(sg)$name2==I.senators[1], "orange", "purple"), # orange lines for the first senator, purple for the other edges
     edge.width = 0.1*E(sg)$n, # edge width proportional to n
     edge.curved = TRUE, # make the edges curved
     vertex.color = col, # update vertex colors based on party affiliation
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y) # fix the upper/lower bounds of the plot
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states


#----------------------------- PROBLEM 1D -----------------------------#

# Calculate degree centrality
dg = degree(sg)
sg.Senators$degree <- dg

# Calculate closeness centrality
cl = closeness(sg)
sg.Senators$closeness <- cl

# Calculate betweeness
bn = betweenness(sg)
sg.Senators$betweeness <- bn


# Senators with top 10 degree centrality
head(sg.Senators[order(sg.Senators$degree, decreasing = TRUE),], 10)

# Senators with top 10 closeness centrality
head(sg.Senators[order(sg.Senators$closeness, decreasing = TRUE),], 10)

# Senators with top 10 betweeness
head(sg.Senators[order(sg.Senators$betweeness, decreasing = TRUE),], 10)


# Basic plot with size for degree centrality
plot(sg, 
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     vertex.color = col, # update vertex colors based on party affiliation
     vertex.size=10*sqrt(dg), # size based on degree centrality
     edge.curved = TRUE, # make the edges curved
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y) # fix the upper/lower bounds of the plot
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states


# Basic plot with size for closeness centrality
plot(sg, 
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     vertex.color = col, # update vertex colors based on party affiliation
     vertex.size=1000*sqrt(cl), # size based on degree centrality
     edge.curved = TRUE, # make the edges curved
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y) # fix the upper/lower bounds of the plot
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states


# Basic plot with size for betweeness
plot(sg, 
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     vertex.color = col, # update vertex colors based on party affiliation
     vertex.size=10*sqrt(bn), # size based on degree centrality
     edge.curved = TRUE, # make the edges curved
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y) # fix the upper/lower bounds of the plot
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states






#----------------------------- PROBLEM 1E -----------------------------#

# calculate quotient of betweeness centrality divided by degree of centrality
sg.Senators$quotient <- sg.Senators$betweeness / sg.Senators$degree

# Senators with top 10 betweeness divided by closeness
head(sg.Senators[order(sg.Senators$quotient, decreasing = TRUE),], 10)



#----------------------------- PROBLEM 1F -----------------------------#

# create communities
set.seed(173)
community = cluster_spinglass(sg, spins = 100, weights = E(sg)$n)
head(community)

# get community information
clust = community$membership #extract community assignments
table(clust)
  
# calculate community modularity
community$modularity

#----------------------------- PROBLEM 1G -----------------------------#

# add communities to dataframe
sg.Senators$clust = community$membership
# add communities to edges
sg = set_edge_attr(sg, "c1", index=E(sg), sg.Senators[paste(senatorLinks$V1),"clust"])
sg = set_edge_attr(sg, "c2", index=E(sg), sg.Senators[paste(senatorLinks$V2),"clust"])
# colors for plots:
clrs = c("darkgreen","orange","lightblue", "purple","magenta")


# Plot

plot(sg, 
     vertex.size = 50, # you can modify this so that the points are visible
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     vertex.shape=recode(V(sg)$party,"R"="square","D"="circle", "I"="rectangle"), # assign vertex shapes by party
     vertex.color=clrs[sg.Senators$clust], # assign vertex color by community
     edge.lty = ifelse(E(sg)$c1 == E(sg)$c2, "solid", "blank"), # only show edges within the same community
     edge.color = clrs[E(sg)$c1], # edge color by community
     edge.curved = TRUE, # make the edges curved
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y) # fix the upper/lower bounds of the plot
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states

#----------------------------- PROBLEM 1H -----------------------------#

table(sg.Senators$party, clust)

#----------------------------- PROBLEM 1I -----------------------------#

table(sg.Senators$state, clust)



