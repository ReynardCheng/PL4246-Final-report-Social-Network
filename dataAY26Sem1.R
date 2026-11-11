## initiate all libraries
library(igraph)
library(igraphdata)
library(tidyverse)
library(bootnet)
library(qgraph)
library(dplyr)

responses = read.csv('AY26Sem1/class-responses_AY26Sem1.csv', header = T)
node_info = read.csv('AY26Sem1/class-names_AY26Sem1.csv', header = T)

responses_dw = graph_from_data_frame(responses, directed = T, vertices = node_info)
summary(responses_dw)

## differentiate the nodes by gender
gender_colors = ifelse(
  V(responses_dw)$gender == "Female",
  "pink",
  ifelse(
    V(responses_dw)$gender == "Male", "lightblue",
    "orange"
  )
)
par(mar=c(0,0,0,0)+.6, mfrow = c(1,1))
plot(responses_dw,vertex.color = gender_colors)

## assigning ids node attribute to the network
## comparing nodes to of nodeinfo to network
nrow(node_info) == vcount(responses_dw) # comparing number of vertices
identical(node_info$id, V(responses_dw)$name) #overall comparison
setdiff(responses$rater, node_info$id) # check if there are different or missing ids
anyDuplicated(node_info$id) # check for duplicates

#to format the plots
par(
  mfrow = c(2, 2),
  mar = c(1, 1, 3, 1),
  oma = c(0, 0, 5, 0)
)
## friends are people with relationsip
friends = subgraph_from_edges(responses_dw, 
                                    eids = E(responses_dw)[E(responses_dw)$weight >=3]
                                    , delete.vertices = T)
## checking if the relatioship is mutual
mutual_friends = subgraph_from_edges(
  friends,
  eids = E(friends)[which_mutual(friends)],
  delete.vertices = TRUE
)
gender_friends = ifelse(
  V(friends)$gender == "Female", "pink",
  ifelse(
    V(friends)$gender == "Male", "lightblue",
    "orange"
  )
)
gender_mutual = ifelse(
  V(mutual_friends)$gender == "Female","pink",
  ifelse(
    V(mutual_friends)$gender == "Male", "lightblue",
    "orange"
  )
)

######## plotting graphs ###########
plot(friends,vertex.color = gender_friends,edge.arrow.size = 0.7, main = "Figure2. friends")
plot(mutual_friends,vertex.color = gender_mutual,edge.arrow.size = 0.7, main = "Figure3. Mutually Friends")

close_friends = subgraph_from_edges(responses_dw, 
                              eids = E(responses_dw)[E(responses_dw)$weight >3]
                              , delete.vertices = T)

mutual_close = subgraph_from_edges(
  close_friends,
  eids = E(close_friends)[which_mutual(close_friends)],
  delete.vertices = TRUE
)

gender_close = ifelse(
  V(close_friends)$gender == "Female", "pink",
  ifelse(
    V(close_friends)$gender == "Male", "lightblue",
    "orange"
  )
)
gender_mutualclose = ifelse(
  V(mutual_close)$gender == "Female", "pink",
  ifelse(
    V(mutual_close)$gender == "Male", "lightblue",
    "orange"
  )
)

######## plotting graphs ###########
plot(close_friends, vertex.color = gender_close,edge.arrow.size = 0.7, main = "Figure4. close friends")
plot(mutual_close,vertex.color = gender_mutualclose,edge.arrow.size = 0.7, main = "Figure5. Mutually close")

# Overall title
title(
  main = "AY26 Semester 1 Social Network",
  outer = TRUE,
  line = 1,
  cex.main = 1.5
)

######################### Macro Analysis #########
## average connection weight
E(responses_dw)$weight |> summary()

## summarises the network
summary(responses_dw)

##Degree
degree(responses_dw) |> mean()
degree(friends) |> mean()
degree(mutual_friends) |> mean()

degree(close_friends) |> mean()
degree(mutual_close) |> mean()

mean(degree(responses_dw, mode = "in"))

mean(degree(responses_dw, mode = "out"))

# Network density
edge_density(responses_dw)

# Global clustering/transitivity
transitivity(responses_dw, type = "global")
transitivity(responses_dw, type = "local") |> mean()

transitivity(responses_dw, type = "local") |>
  mean(na.rm = TRUE)

# Average path length
mean_distance(responses_dw)

# Diameter
diameter(responses_dw)

## Strength 
strength(responses_dw)
strength(responses_dw) |> mean()


##assortative mixing
## Assortative mixing

# Degree assortativity
assortativity_degree(
  responses_dw,
  directed = TRUE
)

# Gender assortativity
gender = V(responses_dw)$gender

# Treat missing gender as "No Response"
gender[is.na(gender)] = "No Response"

# Convert categories to integer IDs
gender_type <- as.integer(factor(gender))

assortativity_nominal(
  responses_dw,
  types = gender_type,
  directed = TRUE
)

## comparing degrees and strength between gender
gender_comparison = data.frame(
  gender = V(responses_dw)$gender,
  out_degree = degree(responses_dw, mode = "out"),
  in_degree = degree(responses_dw, mode = "in"),
  out_strength = strength(responses_dw, mode = "out")
)

## mean in-out degrees and strength for each gender
gender_comparison |>
  filter(gender %in% c("Male", "Female")) |>
  group_by(gender) |>
  summarise(
    n = n(),
    mean_out_degree = mean(out_degree),
    mean_in_degree = mean(in_degree),
    mean_out_strength = mean(out_strength),
    .groups = "drop"
  )

gender_comparison$mean_out_weight = with(
  gender_comparison,
  ifelse(
    out_degree > 0,
    out_strength / out_degree,
    NA_real_
  )
)
##measuring the mean outgoing edge weight by gender
gender_comparison |>
  filter(gender %in% c("Male", "Female")) |>
  group_by(gender) |>
  summarise(
    mean_relationship_weight = mean(
      mean_out_weight,
      na.rm = TRUE
    ),
    .groups = "drop" #removes the groupings after summarise() function
  )

#measures reciprocity accross all students
reciprocity_table <- data.frame(
  Network = c(
    "All relationships",
    "Friendships",
    "Close friendships"
  ),
  Reciprocity = c(
    reciprocity(responses_dw),
    reciprocity(friends),
    reciprocity(close_friends)
  )
)

print(reciprocity_table)


# Function to calculate reciprocity by gender
gender_reciprocity = function(graph, network_name) {
  
  # Identify reciprocated edges
  mutual = which_mutual(graph)
  
  # Extract edge information
  edge_data = igraph::as_data_frame(graph, what = "edges") #specify function from igraph due to loading mutliple libraries
  
  # Add reciprocity status
  edge_data$reciprocated = mutual
  
  # Identify gender of the student making the nomination
  edge_data$gender = V(graph)$gender[
    match(edge_data$from, V(graph)$name)
  ]
  
  # Calculate reciprocity by gender
  edge_data |>
    filter(gender %in% c("Male", "Female")) |>
    group_by(gender) |>
    summarise(
      total_edges = n(),
      reciprocal_edges = sum(reciprocated),
      reciprocity = mean(reciprocated),
      .groups = "drop"
    ) |>
    mutate(Network = network_name)
}

# Combine results from three networks
reciprocity_gender_table = bind_rows(
  gender_reciprocity(responses_dw, "All relationships"),
  gender_reciprocity(friends, "Friendships"),
  gender_reciprocity(close_friends, "Close friendships")
)

# Print table
print(reciprocity_gender_table)

## Small world index
responses_uu = graph_from_data_frame(responses, directed = F, vertices = node_info)
summary(responses_uu)
responses_uu <- delete_edge_attr(
  responses_uu,
  "weight"
)
summary(responses_uu)

##making er networks
set.seed(1)

er_graphs <- list()

for(i in 1:1000) { # number of networks to generate
  er_graphs[[i]] <- sample_gnm(n = gorder(responses_uu), m = gsize(responses_uu), directed = F, loops = F)
}

# compute the L and C for each ER network, take the mean 
sapply(er_graphs, mean_distance) |> mean() # L_rand
sapply(er_graphs, transitivity, type = 'global') |> mean() # C_rand

##calculating small world index
CiC = transitivity(responses_uu, type = "global")/sapply(er_graphs, transitivity, type = 'global') |> mean()
LiL = mean_distance(responses_uu)/sapply(er_graphs, mean_distance) |> mean()
swi = CiC/LiL
swi

  
