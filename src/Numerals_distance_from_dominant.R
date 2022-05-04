source("./Numerals_functions.R")
options(scipen = 999)

####################################
# USE COMPLEXITY
####################################

languages_complexity = read.csv('../data/languages_complexities_all_LOTs_use_complexity.csv', header=TRUE)

####################################
# Pareto computations
####################################
#Import 3 paretos and compute and store distances for each
complexities = complexities_list() #complexity stores all LoT combinations in the format LOTprimitives_5 weights (primitive, +, -, successor, higher)

for(complexity in complexities){
  assign(paste0('natural_distances', complexity), paste0("../data/natural_distances_pareto", complexity, ".csv") )
  assign(paste0('natural', complexity), complexity.dist(subset(languages_complexity, type == "natural"), subset(languages_complexity, type == "artificial"), paste0("languages_complexity", complexity)))
  write.csv(eval(parse(text = paste0('natural', complexity))), eval(parse(text = paste0('natural_distances', complexity))), row.names=FALSE)
}

LoTs = c()
average_distances = c()
for(complexity in complexities){
  LoTs = c(LoTs, complexity)
  current_df = eval(parse(text = paste0('natural', complexity)))
  average_distances = c(average_distances, mean(current_df$distance))
}

LoTs_minimals_tokens_df = cbind(as.data.frame(LoTs), as.data.frame(average_distances))

#rankings
LoTs_minimals_tokens_df = LoTs_minimals_tokens_df[order(average_distances),]
write.csv(LoTs_minimals_tokens_df,'../data/LoTs_distances_from_dominant.csv' , row.names=FALSE)


# Histogram of distances
p = ggplot(LoTs_minimals_tokens_df, aes(x=average_distances)) +
  geom_histogram(binwidth=.01, colour="black", fill="white")+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13)) + labs(x= "Average relativized distance")
print(p)