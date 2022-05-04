library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rje)
library(rlist)
library(stringr)
library(purrr)

#Produce a list of LoT names based on their primitives and weights (w_prim = 1 for all prim for this version of the study)
complexities_list <- function(){
  primitive_classes = list(list(1,2,3), list(1,2), list(1), list(1,2,3,4), list(1,2,3,4,5), list(1,2,3,4,5,6), list(1,2,3,4,5,6,7), list(1,2,3,4,5,6,7,8), list(1,2,3,4,5,6,7,8,9), list(1,2,3,4,5,6,7,8,9,10), 
                           list(1,2,3,10), list(1,2,10), list(1,10), list(1,2,3,4,10), list(1,2,3,4,5,10), list(1,2,3,4,5,6,10), list(1,2,3,4,5,6,7,10), list(1,2,3,4,5,6,7,8,10),
                           list(1,2,3,20), list(1,2,20), list(1,20), list(1,2,3,4,20), list(1,2,3,4,5,20), list(1,2,3,4,5,6,20), list(1,2,3,4,5,6,7,20), list(1,2,3,4,5,6,7,8,20), list(1,2,3,4,5,6,7,8,9,20),
                           list(1,2,3,5), list(1,2,5), list(1,5),
                           list(1,2,3,5,10), list(1,2,5,10), list(1,5,10),
                           list(1,2,3,5,20), list(1,2,5,20), list(1,5,20),
                           list(1,2,3,5,10,20), list(1,2,5,10,20), list(1,5,10,20),
                           list(1,2,3,10,20), list(1,2,10,20), list(1,10,20), list(1,2,3,4,10,20), list(1,2,3,4,5,10,20), list(1,2,3,4,5,6,10,20), list(1,2,3,4,5,6,7,10,20), list(1,2,3,4,5,6,7,8,10,20),list(1,2,3,4,5,6,7,8,9,10,20)
  )
  
  LoTs = c()
  for (primitives in primitive_classes){
    LoT_name = reduce(primitives, paste0) #first part of the name: which numerals are primitives
    LoTs = c(LoTs, LoT_name)}
  
  weight_options = list(1)
  product = cross(list(weight_options, weight_options, weight_options, weight_options, weight_options))

  weights_product = c()
  for(i in product){
    weights_product = c(weights_product, reduce(i, paste0))}

  complexities = c()
  for(LoT in LoTs){
    for(weights in weights_product){
      complexities = c(complexities, paste0(LoT, '_', weights))
  }
  }
  return(complexities)
}

### Compute complexity of a language in a df for an LoT (complexity argument)

use_weighted_complexity <- function(language_id, df, complexity){
  temp <- subset(df, language == language_id)
  u <- 0
  for (i in temp$word){
    currentwordcomplexity <- subset(temp, word == i)[[complexity]]
    currentwordprobability <- probaf(subset(temp, word == i)[['extension']])
    u <- u + currentwordcomplexity*currentwordprobability
  }
  return(u)
}


###Informativeness-related functions
# Compute the prior for a given number: power-law basic
prior_sum = 0 #for normalization of prior probas
for(i in 1:100){
  prior_sum = prior_sum + i**(-2)
}

probaf <- function(state){
  number = as.numeric(state)
  return(number**(-2)/prior_sum)
}


### Pareto-frontier related functions

# Compute complexity distance
complexity.dist = function(df, pareto, complexity) {#df is a data frame with languages of interest; pareto is a df with estimated points of the pareto frontier
  for(i in 1:nrow(df)){
    df$distance[i] = (df[[complexity]][i] - pareto[[complexity]][1])/pareto[[complexity]][1] #normalized by the complexity of complexity of the optimal system
     }
  return(df)
}

