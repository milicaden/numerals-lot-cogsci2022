library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rje)
library(rlist)
library(stringr)
library(purrr)
####################################
# Functions
####################################
# Useful version of as.numeric (as.numeric annoyingly can change values if applied to factors...)
as.numeric2 = function(x){
  return(as.numeric(as.character(x)))
}

# Filter function: to ease further computations, we remove expressions which result in the same meaning as a simpler expression

filter = function(set){
  unwanted = c()
  for(i in 1:length(set)){
    for(j in 1:length(set)){
      if(i==j){
        next}
      if(as.numeric2(unlist(set[i])[2]) == as.numeric2(unlist(set[j])[2])){
        if(as.numeric2(unlist(set[i])[3]) > as.numeric2(unlist(set[j])[3])){
          unwanted = c(unwanted, i)
        }
        if(as.numeric2(unlist(set[j])[3]) > as.numeric2(unlist(set[i])[3])){
          unwanted = c(unwanted, j)
        }
        if((as.numeric2(unlist(set[j])[3]) == as.numeric2(unlist(set[i])[3])) & i < j){# if they are of equal complexity, throw out the one with larger index
          unwanted = c(unwanted, j)
        }
      }
    }
  }
  if(length(unwanted) >0){
    set = set[-unwanted]}
  return(set)
}
# Generate function for recursive application of higher_than, successor and +/- functions up to certain depth
generate = function(depth, complexity_plus, complexity_minus, complexity_multiplicatiton, complexity_division){
    start = 2
  while (start < depth + 1){
    nam <- paste0("set", start)
    assign(nam, c())
    previous = eval(parse(text = paste0("set", start-1)))
    for(x in 1:start){ # +/- combinations 
      for(y in 1:start){
        if(x+y == start){
          for(first_element in eval(parse(text = paste0("set", x)))){
            for(second_element in  eval(parse(text = paste0("set", y)))){ #format: LoT, extension, complexity
              current_plus = c(paste0(first_element[1], "_plus_", second_element[1]), as.numeric2(first_element[2]) + as.numeric2(second_element[2]), as.numeric2(first_element[3]) + as.numeric2(second_element[3]) + complexity_plus)
              current_minus = c(paste0(first_element[1], "_minus_", second_element[1]), as.numeric2(first_element[2]) - as.numeric2(second_element[2]), as.numeric2(first_element[3]) + as.numeric2(second_element[3]) + complexity_minus)
              current_multiplication = c(paste0(first_element[1], "_multiplication_", second_element[1]), as.numeric2(first_element[2]) * as.numeric2(second_element[2]), as.numeric2(first_element[3]) + as.numeric2(second_element[3]) + complexity_multiplicatiton)
              current_division = c(paste0(first_element[1], "_division_", second_element[1]), as.numeric2(first_element[2]) / as.numeric2(second_element[2]), as.numeric2(first_element[3]) + as.numeric2(second_element[3]) + complexity_division)
              
              if(as.numeric2(current_plus[2])<100){
                assign(nam, append(eval(parse(text = nam)), list(current_plus)))
              }
              if(as.numeric2(current_minus[2])> 0 & as.numeric2(current_minus[2])<100){
                assign(nam, append(eval(parse(text = nam)), list(current_minus)))
              }
              if(as.numeric2(current_multiplication[2])<100){
                assign(nam, append(eval(parse(text = nam)), list(current_multiplication)))
              }
              if(is.integer(current_division[2])){
                assign(nam, append(eval(parse(text = nam)), list(current_division)))
              }
            }
             }
           }
      }
    }
    assign(nam, filter(eval(parse(text = nam)))) #filter

    all = c()
    for(i in 1:start){
      all = c(all, eval(parse(text = paste0("set", i))))
    }
    
    all_filtered = filter(all)
    
    if(length(all_filtered) == 99){
      return(all_filtered)
    }
    else{start = start+1}
  }
}



####################################
#LoT
####################################
primitive_classes = list(list(1,2,3), list(1,2), list(1), list(1,2,3,4), list(1,2,3,4,5), list(1,2,3,4,5,6), list(1,2,3,4,5,6,7), list(1,2,3,4,5,6,7,8), list(1,2,3,4,5,6,7,8,9), list(1,2,3,4,5,6,7,8,9,10), 
                         list(1,2,3,10), list(1,2,10), list(1,10), list(1,2,3,4,10), list(1,2,3,4,5,10), list(1,2,3,4,5,6,10), list(1,2,3,4,5,6,7,10), list(1,2,3,4,5,6,7,8,10),
                         list(1,2,3,20), list(1,2,20), list(1,20), list(1,2,3,4,20), list(1,2,3,4,5,20), list(1,2,3,4,5,6,20), list(1,2,3,4,5,6,7,20), list(1,2,3,4,5,6,7,8,20), list(1,2,3,4,5,6,7,8,9,20),
                         list(1,2,3,5), list(1,2,5), list(1,5),
                         list(1,2,3,5,10), list(1,2,5,10), list(1,5,10),
                         list(1,2,3,5,20), list(1,2,5,20), list(1,5,20),
                         list(1,2,3,5,10,20), list(1,2,5,10,20), list(1,5,10,20),
                         list(1,2,3,10,20), list(1,2,10,20), list(1,10,20), list(1,2,3,4,10,20), list(1,2,3,4,5,10,20), list(1,2,3,4,5,6,10,20), list(1,2,3,4,5,6,7,10,20), list(1,2,3,4,5,6,7,8,10,20),list(1,2,3,4,5,6,7,8,9,10,20)
                         )


####################################
# Generation of LOT expressions for the meanings (Version 1: without passing through previously defined items)
####################################
complexities = c(1)

for(option1 in complexities){
  complexity_primitive = option1
  
  for(option2 in complexities){
    complexity_plus = option2
    
    for(option3 in complexities){
      complexity_minus = option3
      
       for(option4 in complexities){
          complexity_multiplication = option4
          
          for(option5 in complexities){
            complexity_division = option5
          
for (primitives in primitive_classes){
  LoT_name = reduce(primitives, paste0) #first part of the name: which numerals are primitives
  set1 = c()

  for(i in primitives){
    set1 = c(set1, list(c(as.character(i), as.character(i), complexity_primitive))) # list(LoT expression, extension, complexity), for primitives complexity = 1
  }

  rm(lot_expressions)
  rm(lot_expressions_df)
  
  lot_expressions = generate(15, complexity_plus, complexity_minus, complexity_multiplication, complexity_division)

  lot_expressions_df = as.data.frame(do.call(rbind, lot_expressions))
  colnames(lot_expressions_df) <- c("LoT", "extension", paste0("complexity", LoT_name, "_", option1, option2, option3, option4, option5))
  assign(paste0("lot_expressions_df",  LoT_name, "_", option1, option2, option3, option4, option5), lot_expressions_df)

# Find the shortest LoT expression for each extension
# Create the data frame that stores for each extension its (minimum) complexity

  minimum.df = lot_expressions_df[FALSE,]
  for(i in unique(lot_expressions_df$extension)){
    temp = subset(lot_expressions_df, extension == i)
    temp[[paste0("complexity", LoT_name, "_", option1, option2, option3, option4, option5)]] = as.numeric2(temp[[paste0("complexity", LoT_name, "_", option1, option2, option3, option4, option5)]])
    minimum = min(temp[[paste0("complexity", LoT_name, "_", option1, option2, option3, option4, option5)]])
    temp.min = subset(temp, eval(parse(text = paste0("complexity", LoT_name, "_", option1, option2, option3, option4, option5))) == minimum)
    minimum.df = rbind(minimum.df, temp.min[1,])
  }

  assign(paste0("minimum.df",  LoT_name, "_", option1, option2, option3, option4, option5), minimum.df) #we will have one minimum.df for each LoT
  
  if(exists('minimum.df.final') && is.data.frame(get('minimum.df.final'))){
    minimum.df.final = merge(minimum.df.final, minimum.df[,c("extension", paste0("complexity", LoT_name, "_", option1, option2, option3, option4, option5))], by = c("extension"))
  }
  else{
    minimum.df.final = minimum.df[,c("extension", paste0("complexity", LoT_name, "_", option1, option2, option3, option4, option5))]
  }
  
  }
  
  
        }
    }
}
}
}

write.csv(minimum.df.final,'../data/shortest_LOT_descriptions.csv',row.names=FALSE)

