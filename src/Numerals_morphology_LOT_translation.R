library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rje)
library(rlist)
library(stringr)
library(purrr)
source("./Numerals_functions.R")

####################################
# Morphology to LoT
####################################
# Useful version of as.numeric (as.numeric annoyingly can change values if applied to factors...)
as.numeric2 = function(x){
  return(as.numeric(as.character(x)))
}

natural_lang = read.csv('../data/natural_languages.csv', header=TRUE)
dictionary = read.csv('../data/shortest_LOT_descriptions.csv', header=TRUE)

LoT_list = complexities_list()
for(i in 1:length(LoT_list)){
  LoT_list[i] = paste0('complexity', LoT_list[i])
}
for(x in LoT_list){
  natural_lang[[x]] = 0
}

for(complexity in LoT_list){ 
  for(i in 1:length(natural_lang$morphology)){
  j1 = lengths(regmatches(natural_lang$morphology[i], gregexpr('*', natural_lang$morphology[i], fixed=TRUE)))
  natural_lang[[complexity]][i] = natural_lang[[complexity]][i] + j1
  j2 = lengths(regmatches(natural_lang$morphology[i], gregexpr('+', natural_lang$morphology[i], fixed=TRUE)))
  natural_lang[[complexity]][i] = natural_lang[[complexity]][i] + j2
  j3 = lengths(regmatches(natural_lang$morphology[i], gregexpr('-', natural_lang$morphology[i], fixed=TRUE)))
  natural_lang[[complexity]][i] = natural_lang[[complexity]][i] + j3
  j4 = lengths(regmatches(natural_lang$morphology[i], gregexpr('/', natural_lang$morphology[i], fixed=TRUE)))
  natural_lang[[complexity]][i] = natural_lang[[complexity]][i] + j4
  for(n in 1:99){#nb: condition around n makes sure e.g. 1s are not counted in 11
    k = lengths(regmatches(natural_lang$morphology[i], gregexpr(paste0('(?<![0-9])', n, '(?![0-9])'), natural_lang$morphology[i], perl = TRUE)))
    if(k > 0){
      comp_current_df = subset(dictionary, extension == n) 
      comp_current = comp_current_df[1, complexity]
      natural_lang[[complexity]][i] = natural_lang[[complexity]][i] + k*comp_current
    }
    
  }
  }
}

####################################
# LoT descriptions of numerals for natural languages
####################################

write.csv(natural_lang,'../data/natural_lang_LOT_encodings.csv',row.names=FALSE)

