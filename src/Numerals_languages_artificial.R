library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rje)

Folder = "../data/"
####################################
#Artificial language data
####################################

artificial_language = c()
for(w in 1:99){
  word = paste0("word", w)
  extension = w
  artificial_language <- c(artificial_language, list(c(word, extension))) 
  }
  

# Create a df from a list of vectors
artificial_df = as.data.frame(do.call(rbind, artificial_language))
colnames(artificial_df) <- c("word", "extension")
artificial_df$language = "artificial_language"

#transforming to strings because lists cannot be written in a csv
artificial_df <- apply(artificial_df,2,as.character)

# Generate fake languages and their items
artifical_filename = paste0(Folder, "artificial_languages.csv")
write.csv(artificial_df, artifical_filename, row.names=FALSE)

