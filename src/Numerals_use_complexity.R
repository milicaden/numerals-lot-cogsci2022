source("./Numerals_functions.R")
options(scipen = 999)


####################################
# Import languages and LoT descriptions
####################################
Folder = "../data/"
artificial_languages = read.csv("../data/artificial_languages.csv", header = TRUE)
artificial_languages$type = "artificial"

minimum_desc= read.csv("../data/shortest_LOT_descriptions.csv", header = TRUE)
artificial_languages = merge(artificial_languages, minimum_desc, by = c("extension"))
artificial_languages$morphology = NA

natural_languages = read.csv("../data/natural_lang_LOT_encodings.csv", header = TRUE)
natural_languages$type = "natural"


all_languages = rbind(artificial_languages, natural_languages)


####################################
# Complexity
####################################
# Function 'complexity' defined in Numerals_functions

# Store different complexity measures in vectors

generate_complexities = TRUE
complexities_filename = paste0(Folder, "languages_complexities_all_LOTs_use_complexity.csv")


if(generate_complexities){
  complexities = complexities_list()
  # stores all LoT combinations in the format LOTprimitives_5 weights (primitive, +, -, successor, higher)

  for(i in complexities){
    nam <- paste0("languages_complexity", i)
    assign(nam, c())

  }
  type = c()
  languages = c()

  for(lang in unique(all_languages$language)){
    languages = c(languages, lang)
    type = c(type, subset(all_languages, language == lang)[[1,c("type")]])
    for(i in complexities){
      nam <- paste0("languages_complexity", i)
      assign(nam, append(eval(parse(text = nam)), use_weighted_complexity(lang, all_languages, paste0("complexity", i))))
      }
  } #computes languages' complexity for each LoT combination
   
# Store complexity measures in a df

  langs_complexity = cbind(as.data.frame(languages), as.data.frame(type))
  for(i in complexities){
    nam <- paste0("languages_complexity", i)
    current_df = as.data.frame(eval(parse(text = nam)))
    colnames(current_df) <- c(nam)
    langs_complexity = cbind(langs_complexity, current_df)
  }

  write.csv(langs_complexity, complexities_filename, row.names=FALSE)
} else {
  langs_complexity = read.csv(complexities_filename)
}
