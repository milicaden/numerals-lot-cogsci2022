source("./Numerals_functions.R")
options(scipen = 999)


natural_languages = read.csv("../data/natural_languages.csv", header = TRUE)

####################################
# Check that the formula computes the extension correctly
####################################
errors = subset(natural_languages, extension >99)
natural_languages$correct_extension = as.numeric(gsub("word", "", natural_languages$word))
natural_languages$extension_check = natural_languages$correct_extension == natural_languages$extension
errors = subset(natural_languages, extension_check == FALSE)

####################################
# Check that there is exactly 99 words for each language
####################################
word_numbers = natural_languages %>%
  group_by(language) %>%
  summarise(count=n())

errors = subset(word_numbers, !(count == 99))

####################################
# Check that there is no duplicates within language
####################################
word_numbers_within_language = natural_languages %>%
  group_by(language, word) %>%
  summarise(count=n())

errors = subset(word_numbers_within_language, !(count == 1))
