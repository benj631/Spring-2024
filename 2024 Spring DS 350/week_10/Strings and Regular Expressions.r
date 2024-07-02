pacman::p_load(tidyverse, dplyr, readr,stringr)

# Exercise 1

# Read the contents of the files
rl.txt <- readr::read_lines("randomletters.txt")

combined_rl_text <- paste(rl_txt, collapse = "")

indexes <- c(1, seq(1700, str_length(combined_rl_text), by = 1700))

split_string <- str_split(combined_rl_text, "")

split_string <- unlist(split_string)

# Select characters at specified indexes
selected_chars <- split_string[indexes]

selected_chars <- paste(selected_chars, collapse = "")
# Print the result
print(selected_chars)

# --> the plural of anecdote is not data.z anfra"

# Exercise 2

rlwn.txt <- readr::read_lines("randomletters_wnumbers.txt")


numbers_txt <- paste(rlwn_txt, collapse = "")

# Extract all numbers
numbers <- unlist(str_extract_all(numbers_txt, "\\d+"))

# Print extracted numbers
print(numbers)

# Initialize a vector to store converted letters
numbers_converted <- character(length(numbers))

# Convert each number to the corresponding letter
for (i in seq_along(numbers)) {
  num <- as.numeric(numbers[i])
  if (num >= 1 && num <= 26) {
    numbers_converted[i] <- letters[num]
  } else {
    numbers_converted[i] <- NA  # Assign NA if the number is out of the 1-26 range
  }
}

# Print the converted letters
numbers_converted <- paste(numbers_converted, collapse = "")
print(numbers_converted)


# --> expertsoftenpossessmoredatathanjudgment




# Exercise 3

rlwn.txt <- readr::read_lines("randomletters_wnumbers.txt")

pattern <- "[aeiouAEIOU]+"

# Extract all sequences of vowels
vowel_sequences <- str_extract_all(rlwn.txt, pattern)[[1]]

# Print the vowel sequences
print(vowel_sequences)

# Find the longest sequence of vowels
longest_vowel_sequence <- vowel_sequences[which.max(nchar(vowel_sequences))]

# Print the longest sequence of vowels
print(paste("Longest sequence of vowels:", longest_vowel_sequence))

# --> euaauue
