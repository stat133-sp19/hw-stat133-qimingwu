# Title: Data Preparation for Visualizing Players' Shots
# Description: This R script is used to prepare data read from CSV files for visualizing each player's shots performance
# Inputs: Five .csv files in data directory, one for each player, named after the players
# Outputs: shots-data.csv for combined data and various txt files for summary statistics for each player

library(dplyr)

# Read in the five data sets, using relative file paths; the decision of the data types for each column is up to you.
column_types <- c("character", "character", "integer", "integer", "integer", "integer", "character", "character", "character", "integer", "character", "integer", "integer")

curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE, header = TRUE, colClasses = column_types)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE, header = TRUE, colClasses = column_types)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE, header = TRUE, colClasses = column_types)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE, header = TRUE, colClasses = column_types)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE, header = TRUE, colClasses = column_types)

# Add a column name to each imported data frame, that contains the name of the corresponding player
curry <- mutate(curry, name = "Stephen Curry")
durant <- mutate(durant, name = "Kevin Durant")
green <- mutate(green, name = "Draymond Green")
iguodala <- mutate(iguodala, name = "Andre Iguodala")
thompson <- mutate(thompson, name = "Klay Thompson")

# Change the original values of shot_made_flag to more descriptive values: replace "n" with "shot_no", and "y" with "shot_yes".
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"

# Add a column minute that contains the minute number where a shot occurred.
curry$minute <- 12 * curry$period - curry$minutes_remaining
durant$minute <- 12 * durant$period - durant$minutes_remaining
green$minute <- 12 * green$period - green$minutes_remaining
iguodala$minute <- 12 * iguodala$period - iguodala$minutes_remaining
thompson$minute <- 12 * thompson$period - thompson$minutes_remaining

# Use sink() to send the summary() output of each imported data frame into individuals text files. 
# During each sinking operation, the produced summaries should be sent to the output folder using relative paths.
sink(file = "../output/stephen-curry-summary.txt")
summary(curry)
sink()

sink(file = "../output/kevin-durant-summary.txt")
summary(durant)
sink()

sink(file = "../output/draymond-green-summary.txt")
summary(green)
sink()

sink(file = "../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

sink(file = "../output/klay-thompson-summary.txt")
summary(thompson)
sink()

# Use the row binding function rbind() to stack the tables into one single data frame (or tibble object).
grand_table <- rbind(curry, durant, green, iguodala, thompson)

# Export (i.e. write) the assembled table as a CSV file shots-data.csv inside the folder data.
write.csv(x = grand_table, file = "../data/shots-data.csv")

# Use sink() to send the summary() output of the assembled table. Send this output to a text file named shots-data-summary.txt inside the output folder.
sink(file = "../output/shots-data-summary.txt")
summary(grand_table)
sink()
