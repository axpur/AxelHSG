#### Setting workng directory ####
setwd("C:\\Users\\gedmi\\Desktop\\HSG\\Python practice\\Schelling with mesa")

#### Libraries ####
#library(tidyverse)

#### Adjusting Python data ####
data_out <- read.csv("out.csv", stringsAsFactors = F)

## Remove brackets from Python list data
data_out$elections <- gsub(pattern = "\\[", replacement = "", x = data_out$elections)
data_out$elections <- gsub(pattern = "\\]", replacement = "", x = data_out$elections)

data_out$location_0 <- gsub(pattern = "\\[", replacement = "", x = data_out$location_0)
data_out$location_0 <- gsub(pattern = "\\]", replacement = "", x = data_out$location_0)

data_out$location_1 <- gsub(pattern = "\\[", replacement = "", x = data_out$location_1)
data_out$location_1 <- gsub(pattern = "\\]", replacement = "", x = data_out$location_1)

data_out$location_total <- gsub(pattern = "\\[", replacement = "", x = data_out$location_total)
data_out$location_total <- gsub(pattern = "\\]", replacement = "", x = data_out$location_total)

## Remove ID column
data_out <- data_out[,-1]

## Generate column names for list data
elections_cols <- c()
loc0_cols <- c()
loc1_cols <- c()
loc_total_cols <- c()

for (i in 1:9){
  elections_cols[i] <- paste0("elec", i)
  loc0_cols[i] <- paste0("loc0_", i)
  loc1_cols[i] <- paste0("loc1_", i)
  loc_total_cols[i] <- paste0("loc_total_", i)
}

## Create empty dataframe to store adjusted dat
data_values <- as.data.frame(matrix(NA, nrow = nrow(data_out), ncol = 3 + length(elections_cols) * 4))

# Rename the dataframe
colnames(data_values) <- c("happy", "total_0", "total_1", elections_cols, loc0_cols, loc1_cols, loc_total_cols)

## Assigning non-list data
data_values$happy <- data_out$happy
data_values$total_0 <- data_out$total_0
data_values$total_1 <- data_out$total_1

## Looping over each list type data to R workable method
for (j in 1:nrow(data_out)){
  ## 'strsplit' - since Python list data is read as a string, strsplit splits the string 
  # at each comma in this case. Then as.numeric transforms into a numeric vector
  data_values[j, elections_cols] <- as.numeric(strsplit(data_out[j, "elections"], ",")[[1]])
  data_values[j, loc0_cols] <- as.numeric(strsplit(data_out[j, "location_0"], ",")[[1]])
  data_values[j, loc1_cols] <- as.numeric(strsplit(data_out[j, "location_1"], ",")[[1]])
  data_values[j, loc_total_cols] <- as.numeric(strsplit(data_out[j, "location_total"], ",")[[1]])
}
  


#test <- as.numeric(strsplit(data_out[, "elections"], ",")[[1]])
