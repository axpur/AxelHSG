#### Setting workng directory ####
setwd("C:\\Users\\gedmi\\Desktop\\HSG\\Python practice\\Schelling with mesa")

#### Libraries ####
library(tidyverse)

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

## Renaming ID column
colnames(data_out)[1] <- "run"
# 
# ## Generate column names for list data
# elections_cols <- c()
# loc0_cols <- c()
# loc1_cols <- c()
# loc_total_cols <- c()
# 
# for (i in 1:9){
#   elections_cols[i] <- paste0("elec", i)
#   loc0_cols[i] <- paste0("loc0_", i)
#   loc1_cols[i] <- paste0("loc1_", i)
#   loc_total_cols[i] <- paste0("loc_total_", i)
# }
# 
# 
# ## Create empty dataframe to store adjusted dat
# data_values <- as.data.frame(matrix(NA, nrow = nrow(data_out), ncol = 4 + length(elections_cols) * 4))
# 
# # Rename the dataframe
# colnames(data_values) <- c("run", "happy", "total_0", "total_1", elections_cols, loc0_cols, loc1_cols, loc_total_cols)
# 
# ## Assigning non-list data
# data_values$happy <- data_out$happy
# data_values$run <- data_out$run
# data_values$total_0 <- data_out$total_0
# data_values$total_1 <- data_out$total_1
# 
# ## Looping over each list type data to R workable method
# for (j in 1:nrow(data_out)){
#   ## 'strsplit' - since Python list data is read as a string, strsplit splits the string 
#   # at each comma in this case. Then as.numeric transforms into a numeric vector
#   data_values[j, elections_cols] <- as.numeric(strsplit(data_out[j, "elections"], ",")[[1]])
#   data_values[j, loc0_cols] <- as.numeric(strsplit(data_out[j, "location_0"], ",")[[1]])
#   data_values[j, loc1_cols] <- as.numeric(strsplit(data_out[j, "location_1"], ",")[[1]])
#   data_values[j, loc_total_cols] <- as.numeric(strsplit(data_out[j, "location_total"], ",")[[1]])
# }

## Alternative cleaning
# Defining list of parameters of the run
params <- list()
params$runs <- length(data_out$run)
params$types <- 2
params$locations <- 9

# Creating an empty dataframe for storing values
data_store <- as.data.frame(matrix(NA, nrow = params$runs * params$types * params$locations, ncol = 8))
colnames(data_store) <- c("run", "happy", "loc", "elect", "type", "group_total", "loc_grp_total", "loc_total")

# Defining case identifiers
data_store$run <- rep(data_out$run, each = params$types * params$locations)
data_store$happy <- rep(data_out$happy, each = params$types * params$locations)
data_store$loc <- rep(1:params$locations, times = params$runs * params$types)
data_store$type <- rep(1:params$types, times = params$runs, each = params$locations)

# Matching identifiers with data
for (z in 1:nrow(data_out)) {
  data_store[data_store$run == data_out[z, "run"], "elect"] <- rep(as.numeric(strsplit(data_out[z, "elections"], ",")[[1]]), 
                                                                   times = params$types)
  data_store[data_store$run == data_out[z, "run"] & data_store$type == 1, "loc_grp_total"] <- as.numeric(strsplit(data_out[z, "location_0"], ",")[[1]])
  data_store[data_store$run == data_out[z, "run"] & data_store$type == 2, "loc_grp_total"] <- as.numeric(strsplit(data_out[z, "location_1"], ",")[[1]])
  data_store[data_store$run == data_out[z, "run"] & data_store$type == 1, "group_total"] <- data_out[z, "total_0"]
  data_store[data_store$run == data_out[z, "run"] & data_store$type == 2, "group_total"] <- data_out[z, "total_1"]
  data_store[data_store$run == data_out[z, "run"], "loc_total"] <- rep(as.numeric(strsplit(data_out[z, "location_total"], ",")[[1]]), 
                                                                       times = params$types)
}

# Calculating intermediate values for calculating measures
data_calc <- data_store %>%
  group_by(run, loc) %>%
  mutate(total = sum(group_total),
         pi_m = group_total/total,
         pi_jm = loc_grp_total/loc_total,
         pi_j = loc_grp_total/group_total,
         i = sum(pi_m*(1-pi_m)),
         e = sum(pi_m*log(1/pi_m)),
         pi_jm_log = ifelse(pi_jm == 0, 0.001, pi_jm)) %>%
  ungroup()

# Calculating seggregation measures. Names correspond to the numbering in Notebook files
data_ratios <- data_calc %>%
  group_by(run) %>%
  summarize(one = 0.5*sum((loc_total/(total*i))*(abs(pi_jm - pi_m))),
            two = sum((loc_total/(total*e))*pi_jm*log(pi_jm_log/pi_m)),
            four = sum((loc_total/total)* ((pi_jm - pi_m)^2)/((params$types - 1)*pi_m)),
            five = sum((loc_total/total)*(pi_jm-pi_m)^2))

data_ratios2 <- data_calc %>%
  group_by(run, type) %>%
  summarize(three = sum(pi_j * pi_jm))

# Segregation measures over runs in the simulation
ratio_plotter <- ggplot(gather(data_ratios, key = "Ratio", value = "Value", one:five), aes(x = run, y = Value, color = Ratio)) + 
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom") + ggtitle("Ratios over simulation")

ratio_plotter

