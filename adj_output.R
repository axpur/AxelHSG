#### Setting workng directory ####
setwd("C:\\Users\\gedmi\\Desktop\\SE Project\\Further\\New\\AxelHSG")

#### Libraries ####
library(tidyverse)

#### Adjusting Python data ####
data_out <- read.csv("data/out_us.csv", stringsAsFactors = F)

## Remove brackets from Python list data
data_out$elections <- gsub(pattern = "\\[", replacement = "", x = data_out$elections)
data_out$elections <- gsub(pattern = "\\]", replacement = "", x = data_out$elections)

data_out$location_0 <- gsub(pattern = "\\[", replacement = "", x = data_out$location_0)
data_out$location_0 <- gsub(pattern = "\\]", replacement = "", x = data_out$location_0)

data_out$location_1 <- gsub(pattern = "\\[", replacement = "", x = data_out$location_1)
data_out$location_1 <- gsub(pattern = "\\]", replacement = "", x = data_out$location_1)

data_out$location_2 <- gsub(pattern = "\\[", replacement = "", x = data_out$location_2)
data_out$location_2 <- gsub(pattern = "\\]", replacement = "", x = data_out$location_2)

data_out$location_3 <- gsub(pattern = "\\[", replacement = "", x = data_out$location_3)
data_out$location_3 <- gsub(pattern = "\\]", replacement = "", x = data_out$location_3)

data_out$location_total <- gsub(pattern = "\\[", replacement = "", x = data_out$location_total)
data_out$location_total <- gsub(pattern = "\\]", replacement = "", x = data_out$location_total)

## Renaming ID column
colnames(data_out)[1] <- "step"

## Alternative cleaning
# Defining list of parameters of the run
params <- list()
params$steps <- length(unique(data_out$step))
params$types <- 4
params$locations <- 9
params$runs <- 10

# Creating an empty dataframe for storing values
data_store <- as.data.frame(matrix(NA, nrow = params$runs * params$types * params$locations * params$steps, ncol = 9))
colnames(data_store) <- c("run", "steps", "happy", "loc", "elect", "type", "group_total", "loc_grp_total", "loc_total")

# Defining case identifiers
data_store$run <- rep(data_out$run, each = params$types * params$locations)
data_store$happy <- rep(data_out$happy, each = params$types * params$locations)
data_store$loc <- rep(1:params$locations, times = params$runs * params$types * params$runs)
data_store$type <- rep(1:params$types, times = params$runs*params$steps, each = params$locations)
data_store$steps <- rep(data_out$step, each = params$types * params$locations)

# Matching identifiers with data
for (x in 1:length(unique(data_out$run))){
  data_out_loop <- filter(data_out, run == x)
    for (z in 1:length(unique(data_out_loop$step))) {
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x, "elect"] <- rep(as.numeric(strsplit(data_out_loop[z, "elections"], ",")[[1]]), 
                                                                       times = params$types)
      
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x & data_store$type == 1, "loc_grp_total"] <- as.numeric(strsplit(data_out_loop[z, "location_0"], ",")[[1]])
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x & data_store$type == 2, "loc_grp_total"] <- as.numeric(strsplit(data_out_loop[z, "location_1"], ",")[[1]])
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x & data_store$type == 3, "loc_grp_total"] <- as.numeric(strsplit(data_out_loop[z, "location_2"], ",")[[1]])
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x & data_store$type == 4, "loc_grp_total"] <- as.numeric(strsplit(data_out_loop[z, "location_3"], ",")[[1]])
      
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x & data_store$type == 1, "group_total"] <- data_out_loop[z, "total_0"]
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x & data_store$type == 2, "group_total"] <- data_out_loop[z, "total_1"]
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x & data_store$type == 3, "group_total"] <- data_out_loop[z, "total_2"]
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x & data_store$type == 4, "group_total"] <- data_out_loop[z, "total_3"]
      data_store[data_store$steps == data_out[z, "step"] & data_store$run == x, "loc_total"] <- rep(as.numeric(strsplit(data_out_loop[z, "location_total"], ",")[[1]]), 
                                                                           times = params$types)
    }
}
# Calculating intermediate values for calculating measures
data_calc <- data_store %>%
  group_by(run, steps, loc) %>%
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
  group_by(run, steps) %>%
  summarize(one = 0.5*sum((loc_total/(total*i))*(abs(pi_jm - pi_m))),
            two = sum((loc_total/(total*e))*pi_jm*log(pi_jm_log/pi_m)),
            four = sum((loc_total/total)* ((pi_jm - pi_m)^2)/((params$types - 1)*pi_m)),
            five = sum((loc_total/total)*(pi_jm-pi_m)^2))

data_ratios2 <- data_calc %>%
  group_by(run, type, steps) %>%
  summarize(three = sum(pi_j * pi_jm))

# Segregation measures over runs in the simulation
ratio_plotter <- ggplot(gather(data_ratios, key = "Ratio", value = "Value", one:five), aes(x = steps, y = Value, color = Ratio)) + 
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom") + ggtitle("Ratios over simulation") + facet_wrap(~run)

ratio_plotter

