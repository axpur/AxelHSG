#### Setting workng directory ####
setwd("C:\\Users\\gedmi\\Desktop\\SE Project\\Further\\New\\AxelHSG")

#### Functions ####
cleaning_data <- function(params, data_out){
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
  
  # Creating an empty dataframe for storing values
  data_store <- as.data.frame(matrix(NA, nrow = params$runs * params$types * params$locations * params$steps, ncol = 11))
  colnames(data_store) <- c("cnt","run", "steps", "happy", "seg_agents", "loc", "elect", "type", "group_total", "loc_grp_total", "loc_total")
  
  # Defining case identifiers
  data_store$run <- rep(data_out$run, each = params$types * params$locations)
  data_store$cnt <- data_out[1,"cnt"]
  data_store$happy <- rep(data_out$happy, each = params$types * params$locations)
  data_store$seg_agents <- rep(data_out$seg_agents, each = params$types * params$locations)
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
  return(data_store)
}

agg_ratios <- function(data_store){
  data_calc <- data_store %>%
    group_by(run, steps, loc) %>%
    mutate(total = sum(group_total),
           pi_m = group_total/total,
           pi_jm = loc_grp_total/loc_total,
           pi_j = loc_grp_total/group_total,
           i = sum(pi_m*(1-pi_m)),
           e = sum(pi_m*log(1/pi_m)),
           pi_jm_log = ifelse(pi_jm == 0, 0.001, pi_jm),
           other_types_total = total-group_total,
           other_types_loc = loc_total - loc_grp_total,
           pi_other = other_types_total/total,
           pi_other_j = other_types_loc/loc_total,
           pi_other_j_log = ifelse(pi_other_j == 0, 0.001, pi_other_j),
           e_grp = (pi_m*log(1/pi_m)) + (pi_other*log(1/pi_other)),
           share_happy = happy/total,
           share_seg = seg_agents/total) %>%
    ungroup()
  
  # Calculating seggregation measures. Names correspond to the numbering in Notebook files
  data_ratios <- data_calc %>%
    group_by(run, steps, cnt) %>%
    summarize(one = 0.5*sum((loc_total/(total*i))*(abs(pi_jm - pi_m))),
              two = sum((loc_total/(total*e))*pi_jm*log(pi_jm_log/pi_m)),
              four = sum((loc_total/total)* ((pi_jm - pi_m)^2)/((params$types - 1)*pi_m)),
              five = sum((loc_total/total)*(pi_jm-pi_m)^2),
              share_happy = mean(share_happy),
              share_seg = mean(share_seg))
  
  return(data_ratios)
}

grp_ratios <- function(data_store){
  # Calculating intermediate values for calculating measures
  data_calc <- data_store %>%
    group_by(run, steps, loc) %>%
    mutate(total = sum(group_total),
           pi_m = group_total/total,
           pi_jm = loc_grp_total/loc_total,
           pi_j = loc_grp_total/group_total,
           i = sum(pi_m*(1-pi_m)),
           e = sum(pi_m*log(1/pi_m)),
           pi_jm_log = ifelse(pi_jm == 0, 0.001, pi_jm),
           other_types_total = total-group_total,
           other_types_loc = loc_total - loc_grp_total,
           pi_other = other_types_total/total,
           pi_other_j = other_types_loc/loc_total,
           pi_other_j_log = ifelse(pi_other_j == 0, 0.001, pi_other_j),
           e_grp = (pi_m*log(1/pi_m)) + (pi_other*log(1/pi_other)),
           share_happy = happy/total,
           share_seg = seg_agents/total) %>%
    ungroup()
  
  data_ratios2 <- data_calc %>%
    group_by(run, type, steps, cnt) %>%
    summarize(three = sum(pi_j * pi_jm),
              another_one = sum((loc_total/(total*e_grp))*pi_jm*log(pi_jm_log/pi_m) + (loc_total/(total*e_grp))*pi_other_j*log(pi_other_j_log/pi_other)))
  
  return(data_ratios2)
}

#### Libraries ####
library(tidyverse)
library(xtable)

start_time <- Sys.time()
#### Adjusting Python data ####
data_us <- read.csv("data/out_us.csv", stringsAsFactors = F)
colnames(data_us)[1] <- "step"

data_uk <- read.csv("data/out_uk.csv", stringsAsFactors = F)
colnames(data_uk)[1] <- "step"

data_aus <- read.csv("data/out_aus.csv", stringsAsFactors = F)
colnames(data_aus)[1] <- "step"

# Defining parameters of the simulation
params <- list()
params$steps <- length(unique(data_us$step))
params$types <- 4
params$locations <- 9
params$runs <- 20

# Cleaning data for each country
us_store <- cleaning_data(params = params, data_out = data_us)
uk_store <- cleaning_data(params = params, data_out = data_uk)
aus_store <- cleaning_data(params = params, data_out = data_aus)
step1_time <- Sys.time()

# Calculating aggregate ratios
us_agg_ratios <- agg_ratios(us_store)
uk_agg_ratios <- agg_ratios(uk_store)
aus_agg_ratios <- agg_ratios(aus_store)
step2_time <- Sys.time()

# Calculating group ratios
us_grp_ratios <- grp_ratios(us_store)
uk_grp_ratios <- grp_ratios(uk_store)
aus_grp_ratios <- grp_ratios(aus_store)
step3_time <- Sys.time()

# Combining country cases
all_agg_ratios <- bind_rows(list(us_agg_ratios, uk_agg_ratios, aus_agg_ratios))
all_grp_ratios <- bind_rows(list(us_grp_ratios, uk_grp_ratios, aus_grp_ratios))

# Plotting aggregate measures over steps and contrasting country cases
all_agg_ratios_plot <- all_agg_ratios %>%
  group_by(cnt, steps) %>%
  summarize(two = mean(two),
            share_happy = mean(share_happy),
            share_seg = mean(share_seg)) %>%
  gather(key = "Ratio", value = "Value", two, share_happy, share_seg)

all_ratio_plotter_agg <- ggplot(all_agg_ratios_plot, aes(x = steps, y = Value, color = cnt)) + 
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom") + ggtitle("Ratios over simulation") + facet_wrap(~Ratio)

pdf("Plots/all_agg_ratios.pdf")
all_ratio_plotter_agg
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types
all_grp_ratios_plot <- all_grp_ratios %>%
  group_by(cnt, steps, type) %>%
  summarize(another_one = mean(another_one))

all_ratio_plotter_grp <- ggplot(all_grp_ratios_plot, aes(x = steps, y = another_one, color = cnt)) + 
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom") + ggtitle("Ratios over simulation") + facet_wrap(~type)

pdf("Plots/all_grp_ratios.pdf")
all_ratio_plotter_grp
dev.off()

# Different kind of plot (Replication on the original one in Mesa example):
plot_data <- all_agg_ratios %>%
  ungroup() %>%
  group_by(run, cnt) %>%
  filter(steps == 99)

point_plot <- ggplot(plot_data, aes(x = cnt, y = share_happy, color = cnt)) + geom_point(alpha = 0.6) + theme_bw() + 
  ggtitle("Replication of graph in original notes") + scale_y_continuous(limits = c(0, 1)) + xlab("Country")

pdf("Plots/test_plot.pdf", width = 7, height = 5)
point_plot
dev.off()

# Table example
table_example_agg <- all_agg_ratios %>%
  ungroup() %>%
  filter(steps == 24 | steps == 49| steps == 74 | steps == 99) %>%
  group_by(steps, cnt) %>%
  summarize(mean_happy = round(mean(share_happy), 4),
            mean_seg = round(mean(share_seg),4 )) %>%
  ungroup() %>%
  mutate(steps = sprintf("%1.f", steps +1),
         mean_happy = sprintf("%1.2f%%", 100*mean_happy),
         mean_seg = sprintf("%1.2f%%", 100*mean_seg))

colnames(table_example_agg) <- c("Steps", "Country", "Happy", "Segregated")

print(xtable(table_example_agg, type = "latex"), include.rownames = F, file = "Tables/test_table.tex")
