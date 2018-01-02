#### Analysis with R: "Do Voting Systems Affect Segregation" ####
### By: Julia Baumann, Tadas Gedminas and Axel Purwin
# This file contains R code used for analysing simulation output.
# For this reason either use data that is provided in the repository
# or re-generate the data using 'data_collect.py' file. Note that the
# R file is structured that for all cases the analysis for each 
# situation follows the same general flow: 1) Loading data,
# 2) Cleaning data, 3) Combining all country cases together,
# 4) Plotting and storing output for reporting. Given this structure
# replication is possible by scenario instead of replicating all plots
# at once. In order to replicate the results first run 'Setting working directory',
# 'Functions', 'Libraries' and 'Pre-amble' sections. Afterwards each case specific
# section can be run separetly. Note that 'Other analysis' section requires first 
# to run the 'Baseline model' section. Alo, note that the code relies on 3 author 
# defined functions, which contain descriptions within them.
#
# NOTE: Remember to re-define the working directory, of the R file. The 
# directory should contain the R file and a 'Data' folder with all .csv
# files.

#### Setting workng directory ####
setwd("C:\\Users\\gedmi\\Desktop\\SE Project\\Further\\New\\AxelHSG")

#### Functions ####
complete_calc <- function(data_out){
  # The purpose of the function is to 1) clean the data, 2) restructure the data and 3) compute
  # relevant measures. The structure of the cleaning and which ratios are calculated is not 
  # specific to the data, hence can be generalized to all simulated output that is produced using
  # the Python code. 
  
  # Note on the structure of the data. While the python data output is such that a single observation
  # (row) corresponds to a single step in the simulation, for analysis each observation (row) should 
  # in adition to corresponding to a step should also be differentiated by agent type that we are 
  # looking at and location specific measures. 
  
  # The final output of the function is a list of 3 tables which calculate relevant measures.
  # For this reason function output that is referred to [[1]] returns aggregated group segregation measures
  # [[2]] returns group specific segregation measures and [[3]] returns election specific outcomes
  
  # Renaming missing column name and adjusting the value
  colnames(data_out)[1] <- "steps"
  data_out$steps <- data_out$steps+1
  
  # Regex to remove square brackets from python values that were location specific
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
  
  # Names of replacement columns. Indices from 1 to 9 correspond to 9 locations
  colnames_elect <- paste0("elect", 1:9)
  colnames_total <- paste0("loc_total", 1:9)
  colnames_grp0 <- paste0("loc_grp_total0", 1:9)
  colnames_grp1 <- paste0("loc_grp_total1", 1:9)
  colnames_grp2 <- paste0("loc_grp_total2", 1:9)
  colnames_grp3 <- paste0("loc_grp_total3", 1:9)
  
  # Creating empty columns for adding into data
  data_out[, c(colnames_elect, colnames_total, colnames_grp0, colnames_grp1, colnames_grp2, colnames_grp3)] <- NA
  
  # The following lines separate location specific values from a single column to a single distinct column
  data_out[, colnames_elect] <- matrix(as.numeric(unlist(strsplit(data_out[, "elections"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_total] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_total"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_grp0] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_0"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_grp1] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_1"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_grp2] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_2"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_grp3] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_3"], ","))), ncol = 9, byrow = T)
  
  # Maintaining values for A: rename column instead of creating additional varaibles
  data_out[, "group_total0"] <- data_out[,"total_0"]
  data_out[, "group_total1"] <- data_out[,"total_1"]
  data_out[, "group_total2"] <- data_out[,"total_2"]
  data_out[, "group_total3"] <- data_out[,"total_3"]
  
  # Removing irrelevant columns
  data_out <- data_out[, -which(names(data_out) %in% c("elections", "location_total", "location_0", "location_1", "location_2", "location_3",
                                                       "total_0", "total_1", "total_2", "total_3"))]
  
  # Restructuring of the data
  data_store <- data_out %>% 
    gather(key = "agg_types", value = "num", c(colnames_elect, colnames_total, colnames_grp0, colnames_grp1, colnames_grp2, colnames_grp3)) %>%
    mutate(loc = as.factor(substr(agg_types, start = nchar(agg_types), stop = nchar(agg_types))),
           agg_types = substr(agg_types, start = 1, stop = nchar(agg_types)-1)) %>%
    spread(key = "agg_types", value = "num") %>%
    gather(key = "types", value = "num", c("loc_grp_total0", "loc_grp_total1", "loc_grp_total2", "loc_grp_total3", 
                                           "group_total0", "group_total1", "group_total2", "group_total3")) %>%
    mutate(type = parse_number(types),
           types = gsub(pattern = "[0-9]", replacement = "", types)) %>% 
    spread(key = "types", value = "num") %>% 
    arrange(run, steps, type, loc)
  
  # Calculating intermediate values for aggregate segregation measures. ADD: refer to the page for the formula
  data_agg_calc <- data_store %>%
    group_by(run, steps, loc, cases) %>%
    mutate(total = sum(group_total),
           pi_m = group_total/total,
           pi_jm = loc_grp_total/loc_total,
           e = sum(pi_m*log(1/pi_m)),
           pi_jm_log = ifelse(pi_jm == 0, 0.001, pi_jm),
           share_happy = happy/total,
           share_seg = seg_agents/total) %>%
    ungroup()
  
  # Aggregating segregation measures for all groups ADD: refer to the page for the formula
  data_agg_ratios <- data_agg_calc %>%
    group_by(run, steps, cnt, cases) %>%
    summarize(info_seg = sum((loc_total/(total*e))*pi_jm*log(pi_jm_log/pi_m)),
              share_happy = mean(share_happy),
              share_seg = mean(share_seg))
  
  # Calculating intermediate values for group segregation measures ADD: refer to the page for the formula
  data_grp_calc <- data_store %>%
    group_by(run, steps, loc, cases) %>%
    mutate(total = sum(group_total),
           pi_m = group_total/total,
           pi_jm = loc_grp_total/loc_total,
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
  
  # Calculating group specific segregation measure ADD: refer to the page for the formula
  data_grp_ratios <- data_grp_calc %>%
    group_by(run, type, steps, cnt, cases) %>%
    summarize(grp_info = sum((loc_total/(total*e_grp))*pi_jm*log(pi_jm_log/pi_m) + 
                               (loc_total/(total*e_grp))*pi_other_j*log(pi_other_j_log/pi_other)))
  
  data_elec_calc <- data_store %>%
    select(run, steps, loc, elect, cnt)
  
  # Removing duplicate cases such that a single observation for each location exists for each step.
  data_elec_calc <- data_elec_calc[!duplicated(data_elec_calc),]
  
  data_elec_ratios <- data_elec_calc %>%
    arrange(run, loc, steps) %>%
    group_by(run, loc) %>%
    mutate(change_loc = ifelse(elect == lag(elect), 0, 1))
  
  # Returning a list of relevant measures that were calculated
  return(list(data_agg_ratios, data_grp_ratios, data_elec_ratios)) 
}

agg_results_plotter <- function(agg_plot_data, cases){
  # Function used for plotting aggregate measures. The function takes as input 
  # data table of aggregate measures and a logical value 'cases' which dinstiguishes
  # when the plot is single case or when the plot should compare different parameter
  # values.
  
  if (cases == T){
    # Manipulation of data and plotting function are combined into one step
    agg_plot <- agg_plot_data %>%
      group_by(cnt, steps, cases) %>% # Grouping variables. Note that since we are averaging out over all runs, 'runs' variable is omitted
      summarize(info_seg = mean(info_seg), # Averaging out over all runs for each step
                share_happy = mean(share_happy),
                share_seg = mean(share_seg)) %>%
      gather(key = "Ratio", value = "Value", info_seg, share_happy, share_seg) %>% # Restructuring the data from wide to long for plotting
      ggplot(aes(x = steps, y = Value, color = cnt)) + # Defining the x-y and variable used for coloring
      scale_y_continuous(limits = c(0, 1)) + # Defining the y axis scale
      geom_line(size = 1) + # Adjustin line size
      theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + # Changing background and legend specific elements
      xlab("Steps") + scale_color_brewer(palette = "Set1") # Setting x-axis and a color scheme for the plot
      
    # The plot object is returned as function output
      return(agg_plot)
  } else {
    # Manipulation of data and plotting function are combined into one step
    agg_plot <- agg_plot_data %>%
      group_by(cnt, steps) %>% # Grouping variables. Note that since we are averaging out over all runs, 'runs' variable is omitted
      summarize(info_seg = mean(info_seg), # Averaging out over all runs for each step
                share_happy = mean(share_happy),
                share_seg = mean(share_seg)) %>%
      gather(key = "Ratio", value = "Value", info_seg, share_happy, share_seg) %>% # Restructuring the data from wide to long for plotting
      ggplot(aes(x = steps, y = Value, color = cnt)) + # Defining the x-y and variable used for coloring
      scale_y_continuous(limits = c(0, 1)) + # Defining the y axis scale
      geom_line(size = 1) + # Adjusting line size
      theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + # Changing background and legend specific elements
      ggtitle("Mean Aggregate Ratios Over Steps") + # Adding plot title
      facet_grid(~Ratio, labeller = as_labeller(ratio_agg_names)) + xlab("Steps") + # Creating separate plots for each aggergate measure
      scale_color_brewer(palette = "Set1") # Defining the coloring scheme for distinguishing countries
    
    # The plot object is returned as function output
    return(agg_plot)
  }
  
}

grp_results_plotter <- function(grp_plot_data, cases){
  # Function used for plotting group measures. The function takes as input 
  # data table of aggregate measures and a logical value 'cases' which dinstiguishes
  # when the plot is single case or when the plot should compare different parameter
  # values.
  
  if (cases == T) {
    # Manipulation of data and plotting function are combined into one step
    grp_plot <- grp_plot_data %>%
      group_by(cnt, steps, type, cases) %>% # Grouping variables. Note that since we are averaging out over all runs, 'runs' variable is omitted
      summarize(grp_info = mean(grp_info)) %>% # Averaging out over all runs and across agent types for each step
      ggplot(aes(x = steps, y = grp_info, color = cnt)) + # Defining the x-y and variable used for coloring
      scale_y_continuous(limits = c(0, 1)) + # Defining y axis scale
      geom_line(size = 1) + # Adjusting size of lines
      theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + # Changing background and legend specific elements
      xlab("Steps") + ylab("Group Information Value") + # Adding x and y axis labels
      scale_color_brewer(palette = "Set1") # Defining the coloring scheme for distinguishing countries
    
    # The plot object is returned as function output
    return(grp_plot) 
    
  } else {
    # Manipulation of data and plotting function are combined into one step
    grp_plot <- grp_plot_data %>%
      group_by(cnt, steps, type) %>% # Grouping variables. Note that since we are averaging out over all runs, 'runs' variable is omitted
      summarize(grp_info = mean(grp_info)) %>% # Averaging out over all runs and across agent types for each step
      ggplot(aes(x = steps, y = grp_info, color = cnt)) + # Defining the x-y and variable used for coloring
      scale_y_continuous(limits = c(0, 1)) + # Adjusting y-axis scale
      geom_line(size = 1) + # Adjusting line size
      theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + # Changing background and legend specific elements
      ggtitle("Mean Group Segregation Over Steps") + # Adding plot title
      facet_grid(~type, labeller = as_labeller(type_grp_names)) + # Adding different plots for each group
      xlab("Steps") + ylab("Group Information Value") + # Adding x and y axis labels
      scale_color_brewer(palette = "Set1") # Defining the coloring scheme for distinguishing countries
    
    # The plot object is returned as function output
    return(grp_plot)
  }
}

#### Libraries ####
# install.packages(c("tidyverse"))
library(tidyverse)

#### Pre-amble (for all cases) ####
# Creating definitions of variable names for plotting
ratio_agg_names <- c(
  `info_seg` = "Information Value",
  `share_happy` = "Share of Happy Agents",
  `share_seg` = "Share of Segregated Agents"
)

type_grp_names <- c(
  `0` = "Blue",
  `1` = "Red",
  `2` = "Pink",
  `3` = "Lightblue"
)

th_cases_names <- c(
  `4` = "Threshold = 4",
  `5` = "Threshold = 5",
  `6` = "Threshold = 6"
)

nb_cases_names <- c(
  `0.25` = "Neigh. U. = 0.25",
  `0.5` = "Neigh. U. = 0.5",
  `0.75` = "Neigh. U. = 0.75"
)

el_cases_names <- c(
  `0.5` = "Election U. = 0.5",
  `1` = "Election U. = 1",
  `1.5` = "Election U. = 1.5"
)

#### Baseline model ####
# Loading data
data_us <- read.csv("data/out_us.csv", stringsAsFactors = F)
data_uk <- read.csv("data/out_uk.csv", stringsAsFactors = F)
data_aus <- read.csv("data/out_aus.csv", stringsAsFactors = F)

# Cleaning data for each country
us_results <- complete_calc(data_out = data_us)
uk_results <- complete_calc(data_out = data_uk)
aus_results <- complete_calc(data_out = data_aus)

# Combining country cases
all_agg_ratios <- bind_rows(list(us_results[[1]], uk_results[[1]], aus_results[[1]]))
all_grp_ratios <- bind_rows(list(us_results[[2]], uk_results[[2]], aus_results[[2]]))

# Re-ordering country labels
all_agg_ratios$cnt <- factor(all_agg_ratios$cnt, levels = c("UK", "US", "AUS"))
all_grp_ratios$cnt <- factor(all_grp_ratios$cnt, levels = c("UK", "US", "AUS"))

# Plotting aggregate measures over steps and contrasting country cases
agg_ratios_plot <- agg_results_plotter(all_agg_ratios, cases = F)
pdf("Plots/agg_ratios.pdf", width = 9)
agg_ratios_plot
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types
grp_ratios_plot <- grp_results_plotter(all_grp_ratios, cases = F)
pdf("Plots/grp_ratios.pdf", width = 9)
grp_ratios_plot
dev.off()

#### Baseline model with 1000 steps ####
# Loading data
data_us_1000 <- read.csv("data/out_us_1000.csv", stringsAsFactors = F)
data_uk_1000 <- read.csv("data/out_uk_1000.csv", stringsAsFactors = F)
data_aus_1000 <- read.csv("data/out_aus_1000.csv", stringsAsFactors = F)

# Cleaning data for each country
us_results_1000 <- complete_calc(data_out = data_us_1000)
uk_results_1000 <- complete_calc(data_out = data_uk_1000)
aus_results_1000 <- complete_calc(data_out = data_aus_1000)

# Combining country cases
all_agg_ratios_1000 <- bind_rows(list(us_results_1000[[1]], uk_results_1000[[1]], aus_results_1000[[1]]))
all_grp_ratios_1000 <- bind_rows(list(us_results_1000[[2]], uk_results_1000[[2]], aus_results_1000[[2]]))

# Re-ordering country labels
all_agg_ratios_1000$cnt <- factor(all_agg_ratios_1000$cnt, levels = c("UK", "US", "AUS"))
all_grp_ratios_1000$cnt <- factor(all_grp_ratios_1000$cnt, levels = c("UK", "US", "AUS"))

# Plotting aggregate measures over steps and contrasting country cases with 1000 steps
agg_ratios_plot_1000 <- agg_results_plotter(all_agg_ratios_1000, cases = F)
pdf("Plots/agg_ratios_1000.pdf", width = 9)
agg_ratios_plot_1000
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types with 1000 steps
grp_ratios_plot_1000 <- grp_results_plotter(all_grp_ratios_1000, cases = F)
pdf("Plots/grp_ratios_1000.pdf", width = 9)
grp_ratios_plot_1000
dev.off()

#### Varying utility threshold cases ####
# Loading data
data_us_th <- read.csv("data/out_us_th.csv", stringsAsFactors = F)
data_uk_th <- read.csv("data/out_uk_th.csv", stringsAsFactors = F)
data_aus_th <- read.csv("data/out_aus_th.csv", stringsAsFactors = F)

# Cleaning data for each country
us_th_results <- complete_calc(data_out = data_us_th)
uk_th_results <- complete_calc(data_out = data_uk_th)
aus_th_results <- complete_calc(data_out = data_aus_th)

# Combining country cases
all_th_agg_ratios <- bind_rows(list(us_th_results[[1]], uk_th_results[[1]], aus_th_results[[1]]))
all_th_grp_ratios <- bind_rows(list(us_th_results[[2]], uk_th_results[[2]], aus_th_results[[2]]))

# Re-ordering country labels
all_th_agg_ratios$cnt <- factor(all_th_agg_ratios$cnt, levels = c("UK", "US", "AUS"))
all_th_grp_ratios$cnt <- factor(all_th_grp_ratios$cnt, levels = c("UK", "US", "AUS"))

# Plotting aggregate measures over steps and contrasting country cases
th_agg_ratios_plot <- agg_results_plotter(all_th_agg_ratios, cases = T) + 
  ggtitle("Aggregate Ratios with Varying Threshold Utility (Mean of 100 Runs)") + 
  facet_grid(cases~Ratio, labeller = labeller(Ratio = as_labeller(ratio_agg_names), 
                                              cases = as_labeller(th_cases_names)))

pdf("Plots/th_agg_ratios.pdf", width = 9)
th_agg_ratios_plot
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types
th_grp_ratios_plot <- grp_results_plotter(all_th_grp_ratios, cases = T) +
  ggtitle("Group Segregation with Varying Threshold Utility (Mean of 100 Runs)") + 
  facet_grid(cases~type, labeller = labeller(type = as_labeller(type_grp_names), 
                                             cases = as_labeller(th_cases_names))) 

pdf("Plots/th_grp_ratios.pdf", width = 9)
th_grp_ratios_plot
dev.off()

#### Varying neighborhood utility ####
# Loading data
data_us_nb <- read.csv("data/out_us_nb.csv", stringsAsFactors = F)
data_uk_nb <- read.csv("data/out_uk_nb.csv", stringsAsFactors = F)
data_aus_nb <- read.csv("data/out_aus_nb.csv", stringsAsFactors = F)

# Cleaning data for each country
us_nb_results <- complete_calc(data_out = data_us_nb)
uk_nb_results <- complete_calc(data_out = data_uk_nb)
aus_nb_results <- complete_calc(data_out = data_aus_nb)

# Combining country cases
all_nb_agg_ratios <- bind_rows(list(us_nb_results[[1]], uk_nb_results[[1]], aus_nb_results[[1]]))
all_nb_grp_ratios <- bind_rows(list(us_nb_results[[2]], uk_nb_results[[2]], aus_nb_results[[2]]))

# Re-ordering country labels
all_nb_agg_ratios$cnt <- factor(all_nb_agg_ratios$cnt, levels = c("UK", "US", "AUS"))
all_nb_grp_ratios$cnt <- factor(all_nb_grp_ratios$cnt, levels = c("UK", "US", "AUS"))

# Plotting aggregate measures over steps and contrasting country cases
nb_agg_ratios_plot <- agg_results_plotter(all_nb_agg_ratios, cases = T) + 
  ggtitle("Aggregate Ratios with Varying Neighborhood Utility (Mean of 100 Runs)") + 
  facet_grid(cases~Ratio, labeller = labeller(Ratio = as_labeller(ratio_agg_names), 
                                              cases = as_labeller(nb_cases_names)))

pdf("Plots/nb_agg_ratios.pdf", width = 9)
nb_agg_ratios_plot
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types
nb_grp_ratios_plot <- grp_results_plotter(all_nb_grp_ratios, cases = T) +
  ggtitle("Group Segregation with Varying Neighborhood Utility (Mean of 100 Runs)") + 
  facet_grid(cases~type, labeller = labeller(type = as_labeller(type_grp_names), 
                                             cases = as_labeller(nb_cases_names))) 

pdf("Plots/nb_grp_ratios.pdf", width = 9)
nb_grp_ratios_plot
dev.off()

#### Varying election utility ####
# Loading data
data_us_el <- read.csv("data/out_us_el.csv", stringsAsFactors = F)
data_uk_el <- read.csv("data/out_uk_el.csv", stringsAsFactors = F)
data_aus_el <- read.csv("data/out_aus_el.csv", stringsAsFactors = F)


# Cleaning data for each country
us_el_results <- complete_calc(data_out = data_us_el)
uk_el_results <- complete_calc(data_out = data_uk_el)
aus_el_results <- complete_calc(data_out = data_aus_el)

# Combining country cases
all_el_agg_ratios <- bind_rows(list(us_el_results[[1]], uk_el_results[[1]], aus_el_results[[1]]))
all_el_grp_ratios <- bind_rows(list(us_el_results[[2]], uk_el_results[[2]], aus_el_results[[2]]))

# Re-ordering country labels
all_el_agg_ratios$cnt <- factor(all_el_agg_ratios$cnt, levels = c("UK", "US", "AUS"))
all_el_grp_ratios$cnt <- factor(all_el_grp_ratios$cnt, levels = c("UK", "US", "AUS"))

# Plotting aggregate measures over steps and contrasting country cases
el_agg_ratios_plot <- agg_results_plotter(all_el_agg_ratios, cases = T) + 
  ggtitle("Aggregate Ratios with Varying Election Utility (Mean of 100 Runs)") + 
  facet_grid(cases~Ratio, labeller = labeller(Ratio = as_labeller(ratio_agg_names), 
                                              cases = as_labeller(el_cases_names)))

pdf("Plots/el_agg_ratios.pdf", width = 9)
el_agg_ratios_plot
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types
el_grp_ratios_plot <- grp_results_plotter(all_el_grp_ratios, cases = T) +
  ggtitle("Group Segregation with Varying Election Utility (Mean of 100 Runs)") + 
  facet_grid(cases~type, labeller = labeller(type = as_labeller(type_grp_names), 
                                             cases = as_labeller(el_cases_names))) 

pdf("Plots/el_grp_ratios.pdf", width = 9)
el_grp_ratios_plot
dev.off()

#### Other analysis ####
### Box plots for visualizing variation in aggregate measures after 100 steps
box_plot_agg <- all_agg_ratios %>%
  ungroup() %>%
  group_by(run, cnt) %>%
  filter(steps == 100) %>% # Only focus on the last step of the simulation
  gather(key = "Ratios", value = "Value", info_seg:share_seg) %>%
  ggplot(aes(x = cnt, y = Value, fill = cnt)) + geom_boxplot(alpha = 0.6) + theme_bw() + 
    ggtitle("Variation in Segregation Measures (After 100 Steps)") + scale_y_continuous(limits = c(0, 1)) + 
    xlab("Country") + facet_grid(~Ratios, labeller = labeller(Ratios = as_labeller(ratio_agg_names))) + 
    scale_fill_brewer(palette = "Set1") + theme_bw() + theme(legend.position = "bottom", legend.title= element_blank())

pdf("Plots/box_plot_agg.pdf")
box_plot_agg
dev.off()

## Election changes calculations
# Election outcomes are stored in the 3rd element of the list
us_elec_ratios <- us_results[[3]]
uk_elec_ratios <- uk_results[[3]]
aus_elec_ratios <- aus_results[[3]]

# Combining all country cases
all_elec_ratios <- bind_rows(list(us_elec_ratios, uk_elec_ratios, aus_elec_ratios))

# Re-labeling election cases
all_elec_ratios$cnt <- factor(all_elec_ratios$cnt, levels = c("UK", "US", "AUS"))

# Calculating mean change in location over runs
all_elec_ratios_adj <- all_elec_ratios %>%
  filter(!is.na(change_loc)) %>%
  group_by(steps, cnt) %>%
  summarize(elec_ratio = mean(change_loc))

# Plotting fraction of locations that have changing election outcome over-time
election_plot <- ggplot(all_elec_ratios_adj, aes(x = steps, y = elec_ratio, color = cnt)) + geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") + xlab("Steps") + ylab("Fraction of Locations") +
  ggtitle("Fraction of Locations That Have a Changing \nElection Outcome (Mean of 100 Runs)") +
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank())

pdf("Plots/election_plot.pdf")
election_plot
dev.off()
