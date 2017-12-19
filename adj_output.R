#### Setting workng directory ####
setwd("C:\\Users\\gedmi\\Desktop\\SE Project\\Further\\New\\AxelHSG")

#### Functions ####
complete_calc <- function(data_out){
  colnames(data_out)[1] <- "steps"
  data_out$steps <- data_out$steps+1
  
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
  
  colnames_elect <- paste0("elect", 1:9)
  colnames_total <- paste0("loc_total", 1:9)
  colnames_grp0 <- paste0("loc_grp_total0", 1:9)
  colnames_grp1 <- paste0("loc_grp_total1", 1:9)
  colnames_grp2 <- paste0("loc_grp_total2", 1:9)
  colnames_grp3 <- paste0("loc_grp_total3", 1:9)
  
  data_out[, c(colnames_elect, colnames_total, colnames_grp0, colnames_grp1, colnames_grp2, colnames_grp3)] <- NA
  
  data_out[, colnames_elect] <- matrix(as.numeric(unlist(strsplit(data_out[, "elections"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_total] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_total"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_grp0] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_0"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_grp1] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_1"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_grp2] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_2"], ","))), ncol = 9, byrow = T)
  data_out[, colnames_grp3] <- matrix(as.numeric(unlist(strsplit(data_out[, "location_3"], ","))), ncol = 9, byrow = T)
  
  data_out[, "group_total0"] <- data_out[,"total_0"]
  data_out[, "group_total1"] <- data_out[,"total_1"]
  data_out[, "group_total2"] <- data_out[,"total_2"]
  data_out[, "group_total3"] <- data_out[,"total_3"]
  
  data_out <- data_out[, -which(names(data_out) %in% c("elections", "location_total", "location_0", "location_1", "location_2", "location_3",
                                                       "total_0", "total_1", "total_2", "total_3"))]
  
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
  
  data_calc1 <- data_store %>%
    group_by(run, steps, loc, cases) %>%
    mutate(total = sum(group_total),
           pi_m = group_total/total,
           pi_jm = loc_grp_total/loc_total,
           e = sum(pi_m*log(1/pi_m)),
           pi_jm_log = ifelse(pi_jm == 0, 0.001, pi_jm),
           share_happy = happy/total,
           share_seg = seg_agents/total) %>%
    ungroup()
  
  # Calculating seggregation measures. Names correspond to the numbering in Notebook files
  data_ratios1 <- data_calc1 %>%
    group_by(run, steps, cnt, cases) %>%
    summarize(info_seg = sum((loc_total/(total*e))*pi_jm*log(pi_jm_log/pi_m)),
              share_happy = mean(share_happy),
              share_seg = mean(share_seg))
  
  
  data_calc2 <- data_store %>%
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
  
  data_ratios2 <- data_calc2 %>%
    group_by(run, type, steps, cnt, cases) %>%
    summarize(grp_info = sum((loc_total/(total*e_grp))*pi_jm*log(pi_jm_log/pi_m) + (loc_total/(total*e_grp))*pi_other_j*log(pi_other_j_log/pi_other)))
  
  return(list(data_ratios1, data_ratios2, data_store))
}

#### Libraries ####
library(tidyverse)
library(xtable)

start_time <- Sys.time()
#### Adjusting Python data ####
data_us <- read.csv("data/out_us.csv", stringsAsFactors = F)
data_uk <- read.csv("data/out_uk.csv", stringsAsFactors = F)
data_aus <- read.csv("data/out_aus.csv", stringsAsFactors = F)

data_us_th <- read.csv("data/out_us_th.csv", stringsAsFactors = F)
data_uk_th <- read.csv("data/out_uk_th.csv", stringsAsFactors = F)
data_aus_th <- read.csv("data/out_aus_th.csv", stringsAsFactors = F)

data_us_nb <- read.csv("data/out_us_nb.csv", stringsAsFactors = F)
data_uk_nb <- read.csv("data/out_uk_nb.csv", stringsAsFactors = F)
data_aus_nb <- read.csv("data/out_aus_nb.csv", stringsAsFactors = F)

data_us_el <- read.csv("data/out_us_el.csv", stringsAsFactors = F)
data_uk_el <- read.csv("data/out_uk_el.csv", stringsAsFactors = F)
data_aus_el <- read.csv("data/out_aus_el.csv", stringsAsFactors = F)

# Defining parameters of the simulation

start_time <- Sys.time()
print(Sys.time())
# Cleaning data for each country
us_results <- complete_calc(data_out = data_us)
step1_time <- Sys.time()
print(Sys.time())

uk_results <- complete_calc(data_out = data_uk)
step2_time <- Sys.time()
print(Sys.time())

aus_results <- complete_calc(data_out = data_aus)
step3_time <- Sys.time()
print(Sys.time())

# 
us_th_results <- complete_calc(data_out = data_us_th)
step1_th_time <- Sys.time()
print(Sys.time())

uk_th_results <- complete_calc(data_out = data_uk_th)
step2_th_time <- Sys.time()
print(Sys.time())

aus_th_results <- complete_calc(data_out = data_aus_th)
step3_th_time <- Sys.time()
print(Sys.time())

#

us_nb_results <- complete_calc(data_out = data_us_nb)
step1_nb_time <- Sys.time()
print(Sys.time())

uk_nb_results <- complete_calc(data_out = data_uk_nb)
step2_nb_time <- Sys.time()
print(Sys.time())

aus_nb_results <- complete_calc(data_out = data_aus_nb)
step3_nb_time <- Sys.time()
print(Sys.time())

#

us_el_results <- complete_calc(data_out = data_us_el)
step1_el_time <- Sys.time()
print(Sys.time())

uk_el_results <- complete_calc(data_out = data_uk_el)
step2_el_time <- Sys.time()
print(Sys.time())

aus_el_results <- complete_calc(data_out = data_aus_el)
step3_el_time <- Sys.time()
print(Sys.time())

# Calculating aggregate ratios
us_agg_ratios <- us_results[[1]]
uk_agg_ratios <- uk_results[[1]]
aus_agg_ratios <- aus_results[[1]]

# Calculating group ratios
us_grp_ratios <- us_results[[2]]
uk_grp_ratios <- uk_results[[2]]
aus_grp_ratios <- aus_results[[2]]

# Calculating aggregate ratios
us_th_agg_ratios <- us_th_results[[1]]
uk_th_agg_ratios <- uk_th_results[[1]]
aus_th_agg_ratios <- aus_th_results[[1]]

# Calculating group ratios
us_th_grp_ratios <- us_th_results[[2]]
uk_th_grp_ratios <- uk_th_results[[2]]
aus_th_grp_ratios <- aus_th_results[[2]]

# Calculating aggregate ratios
us_el_agg_ratios <- us_el_results[[1]]
uk_el_agg_ratios <- uk_el_results[[1]]
aus_el_agg_ratios <- aus_el_results[[1]]

# Calculating group ratios
us_el_grp_ratios <- us_el_results[[2]]
uk_el_grp_ratios <- uk_el_results[[2]]
aus_el_grp_ratios <- aus_el_results[[2]]

# Calculating aggregate ratios
us_nb_agg_ratios <- us_nb_results[[1]]
uk_nb_agg_ratios <- uk_nb_results[[1]]
aus_nb_agg_ratios <- aus_nb_results[[1]]

# Calculating group ratios
us_nb_grp_ratios <- us_nb_results[[2]]
uk_nb_grp_ratios <- uk_nb_results[[2]]
aus_nb_grp_ratios <- aus_nb_results[[2]]

# Combining country cases
all_agg_ratios <- bind_rows(list(us_agg_ratios, uk_agg_ratios, aus_agg_ratios))
all_agg_ratios$cnt <- factor(all_agg_ratios$cnt, levels = c("UK", "US", "AUS"))

all_grp_ratios <- bind_rows(list(us_grp_ratios, uk_grp_ratios, aus_grp_ratios))
all_grp_ratios$cnt <- factor(all_grp_ratios$cnt, levels = c("UK", "US", "AUS"))

all_th_agg_ratios <- bind_rows(list(us_th_agg_ratios, uk_th_agg_ratios, aus_th_agg_ratios))
all_th_agg_ratios$cnt <- factor(all_th_agg_ratios$cnt, levels = c("UK", "US", "AUS"))

all_th_grp_ratios <- bind_rows(list(us_th_grp_ratios, uk_th_grp_ratios, aus_th_grp_ratios))
all_th_grp_ratios$cnt <- factor(all_th_grp_ratios$cnt, levels = c("UK", "US", "AUS"))

all_nb_agg_ratios <- bind_rows(list(us_nb_agg_ratios, uk_nb_agg_ratios, aus_nb_agg_ratios))
all_nb_agg_ratios$cnt <- factor(all_nb_agg_ratios$cnt, levels = c("UK", "US", "AUS"))

all_nb_grp_ratios <- bind_rows(list(us_nb_grp_ratios, uk_nb_grp_ratios, aus_nb_grp_ratios))
all_nb_grp_ratios$cnt <- factor(all_nb_grp_ratios$cnt, levels = c("UK", "US", "AUS"))

all_el_agg_ratios <- bind_rows(list(us_el_agg_ratios, uk_el_agg_ratios, aus_el_agg_ratios))
all_el_agg_ratios$cnt <- factor(all_el_agg_ratios$cnt, levels = c("UK", "US", "AUS"))

all_el_grp_ratios <- bind_rows(list(us_el_grp_ratios, uk_el_grp_ratios, aus_el_grp_ratios))
all_el_grp_ratios$cnt <- factor(all_el_grp_ratios$cnt, levels = c("UK", "US", "AUS"))

# Plotting aggregate measures over steps and contrasting country cases
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

all_agg_ratios_plot <- all_agg_ratios %>%
  group_by(cnt, steps) %>%
  summarize(info_seg = mean(info_seg),
            share_happy = mean(share_happy),
            share_seg = mean(share_seg)) %>%
  gather(key = "Ratio", value = "Value", info_seg, share_happy, share_seg) %>%
  ggplot(aes(x = steps, y = Value, color = cnt)) + scale_y_continuous(limits = c(0, 1)) +
    geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  ggtitle("Aggregate Ratios Over Steps (Mean of 100 Runs)") + 
  facet_grid(~Ratio, labeller = as_labeller(ratio_agg_names)) + xlab("Steps") + 
  scale_color_brewer(palette = "Set1")
  

pdf("Plots/all_agg_ratios.pdf")
all_agg_ratios_plot
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types
all_grp_ratios_plot <- all_grp_ratios %>%
  group_by(cnt, steps, type) %>%
  summarize(grp_info = mean(grp_info)) %>%
  ggplot(aes(x = steps, y = grp_info, color = cnt)) + scale_y_continuous(limits = c(0, 1)) +
    geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + 
    ggtitle("Group Segregation Over Steps (Mean of 100 Runs)") + 
    facet_grid(~type, labeller = as_labeller(type_grp_names)) + xlab("Steps") + ylab("Group Information Value") +
    scale_color_brewer(palette = "Set1")

pdf("Plots/all_grp_ratios.pdf")
all_grp_ratios_plot
dev.off()

# Plotting aggregate measures over steps and contrasting country cases
th_cases_names <- c(
  `4` = "Threshold = 4",
  `5` = "Threshold = 5",
  `6` = "Threshold = 6"
)

all_th_agg_ratios_plot <- all_th_agg_ratios %>%
  group_by(cnt, steps, cases) %>%
  summarize(info_seg = mean(info_seg),
            share_happy = mean(share_happy),
            share_seg = mean(share_seg)) %>%
  gather(key = "Ratio", value = "Value", info_seg, share_happy, share_seg) %>%
  ggplot(aes(x = steps, y = Value, color = cnt)) + scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  ggtitle("Aggregate Ratios with Varying Threshold Utility (Mean of 100 Runs)") + 
  facet_grid(cases~Ratio, labeller = labeller(Ratio = as_labeller(ratio_agg_names), 
                                              cases = as_labeller(th_cases_names))) +
  xlab("Steps") + scale_color_brewer(palette = "Set1")

pdf("Plots/all_th_agg_ratios.pdf")
all_th_agg_ratios_plot
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types
all_th_grp_ratios_plot <- all_th_grp_ratios %>%
  group_by(cnt, steps, type, cases) %>%
  summarize(grp_info = mean(grp_info)) %>%
  ggplot(aes(x = steps, y = grp_info, color = cnt)) + scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  ggtitle("Group Segregation with Varying Threshold Utility (Mean of 100 Runs)") + 
  facet_grid(cases~type, labeller = labeller(type = as_labeller(type_grp_names), 
                                              cases = as_labeller(th_cases_names))) +
  xlab("Steps") + ylab("Group Information Value") + scale_color_brewer(palette = "Set1")

pdf("Plots/all_th_grp_ratios.pdf")
all_th_grp_ratios_plot
dev.off()

# Plotting aggregate measures over steps and contrasting country cases
nb_cases_names <- c(
  `0.25` = "Neigh. U. = 0.25",
  `0.5` = "Neigh. U. = 0.5",
  `0.75` = "Neigh. U. = 0.75"
)

all_nb_agg_ratios_plot <- all_nb_agg_ratios %>%
  group_by(cnt, steps, cases) %>%
  summarize(info_seg = mean(info_seg),
            share_happy = mean(share_happy),
            share_seg = mean(share_seg)) %>%
  gather(key = "Ratio", value = "Value", info_seg, share_happy, share_seg) %>%
  ggplot(aes(x = steps, y = Value, color = cnt)) + scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  ggtitle("Aggregate Ratios with Varying Neighborhood Utility (Mean of 100 Runs)") + 
  facet_grid(cases~Ratio, labeller = labeller(Ratio = as_labeller(ratio_agg_names), 
                                              cases = as_labeller(nb_cases_names))) +
  xlab("Steps") + scale_color_brewer(palette = "Set1")

pdf("Plots/all_nb_agg_ratios.pdf")
all_nb_agg_ratios_plot
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types
all_nb_grp_ratios_plot <- all_nb_grp_ratios %>%
  group_by(cnt, steps, type, cases) %>%
  summarize(grp_info = mean(grp_info)) %>%
  ggplot(aes(x = steps, y = grp_info, color = cnt)) + scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  ggtitle("Group Segregation with Varying Neighborhood Utility (Mean of 100 Runs)") + 
  facet_grid(cases~type, labeller = labeller(type = as_labeller(type_grp_names), 
                                             cases = as_labeller(nb_cases_names))) +
  xlab("Steps") + ylab("Group Information Value") + scale_color_brewer(palette = "Set1")

pdf("Plots/all_nb_grp_ratios.pdf")
all_nb_grp_ratios_plot
dev.off()

# Plotting aggregate measures over steps and contrasting country cases
el_cases_names <- c(
  `0.5` = "Election U. = 0.5",
  `1` = "Election U. = 1",
  `1.5` = "Election U. = 1.5"
)

all_el_agg_ratios_plot <- all_el_agg_ratios %>%
  group_by(cnt, steps, cases) %>%
  summarize(info_seg = mean(info_seg),
            share_happy = mean(share_happy),
            share_seg = mean(share_seg)) %>%
  gather(key = "Ratio", value = "Value", info_seg, share_happy, share_seg) %>%
  ggplot(aes(x = steps, y = Value, color = cnt)) + scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  ggtitle("Aggregate Ratios with Varying Election Utility (Mean of 100 Runs)") + 
  facet_grid(cases~Ratio, labeller = labeller(Ratio = as_labeller(ratio_agg_names), 
                                              cases = as_labeller(el_cases_names))) +
  xlab("Steps") + scale_color_brewer(palette = "Set1")

pdf("Plots/all_el_agg_ratios.pdf")
all_el_agg_ratios_plot
dev.off()

# Plotting group specific measures over steps and contrasting segregation across different types
all_el_grp_ratios_plot <- all_el_grp_ratios %>%
  group_by(cnt, steps, type, cases) %>%
  summarize(grp_info = mean(grp_info)) %>%
  ggplot(aes(x = steps, y = grp_info, color = cnt)) + scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 1) + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) + 
  ggtitle("Group Segregation with Varying Election Utility (Mean of 100 Runs)") + 
  facet_grid(cases~type, labeller = labeller(type = as_labeller(type_grp_names), 
                                             cases = as_labeller(el_cases_names))) +
  xlab("Steps") + ylab("Group Information Value") + scale_color_brewer(palette = "Set1")

pdf("Plots/all_el_grp_ratios.pdf")
all_el_grp_ratios_plot
dev.off()

## Baseline case tables
# Aggregate ratios
all_agg_ratios_table <- all_agg_ratios %>%
  ungroup() %>%
  filter(steps == 25 | steps == 50| steps == 75 | steps == 100) %>%
  group_by(steps, cnt) %>%
  summarize(mean_happy = round(mean(share_happy),4),
            mean_seg = round(mean(share_seg),4),
            mean_info = round(mean(info_seg), 4)) %>%
  ungroup() %>%
  mutate(steps = sprintf("%1.f", steps),
         mean_happy = sprintf("%1.2f%%", 100*mean_happy),
         mean_seg = sprintf("%1.2f%%", 100*mean_seg),
         mean_info = sprintf("%1.4f", mean_info))

colnames(all_agg_ratios_table) <- c("Steps", "Country", "Happy", "Segregated", "Information Value")

print(xtable(all_agg_ratios_table, type = "latex"), include.rownames = F, file = "Tables/all_agg_ratios.tex")

# Group-based measures
all_grp_ratios_table <- all_grp_ratios %>%
  ungroup() %>%
  filter(steps == 25 | steps == 50| steps == 75 | steps == 100) %>%
  group_by(steps, cnt, type) %>%
  summarize(grp_info = round(mean(grp_info),4)) %>%
  ungroup() %>%
  mutate(steps = sprintf("%1.f", steps),
         grp_info = sprintf("%1.4f%%", grp_info))

colnames(all_grp_ratios_table) <- c("Steps", "Country", "Type", "Group Information Value")

print(xtable(all_grp_ratios_table, type = "latex"), include.rownames = F, file = "Tables/all_grp_ratios.tex")

## Baseline case tables
# Aggregate ratios
th_agg_ratios_table <- all_th_agg_ratios %>%
  ungroup() %>%
  filter(steps == 25 | steps == 50| steps == 75 | steps == 100) %>%
  group_by(steps, cnt, cases) %>%
  summarize(mean_happy = round(mean(share_happy),4),
            mean_seg = round(mean(share_seg),4),
            mean_info = round(mean(info_seg), 4)) %>%
  ungroup() %>%
  mutate(steps = sprintf("%1.f", steps),
         mean_happy = sprintf("%1.2f%%", 100*mean_happy),
         mean_seg = sprintf("%1.2f%%", 100*mean_seg),
         mean_info = sprintf("%1.4f", mean_info),
         cases = paste0("Case ", cases))

colnames(th_agg_ratios_table) <- c("Steps", "Country", "Threshold Cases", "Happy", "Segregated", "Information Value")

print(xtable(th_agg_ratios_table, type = "latex"), include.rownames = F, file = "Tables/th_agg_ratios.tex")

# Group-based measures
th_grp_ratios_table <- all_th_grp_ratios %>%
  ungroup() %>%
  filter(steps == 25 | steps == 50| steps == 75 | steps == 100) %>%
  group_by(steps, cnt, type, cases) %>%
  summarize(grp_info = round(mean(grp_info),4)) %>%
  ungroup() %>%
  mutate(steps = sprintf("%1.f", steps),
         grp_info = sprintf("%1.4f", grp_info),
         cases = paste0("Case: ", cases)) %>%
  spread(key = "cases", value = "grp_info")

colnames(th_grp_ratios_table) <- c("Steps", "Country", "Type", paste0("Group Information Value (Threshold: ", 4:6, ")"))

print(xtable(th_grp_ratios_table, type = "latex"), include.rownames = F, file = "Tables/th_grp_ratios.tex")

## Changing neighborhood utility tables
# Aggregate ratios
nb_agg_ratios_table <- all_nb_agg_ratios %>%
  ungroup() %>%
  filter(steps == 25 | steps == 50| steps == 75 | steps == 100) %>%
  group_by(steps, cnt, cases) %>%
  summarize(mean_happy = round(mean(share_happy),4),
            mean_seg = round(mean(share_seg),4),
            mean_info = round(mean(info_seg), 4)) %>%
  ungroup() %>%
  mutate(steps = sprintf("%1.f", steps),
         mean_happy = sprintf("%1.2f%%", 100*mean_happy),
         mean_seg = sprintf("%1.2f%%", 100*mean_seg),
         mean_info = sprintf("%1.4f", mean_info),
         cases = paste0("Case: ", cases))

colnames(nb_agg_ratios_table) <- c("Steps", "Country", "Neighborhood Utility", "Happy", "Segregated", "Information Value")

print(xtable(nb_agg_ratios_table, type = "latex"), include.rownames = F, file = "Tables/nb_agg_ratios.tex")

# Group-based measures
nb_grp_ratios_table <- all_nb_grp_ratios %>%
  ungroup() %>%
  filter(steps == 25 | steps == 50| steps == 75 | steps == 100) %>%
  group_by(steps, cnt, type, cases) %>%
  summarize(grp_info = round(mean(grp_info),4)) %>%
  ungroup() %>%
  mutate(steps = sprintf("%1.f", steps),
         grp_info = sprintf("%1.4f", grp_info),
         cases = paste0("Case: ", cases)) %>%
  spread(key = "cases", value = "grp_info")

colnames(nb_grp_ratios_table) <- c("Steps", "Country", "Type", paste0("Group Information Value (Neighborhood Utility: ", c(0.25, 0.5, 0.75), ")"))

print(xtable(nb_grp_ratios_table, type = "latex"), include.rownames = F, file = "Tables/nb_grp_ratios.tex")

## Chaging election utility
# Aggregate ratios
el_agg_ratios_table <- all_el_agg_ratios %>%
  ungroup() %>%
  filter(steps == 25 | steps == 50| steps == 75 | steps == 100) %>%
  group_by(steps, cnt, cases) %>%
  summarize(mean_happy = round(mean(share_happy),4),
            mean_seg = round(mean(share_seg),4),
            mean_info = round(mean(info_seg), 4)) %>%
  ungroup() %>%
  mutate(steps = sprintf("%1.f", steps),
         mean_happy = sprintf("%1.2f%%", 100*mean_happy),
         mean_seg = sprintf("%1.2f%%", 100*mean_seg),
         mean_info = sprintf("%1.4f", mean_info),
         cases = paste0("Case: ", cases))

colnames(el_agg_ratios_table) <- c("Steps", "Country", "Election Utility", "Happy", "Segregated", "Information Value")

print(xtable(el_agg_ratios_table, type = "latex"), include.rownames = F, file = "Tables/el_agg_ratios.tex")

# Group-based measures
el_grp_ratios_table <- all_el_grp_ratios %>%
  ungroup() %>%
  filter(steps == 25 | steps == 50| steps == 75 | steps == 100) %>%
  group_by(steps, cnt, type, cases) %>%
  summarize(grp_info = round(mean(grp_info),4)) %>%
  ungroup() %>%
  mutate(steps = sprintf("%1.f", steps),
         grp_info = sprintf("%1.4f", grp_info),
         cases = paste0("Case: ", cases)) %>%
  spread(key = "cases", value = "grp_info")

colnames(el_grp_ratios_table) <- c("Steps", "Country", "Type", paste0("Group Information Value (Election Utility: ", c(0.5, 1, 1.5), ")"))

print(xtable(el_grp_ratios_table, type = "latex"), include.rownames = F, file = "Tables/el_grp_ratios.tex")

step_final <- Sys.time()
print(step_final - start_time)

### Box plots for visualizing variation in outcomes
# Aggregate measures
box_plot_agg <- all_agg_ratios %>%
  ungroup() %>%
  group_by(run, cnt) %>%
  filter(steps == 100) %>%
  gather(key = "Ratios", value = "Value", info_seg:share_seg) %>%
  ggplot(aes(x = cnt, y = Value, fill = cnt)) + geom_boxplot(alpha = 0.6) + theme_bw() + 
    ggtitle("Variation in Segregation Measures (After 100 Steps)") + scale_y_continuous(limits = c(0, 1)) + 
    xlab("Country") + facet_grid(~Ratios, labeller = labeller(Ratios = as_labeller(ratio_agg_names))) + 
    scale_fill_brewer(palette = "Set1") + theme_bw() + theme(legend.position = "bottom", legend.title= element_blank())

pdf("Plots/box_plot_agg.pdf")
box_plot_agg
dev.off()

# Chnges by Utility Threshold
box_plot_th <- all_th_agg_ratios %>%
  ungroup() %>%
  group_by(run, cnt) %>%
  filter(steps == 100) %>%
  gather(key = "Ratios", value = "Value", info_seg:share_seg) %>%
  ggplot(aes(x = as.factor(cases), y = Value, fill = as.factor(cnt))) + geom_boxplot(alpha = 0.6) +
  ggtitle("Variation in Segregation Measures (After 100 Steps)") + scale_y_continuous(limits = c(0, 1)) + 
  xlab("Varying Utility Threshold") + scale_fill_brewer(palette = "Set1") + 
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) +
  facet_grid(cnt~Ratios, labeller = labeller(Ratios = as_labeller(ratio_agg_names)))

pdf("Plots/box_plot_th.pdf")
box_plot_th
dev.off()

# Chnges by Election Utility
box_plot_el <- all_el_agg_ratios %>%
  ungroup() %>%
  group_by(run, cnt) %>%
  filter(steps == 100) %>%
  gather(key = "Ratios", value = "Value", info_seg:share_seg) %>%
  ggplot(aes(x = as.factor(cases), y = Value, fill = as.factor(cnt))) + geom_boxplot(alpha = 0.6) +
  ggtitle("Variation in Segregation Measures (After 100 Steps)") + scale_y_continuous(limits = c(0, 1)) + 
  xlab("Varying Election Utility") + scale_fill_brewer(palette = "Set1") + 
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) +
  facet_grid(cnt~Ratios, labeller = labeller(Ratios = as_labeller(ratio_agg_names)))

pdf("Plots/box_plot_el.pdf")
box_plot_el
dev.off()


# Chnges by Neighnorhood Utility
box_plot_nb <- all_nb_agg_ratios %>%
  ungroup() %>%
  group_by(run, cnt) %>%
  filter(steps == 100) %>%
  gather(key = "Ratios", value = "Value", info_seg:share_seg) %>%
  ggplot(aes(x = as.factor(cases), y = Value, fill = as.factor(cnt))) + geom_boxplot(alpha = 0.6) +
  ggtitle("Variation in Segregation Measures (After 100 Steps)") + scale_y_continuous(limits = c(0, 1)) + 
  xlab("Varying Neighborhood Threshold") + scale_fill_brewer(palette = "Set1") + 
  theme_bw() + theme(legend.position = "bottom", legend.title= element_blank()) +
  facet_grid(cnt~Ratios, labeller = labeller(Ratios = as_labeller(ratio_agg_names)))

pdf("Plots/box_plot_nb.pdf")
box_plot_nb
dev.off()
