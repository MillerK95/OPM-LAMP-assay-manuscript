#### Melt curve analysis of validation 

rm(list = ls())

library(tidyverse)
library(ggridges)
library(readxl)
library(viridis)
library(stringr)
library(stringi)
library(cowplot)
library(car)

##Reading in files

file_path <- ("C:/PhD/phd_work/LAMP_experiments/R_files/Input/val_plate_files/")

file_names<-file_path %>%
  list.files() %>%
  set_names(str_remove(gsub(" ", "_",.), ".xlsx"))

Sample_setup_df<- file_names %>%
  purrr::map_dfr(function(file_name){ # iterate through each file name
    read_excel(paste0(file_path, file_name),1, skip=45)
  }, .id="name")

Raw_amplification_df<- file_names %>%
  purrr::map_dfr(function(file_name){ # iterate through each file name
    read_excel(paste0(file_path, file_name),2, skip=45)
  }, .id="name")

Ct_results_df<- file_names %>%
  purrr::map_dfr(function(file_name){ # iterate through each file name
    read_excel(paste0(file_path, file_name),3, skip=45)
  }, .id="name")

Amp_results_df<- file_names %>%
  purrr::map_dfr(function(file_name){ # iterate through each file name
    read_excel(paste0(file_path, file_name),5, skip=45)
  }, .id="name")

Melt_curve_data_df<- file_names %>%
  purrr::map_dfr(function(file_name){ # iterate through each file name
    read_excel(paste0(file_path, file_name),6, skip=45)
  }, .id="name")

Melt_curve_results_df<- file_names %>%
  purrr::map_dfr(function(file_name){ # iterate through each file name
    read_excel(paste0(file_path, file_name),7, skip=45)
  }, .id="name")


##Metadata
status <- read.csv("C:/PhD/phd_work/LAMP_experiments/R_files/Output/high_val_plate_one.csv")
view(status)
status <- status %>% mutate(Colour = case_when(
  Status == "carc_pos" ~ '#000000',
  Status == "carc_neg" ~ '#E69F00',
  Status == "pcon" ~ '#56B4E9',
  Status == "ncon" ~ '#009E73',
))
status <- status %>% select(Well, Status, Colour)
status <- rename(status, "Well_Position" = "Well")


##Amp analysis 
amp_data <- Ct_results_df %>%  rename("Well_Position" = "Well Position", "Target" = "Target Name", "Fluorescence" = "Delta Rn")
amp_data <- left_join(amp_data, status, by = "Well_Position")
amp_data <- amp_data[grep("Target 2", amp_data$Target), ]
amp_data <- amp_data %>% mutate(Name = case_when(name == "2022_04_06_highprev_validation_LE1_primer" ~ "Old_Plate_LE1_Primer",
                                               name == "2023_02_14_highprev_validation_LE2_primer" ~ "New_Plate_LEF_Primer",
                                               name == "2023_02_17_original_val_plate_final_primer_set" ~ "Old_Plate_LEF_Primer"))
amp_data <- subset(amp_data, select = -c(Target, name))

##setting some parameters
name_order <- c("Old_Plate_LE1_Primer", "Old_Plate_LEF_Primer", "New_Plate_LEF_Primer")

# Define facet labels with correct order
facet_labels <- c("Old_Plate_LE1_Primer" = "Primer Set 1",
                  "Old_Plate_LEF_Primer" = "Primer Set F First",
                  "New_Plate_LEF_Primer" = "Primer Set F Replacement")

# Convert the "Name" variable to a factor with the specified order
amp_data$Name <- factor(amp_data$Name, levels = name_order)

##Plotting
amp_plot <- ggplot(data = amp_data, aes(Cycle, Fluorescence, group = Well_Position, colour = Colour)) +
  geom_line() +
  ylab("Fluorescence") +
  xlab("Cycle") +
  theme_bw() +
  facet_wrap(~Name, labeller = labeller(Name = as_labeller(facet_labels)))

amp_plot <- amp_plot + scale_colour_discrete(name = "Sample", labels = c(expression(italic("C. iliaca")* "Positive"), "Negative Control", "Positive Control", expression(italic("C. iliaca")* "Negative")))
amp_plot


##Melt curve analysis 
mc_data <- Melt_curve_data_df %>%  rename("Well_Position" = "Well Position", "Target" =  "Target Name")
mc_data <- left_join(mc_data, status, by = "Well_Position")
mc_data <- mc_data[grep("Target 2", mc_data$Target), ]
mc_data <- subset(mc_data, select = -(Target))
mc_data <- mc_data %>% mutate(Name = case_when(name == "2022_04_06_highprev_validation_LE1_primer" ~ "Old_Plate_LE1_Primer",
                                               name == "2023_02_14_highprev_validation_LE2_primer" ~ "New_Plate_LEF_Primer",
                                               name == "2023_02_17_original_val_plate_final_primer_set" ~ "Old_Plate_LEF_Primer"))
mc_data <- subset(mc_data, select = -(name))

##Multiplot
mc_plot <- ggplot(data = mc_data, aes(Temperature, Derivative, group = Well_Position, colour = Colour)) +
  geom_line() +
  ylab("Derivative") +
  xlab("Temperature (\u00B0C)") +
  theme_bw() +
  facet_wrap(~Name)
mc_plot <- mc_plot + scale_colour_discrete(name = "Sample", labels = c(expression(italic("C. iliaca")* "Positive"), "Negative Control", "Positive Control", expression(italic("C. iliaca")* "Negative")))
mc_plot

##Distribution of Ct 
end_data <- Amp_results_df %>%  rename("Well_Position" = "Well Position", "Target" = "Target Name", "Sample" =  "Sample Name", "CT_Mean" =  "Ct Mean", "CT_SD" =  "Ct SD", "CT_Thresh" =  "Ct Threshold", "MT" =  "Tm1")
end_data <- end_data %>% mutate(Name = case_when(name == "2022_04_06_highprev_validation_LE1_primer" ~ "Old_Plate_LE1_Primer",
                                               name == "2023_02_14_highprev_validation_LE2_primer" ~ "New_Plate_LEF_Primer",
                                               name == "2023_02_17_original_val_plate_final_primer_set" ~ "Old_Plate_LEF_Primer"))
end_data <- end_data[grep("Target 2", end_data$Target), ]
end_data <- subset(end_data, select = c(Name, Well, Well_Position, Sample, CT, CT_Mean, CT_SD, CT_Thresh, MT))
end_data$CT[end_data$CT == 'Undetermined'] <- '0'
end_data$CT <- as.numeric(end_data$CT)

## Going forward these plots are the final plots for thesis 
end_data <- end_data %>% mutate(ID = case_when(Sample == "water_neg" & CT == 0 ~ "WN_no_amp",
                                           Sample == "water_neg" & CT > 0 ~ "Water Negative with Amplification",
                                           Sample == "opm_neg" & CT == 0 ~ "ON_no_amp",
                                           Sample == "opm_neg" & CT > 0 ~ "OPM Negative with Amplification",
                                           Sample == "opm_lysate_plate_1" & CT >= 0 ~ "Positives",
                                           Sample == "pos" & CT >= 0 ~ "Positives"))

##plotting OPM negatives that consistently amplify
outlier_data <- end_data %>% filter(ID == "OPM Negative with Amplification", CT < 100)
outlier_data$Name <- factor(outlier_data$Name, levels = c("New_Plate_LEF_Primer", "Old_Plate_LEF_Primer", "Old_Plate_LE1_Primer"))

# Convert the "Name" variable to a factor with the specified order
outlier_data$Name <- factor(outlier_data$Name, levels = name_order)

outliers <- ggplot(data=outlier_data, aes(y = CT, x = MT, colour=Well_Position)) +
  facet_wrap(~Name, labeller = labeller(Name = as_labeller(facet_labels))) +
  geom_hline(yintercept=100) +
  geom_point() +
  theme_bw()

outliers <- outliers + scale_colour_discrete(name = "Well Position")

outliers

##Outlier MT mean
outlier_data %>%
  #Grouping by plate name
  group_by(Name) %>% 
  #Caluclating mean and sg grouped by plate name
  summarise(
    mean = mean(MT, na.rm = T),
    sd = sd(MT, na.rm = T)
  )

##Removing undetermined results and 0 CT results as none of these had meaningful amp or melt curves.
CT_plus100_data <- end_data %>% filter(ID != c("WN_no_amp", "ON_no_amp"), CT > 0)
CT_plus100_data$Name <- factor(CT_plus100_data$Name, levels = c("New_Plate_LEF_Primer", "Old_Plate_LEF_Primer", "Old_Plate_LE1_Primer"))

MT_above_100 <- CT_plus100_data %>% ggplot(aes(x = MT, y = ID)) +
                          geom_boxplot() +
                          scale_y_discrete(labels = function(y) str_wrap(y, width = 15)) +
                          facet_wrap(~Name, labeller = labeller(Name = as_labeller(facet_labels))) +
                          theme_bw()
MT_above_100 <- MT_above_100 + theme(axis.text.y = element_text(size = 15))

CT_above_100 <- CT_plus100_data %>% ggplot(aes(x = CT, y = ID)) +
                          geom_boxplot() +
                          scale_y_discrete(labels = function(y) str_wrap(y, width = 15)) +
                          facet_wrap(~Name, labeller = labeller(Name = as_labeller(facet_labels))) +
                          theme_bw()
CT_above_100 <- CT_above_100 + theme(axis.text = element_text(size = 10))

##Removing undetermined and also capping at 100 CT
CT_cap100_data <- end_data %>% filter(ID != c("WN_no_amp", "ON_no_amp"), CT <= 100, CT > 0 )
CT_cap100_data$Name <- factor(CT_cap100_data$Name, levels = c("New_Plate_LEF_Primer", "Old_Plate_LEF_Primer", "Old_Plate_LE1_Primer"))

MT_cap_100 <- CT_cap100_data %>% ggplot(aes(x = MT, y = ID)) +
              geom_boxplot() +
              scale_y_discrete(labels = function(y) str_wrap(y, width = 15)) +
              facet_wrap(~Name, labeller = labeller(Name = c("New_Plate_LEF_Primer" = "Primer Set F Replacement",
                                                             "Old_Plate_LEF_Primer" = "Primer Set F First",
                                                             "Old_Plate_LE1_Primer" = "Primer Set 1"))) +
              theme_bw()
MT_cap_100


CT_cap_100 <- CT_cap100_data %>% ggplot(aes(x = CT, y = ID)) +
                        geom_boxplot() +
                        scale_y_discrete(labels = function(y) str_wrap(y, width = 15)) +
                        facet_wrap(~Name, labeller = labeller(Name = c("New_Plate_LEF_Primer" = "Primer Set F Replacement",
                                                                        "Old_Plate_LEF_Primer" = "Primer Set F First",
                                                                        "Old_Plate_LE1_Primer" = "Primer Set 1"))) +
                        theme_bw()
CT_cap_100

end_data %>%
  #Grouping by plate name
  group_by(Name) %>% 
  #Caluclating mean and sg grouped by plate name
  summarise(
    mean = mean(MT, na.rm = T),
    sd = sd(MT, na.rm = T)
  )

##Cowplots
# Convert the "Name" variable to a factor with the specified order
CT_plus100_data$Name <- factor(CT_plus100_data$Name, levels = name_order)
CT_cap100_data$Name <- factor(CT_cap100_data$Name, levels = name_order)



##MT
##Removing y axis from right hand plot
cowMT_Cap <- CT_cap100_data %>% ggplot(aes(x = MT, y = ID)) +
  geom_boxplot() +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 15)) +
  facet_wrap(~Name, labeller = labeller(Name = as_labeller(facet_labels))) +
  xlim(78, 82) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 
cowMT_Cap

plot_grid(MT_above_100, cowMT_Cap, 
          labels = "AUTO", 
          label_size = 10,
          label_x = 0, label_y = 0,
          hjust = -0.5, vjust = -0.5,
          rel_widths = c(1.25,1))

##CT
##Removing y axis from right hand plot
cowCT_Cap <- CT_cap100_data %>% ggplot(aes(x = CT, y = ID)) +
  geom_boxplot() +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 15)) +
  facet_wrap(~Name, labeller = labeller(Name = as_labeller(facet_labels))) +
  xlim(0, 300) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 
cowCT_Cap 

##Keeping x axis the same
cowCT_Above <- CT_plus100_data %>% ggplot(aes(x = CT, y = ID)) +
  geom_boxplot() +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 15)) +
  facet_wrap(~Name, labeller = labeller(Name = as_labeller(facet_labels))) +
  xlim(0, 300) +
  theme_bw()

cowCT_Above <- cowCT_Above + theme(axis.text.y = element_text(size = 15))

plot_grid(cowCT_Above, cowCT_Cap, 
          labels = "AUTO", 
          label_size = 12,
          label_x = 0, label_y = 0,
          hjust = -0.5, vjust = -0.5,
          rel_widths = c(1.25,1))
##MT by CT 
# Plotting code with the modified order and wrapped labels
MTCT_scatter <- ggplot(data=CT_plus100_data, aes(y = CT, x = MT, colour = ID)) +
  facet_wrap(~Name, labeller = labeller(Name = as_labeller(facet_labels))) +
  geom_hline(yintercept=100) +
  geom_point() +
  theme_bw() +
  scale_colour_discrete(labels = c("OPM Negative \n with Amplification", 
                                   "Positives", 
                                   "Water Negative \n with Amplification"))

# Display the plot
print(MTCT_scatter)
##Stats 
##CT cap 100

CT100_aov <- aov(CT ~Name, data = CT_cap100_data)
hist(CT100_aov$residuals)
shapiro.test(CT100_aov$residuals)

tt <- TukeyHSD(CT100_aov)
tt$Name[,"p adj"]

MT100_aov <- aov(MT ~Name, data = CT_cap100_data)
hist(MT100_aov$residuals)
shapiro.test(MT100_aov$residuals)

tt <- TukeyHSD(MT100_aov)
tt$Name[,"p adj"]

##Sensivity and specificity 
##Checking true positives
end_data %>% 
  group_by(Name) %>% 
  filter(CT >0 & grepl("Positives", ID)) %>% 
  count()

##Check for false positives
end_data %>% 
  group_by(Name) %>% 
  filter( CT >0 & grepl("OPM Negative with Amplification", ID)) %>% 
  count()

end_data %>% 
  group_by(Name) %>% 
  filter(CT >0 & grepl("Water Negative with Amplification", ID)) %>% 
  count()
##Checking true negatives
end_data %>% 
  group_by(Name) %>% 
  filter( CT == 0 & grepl("OPM Negative with Amplification", ID)) %>% 
  count()

end_data %>% 
  group_by(Name) %>% 
  filter(CT == 0 & grepl("Water Negative with Amplification", ID)) %>% 
  count()

end_data %>% 
  group_by(Name) %>% 
  filter(CT == 0 & grepl("ON_no_amp", ID)) %>% 
  count()

end_data %>% 
  group_by(Name) %>% 
  filter(CT == 0 & grepl("WN_no_amp", ID)) %>% 
  count()

##Cap data 100 CT
##Checking true positives
end_data %>% 
  group_by(Name) %>% 
  filter(CT <= 100 & CT >0 & grepl("Positives", ID)) %>% 
  count()

##Check for false positives
end_data %>% 
  group_by(Name) %>% 
  filter(CT <= 100 & CT >0 & grepl("OPM Negative with Amplification", ID)) %>% 
  count()

end_data %>% 
  group_by(Name) %>% 
  filter(CT <= 100 & CT >0 & grepl("Water Negative with Amplification", ID)) %>% 
  count()
##Checking true negatives
end_data %>% 
  group_by(Name) %>% 
  filter(CT <= 100 & CT == 0 & grepl("OPM Negative with Amplification", ID)) %>% 
  count()

end_data %>% 
  group_by(Name) %>% 
  filter(CT <= 100 &CT == 0 & grepl("Water Negative with Amplification", ID)) %>% 
  count()

end_data %>% 
  group_by(Name) %>% 
  filter(CT <= 100 & CT == 0 & grepl("ON_no_amp", ID)) %>% 
  count()

end_data %>% 
  group_by(Name) %>% 
  filter(CT <= 100 & CT == 0 & grepl("WN_no_amp", ID)) %>% 
  count()

##Filtering samples to only samples in both the LEF old plate and LE1 plate for sensitivity and specificity

metric_data <- end_data %>% filter(Name != "New_Plate_LEF_Primer")
LEFmetric_data <- metric_data %>% filter(Name == "Old_Plate_LEF_Primer")
LE1metric_data <- metric_data %>% filter(Name == "Old_Plate_LE1_Primer")
LEFnmetric_data <- end_data %>% filter(Name == "New_Plate_LEF_Primer")

LEFnegs <- LEFmetric_data %>% filter(CT == 0)

## Filter by df same in other df with well postiion and 100 cycle threshold 
LEFnmetric_data <- as.data.frame(LEFnmetric_data)

OPMNeg_range <- LEFmetric_data %>% 
  filter(ID == "OPM Negative with Amplification")
range(OPMNeg_range$MT)



