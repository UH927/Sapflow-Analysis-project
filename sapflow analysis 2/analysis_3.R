#### for analyzing met and sapflow for BIO3013F project
### Adam, 8 April


rm(list=ls())

library(tidyverse)
citation("tidyverse")
library(ggpubr)
citation("ggpubr")
citation()

##################

# find directory
mainDir <- dirname(rstudioapi::getActiveDocumentContext()$path)

#read in data
metdata_raw <- read.csv(paste0(mainDir,"/data/cleaned/metdata.csv"))
sf_cont_raw <- read.csv(paste0(mainDir,"/data/cleaned/sapflow_control.csv"))
sf_treat_raw <- read.csv(paste0(mainDir,"/data/cleaned/sapflow_treatment.csv"))

#convert to correct time
metdata <- metdata_raw %>%
  mutate(Timestamp = ymd_hms(datetime,tz = "Africa/Johannesburg"))
  
sf_cont <- sf_cont_raw %>%
  mutate(Timestamp = ymd_hms(Timestamp,tz="Africa/Johannesburg")) %>%
  mutate(Timestamp_2 = Timestamp+hours(2)) %>%
  mutate(Date = as_date(Timestamp_2)) %>%
  mutate(Hour = hour(Timestamp_2)) %>%
  mutate(Rounded_timestamp = ymd_hms(Rounded_timestamp,tz="Africa/Johannesburg")) %>%
  mutate(Rounded_timestamp_2 = Rounded_timestamp+hours(2)) 


sf_treat <- sf_treat_raw %>%
  mutate(Timestamp = ymd_hms(Timestamp,tz="Africa/Johannesburg")) %>%
  mutate(Timestamp_2 = Timestamp+hours(2)) %>%
  mutate(Date = as_date(Timestamp_2)) %>%
  mutate(Hour = hour(Timestamp_2)) %>%
  mutate(Rounded_timestamp = ymd_hms(Rounded_timestamp,tz="Africa/Johannesburg")) %>%
  mutate(Rounded_timestamp_2 = Rounded_timestamp+hours(2))

#have a look
ggplot(NULL)+
  geom_area(data=metdata, aes(x=Timestamp, y = Sol_W_m2/400), fill="yellow") +
  geom_line(data=metdata, aes(x=Timestamp, y = VPD), col="black") +
  geom_line(data=sf_cont, aes(x=Timestamp_2, y = Mean_Heat_Ratio), col="green") +
  geom_line(data=sf_treat, aes(x=Timestamp_2, y = Mean_Heat_Ratio), col="red") +
  geom_line(data=metdata, aes(x=Timestamp, y = SM_T), col="pink") +
  geom_line(data=metdata, aes(x=Timestamp, y = SM_C), col="lightblue") +
  geom_line(data=metdata, aes(x=Timestamp, y = Rain_mm_Tot), col="blue")


# trim data sets to same time span

# Find minimum and maximum timestamps across sapflow datasets
sapflow_start <- min(c(min(sf_cont$Timestamp_2, na.rm = TRUE), min(sf_treat$Timestamp_2, na.rm = TRUE)))
sapflow_end   <- max(c(max(sf_cont$Timestamp_2, na.rm = TRUE), max(sf_treat$Timestamp_2, na.rm = TRUE)))

# Trim metdata to match sapflow period
metdata <- metdata %>%
  filter(Timestamp >= sapflow_start, Timestamp <= sapflow_end)

#replot trimmed datasets to have a look
ggplot(NULL)+
  geom_area(data=metdata, aes(x=Timestamp, y = Sol_W_m2/400), fill="yellow") +
  geom_line(data=metdata, aes(x=Timestamp, y = VPD), col="black") +
  geom_line(data=sf_cont, aes(x=Timestamp_2, y = Mean_Heat_Ratio), col="green") +
  geom_line(data=sf_treat, aes(x=Timestamp_2, y = Mean_Heat_Ratio), col="red") +
  geom_line(data=metdata, aes(x=Timestamp, y = SM_T), col="pink") +
  geom_line(data=metdata, aes(x=Timestamp, y = SM_C), col="lightblue") +
  geom_line(data=metdata, aes(x=Timestamp, y = Rain_mm_Tot), col="blue")

#remove unneeded variables
rm(metdata_raw,sf_cont_raw, sf_treat_raw)


##################################
### for looking at smaller sections

datestart <- ymd("2025-03-14")
dateend <- ymd("2025-04-04")

#filter for plotting
met_plot <- metdata %>%
  filter(DateR >= datestart & DateR <= dateend)

sf_cont_plot <- sf_cont %>%
  filter(Timestamp >= datestart & Timestamp <= dateend)

sf_treat_plot <- sf_treat %>%
  filter(Timestamp >= datestart & Timestamp <= dateend)

sf_cont_plot_zero <- sf_cont_plot %>%
  filter(Hour <=3) 

sf_treat_plot_zero <- sf_treat_plot %>%
  filter(Hour <=3) 

ggplot(NULL)+
  geom_line(data=met_plot,aes(x=Timestamp,y=Rain_mm_Tot),col="blue")+
  geom_area(data=met_plot,aes(x=Timestamp,y=Sol_W_m2/500),fill="yellow") +
  geom_line(data=met_plot,aes(x=Timestamp,y=VPD),col="black")+
  geom_line(data=sf_cont_plot,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="darkblue") +
  geom_line(data=sf_treat_plot,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="red") +
  geom_point(data=sf_cont_plot_zero,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="lightblue") +
  geom_point(data=sf_treat_plot_zero,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="pink") +
  ylim(0,2)

#remove uneeded variables
rm(met_plot, sf_cont_plot, sf_treat_plot, sf_cont_plot_zero, sf_treat_plot_zero)




#####################################
### removing outliers


#######################################
# Function to remove outliers 
remove_sapflow_outliers <- function(df) {
  df <- df %>%
    arrange(Timestamp) %>%
    mutate(
      val = Mean_Heat_Ratio,
      lag1 = lag(val, 1),
      lag2 = lag(val, 2),
      lag3 = lag(val, 3),
      lag4 = lag(val, 4),
      lag5 = lag(val, 5)
    ) %>%
    rowwise() %>%
    mutate(
      rolling_mean = mean(c(lag1, lag2, lag3, lag4, lag5), na.rm = TRUE),
      rolling_sd = sd(c(lag1, lag2, lag3, lag4, lag5), na.rm = TRUE),
      is_outlier = abs(val - rolling_mean) > (2 * rolling_sd)
    ) %>%
    ungroup()
  
  return(df)
}

# Apply to both sapflow datasets
sf_cont_marked <- remove_sapflow_outliers(sf_cont)
sf_treat_marked <- remove_sapflow_outliers(sf_treat)

# Filter out outliers for clean datasets
sf_cont_clean <- sf_cont_marked %>% filter(!is_outlier | is.na(is_outlier))
sf_treat_clean <- sf_treat_marked %>% filter(!is_outlier | is.na(is_outlier))

#plots to show outliers
ggplot(sf_cont_marked, aes(x = Timestamp_2, y = Mean_Heat_Ratio, color = is_outlier)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  labs(title = "Control Sap Flow with Outliers Marked", y = "Mean Heat Ratio", color = "Outlier")

ggplot(sf_treat_marked, aes(x = Timestamp_2, y = Mean_Heat_Ratio, color = is_outlier)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  labs(title = "Control Sap Flow with Outliers Marked", y = "Mean Heat Ratio", color = "Outlier")

ggplot(NULL)+
  geom_point(data=sf_cont, aes(x = Timestamp_2, y = Mean_Heat_Ratio),col="red")+
  geom_point(data=sf_cont_clean, aes(x = Timestamp_2, y = Mean_Heat_Ratio)) 
  
ggplot(NULL)+
  geom_point(data=sf_treat, aes(x = Timestamp_2, y = Mean_Heat_Ratio),col="red")+
  geom_point(data=sf_treat_clean, aes(x = Timestamp_2, y = Mean_Heat_Ratio)) 


rm(sf_cont_marked,sf_treat_marked, sf_cont, sf_treat)


##################################
### for looking at smaller sections of cleaned data


##################
datestart <- ymd("2025-01-25")
dateend <- ymd("2025-02-01")

#filter for plotting
met_plot <- metdata %>%
  filter(DateR >= datestart & DateR <= dateend)

sf_cont_plot <- sf_cont_clean %>%
  filter(Timestamp >= datestart & Timestamp_2 <= dateend)

sf_treat_plot <- sf_treat_clean %>%
  filter(Timestamp >= datestart & Timestamp_2 <= dateend)

sf_cont_plot_zero <- sf_cont_plot %>%
  filter(Hour <=3) 
         
sf_treat_plot_zero <- sf_treat_plot %>%
  filter(Hour <=3) 

ggplot(NULL)+
  geom_line(data=met_plot,aes(x=Timestamp,y=Rain_mm_Tot),col="blue")+
  geom_area(data=met_plot,aes(x=Timestamp,y=Sol_W_m2/500),fill="yellow") +
  geom_line(data=met_plot,aes(x=Timestamp,y=VPD),col="black")+
  geom_line(data=sf_cont_plot,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="darkblue") +
  geom_line(data=sf_treat_plot,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="red") +
  geom_point(data=sf_cont_plot_zero,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="lightblue") +
  geom_point(data=sf_treat_plot_zero,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="pink") +
  ylim(0,2)


#remove uneeded variables
rm(met_plot, sf_cont_plot, sf_treat_plot, sf_cont_plot_zero, sf_treat_plot_zero)


#############################################
#### creating a multi-panel figure of continuous data

sapflow <- ggplot(NULL)+
  geom_line(data=sf_cont_clean,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="darkblue") +
  geom_line(data=sf_treat_clean,aes(x=Timestamp_2,y=Mean_Heat_Ratio),col="red") +
  ylim(0.5,2) +
  labs(x= "Date", y = "Heat Ratio") +
  theme_minimal() +
  xlab(NULL)

soilmoisture <- ggplot(NULL)+
  geom_line(data=metdata, aes(x=Timestamp, y = SM_T), col="pink") +
  geom_line(data=metdata, aes(x=Timestamp, y = SM_C), col="lightblue") +
  #ylim(0.5,2) +
  labs(x= "Date", y = "Soil Water Potential (MPa)") +
  theme_minimal() +
  xlab(NULL)

VPD <- ggplot(NULL)+
  geom_line(data=metdata, aes(x=Timestamp, y = VPD)) +
  labs(x= "Date", y = "VPD (kPa)") +
  theme_minimal() +
  xlab(NULL)

Temp <- ggplot(NULL)+
  geom_line(data=metdata, aes(x=Timestamp, y = AirTC_avg)) +
  labs(x= "Date", y = "Air Temp (C)") +
  theme_minimal() 
  
combined_plot <- ggarrange(sapflow, soilmoisture,VPD, Temp, ncol=1, align="v", legend = "right")
combined_plot

ggsave(paste0(mainDir,"/results/plot1.jpg"),combined_plot,width=15, height=20, dpi=300, units="cm")


#remove unneeded plots
rm(sapflow, soilmoisture,VPD, Temp)





##############################################################
##### creating daily values and combining into one dataframe

# create met daily data 
data_combined_met <- metdata %>%
  mutate(Date = as_date(Timestamp)) %>%
  group_by(Date) %>%
  summarise(
    mean_VPD = mean(VPD, na.rm = TRUE),
    max_AirTemp = max(AirTC_avg, na.rm = TRUE),
    mean_SM_T = mean(SM_T, na.rm = TRUE),
    mean_SM_C = mean(SM_C, na.rm = TRUE),
    total_Rain = sum(Rain_mm_Tot, na.rm = TRUE)
  ) %>%
  slice(1:(n() - 1)) %>% # remove last row due to incomplete data
  ungroup()

# check with plot
ggplot(NULL)+
  geom_col(data= data_combined_met, aes(x=Date, y = mean_VPD))
  
rm(metdata)


############################
# zeroing values for sapflow

# finding the mean value every night
cont_zero_dynamic <- sf_cont_clean %>%
  filter(Hour <= 3) %>%
  group_by(Date) %>%
  summarize(night_ave = mean(Mean_Heat_Ratio))

treat_zero_dynamic <- sf_treat_clean %>%
  filter(Hour <= 3) %>%
  group_by(Date) %>%
  summarize(night_ave = mean(Mean_Heat_Ratio))

# subtract mean nightly value from mean HR value to zero
sf_cont_clean_zero_dynamic <- left_join(sf_cont_clean, cont_zero_dynamic,)  %>%
  mutate(Mean_Heat_Ratio_zero_dynamic = Mean_Heat_Ratio - night_ave)

sf_treat_clean_zero_dynamic <- left_join(sf_treat_clean, treat_zero_dynamic,)  %>%
  mutate(Mean_Heat_Ratio_zero_dynamic = Mean_Heat_Ratio - night_ave)

# summarize into daliy sums
data_combined_sf_cont_clean_zero_dynamic <- sf_cont_clean_zero_dynamic %>%
  filter(Hour >=7 & Hour <= 17) %>%
  group_by(Date) %>%
  summarise(total_HR_cont = sum(Mean_Heat_Ratio_zero_dynamic, na.rm = TRUE)) %>%
  slice(1:(n() - 1)) %>% # remove last row due to incomplete data
  ungroup()

data_combined_sf_treat_clean_zero_dynamic <- sf_treat_clean_zero_dynamic %>%
  filter(Hour >=7 & Hour <= 17) %>%
  group_by(Date) %>%
  summarise(total_HR_treat = sum(Mean_Heat_Ratio_zero_dynamic, na.rm = TRUE)) %>%
  slice(1:(n() - 1)) %>% # remove last row due to incomplete data
  ungroup()


#remove unneeded variables
rm(sf_treat_clean, sf_cont_clean,cont_zero_dynamic, treat_zero_dynamic, sf_cont_clean_zero_dynamic, sf_treat_clean_zero_dynamic)


###############################
# add in leaf temperature data
tc_raw <- read.csv(paste0(mainDir,"/data/Thermocouple DKR data combined.csv"))

tc_data <- tc_raw %>%
  mutate(Timestamp = mdy_hm(TIMESTAMP)) %>%
  mutate(Date = as.Date(Timestamp)) %>%
  mutate(tc_cont = as.numeric(Plot_6__Plant)) %>%
  mutate(tc_treat = as.numeric(Plot_2__Plant)) %>%
  group_by(Date) %>%
  summarize(max_tc_cont = max(tc_cont), max_tc_treat = max(tc_treat)) %>%
  slice(1:(n() - 1)) # remove last row due to incomplete data

rm(tc_raw)

###################################################
# combine sap data and met data and leaf temp into one dataframe

#join sapflow data
data_combined_daily <- full_join(data_combined_met, data_combined_sf_cont_clean_zero_dynamic)

# add the met data
data_combined_daily <- full_join(data_combined_daily, data_combined_sf_treat_clean_zero_dynamic)

# add the lef temp data into final dataframe
data_combined_daily <- left_join(data_combined_daily, tc_data)


#remove unneeded variables
rm(data_combined_sf_cont_clean_zero_dynamic,data_combined_sf_treat_clean_zero_dynamic,data_combined_met, tc_data)




##########################
#create multi-panel plot


Sapflow <- ggplot(data_combined_daily) +
  geom_col(aes(x = Date, y = total_HR_cont, fill = "Control"), color = "darkblue") +
  geom_col(aes(x = Date, y = total_HR_treat, fill = "Treatment"), color = "red", alpha = 0.25) +
  scale_fill_manual(name = "", values = c("Control" = "darkblue", "Treatment" = "red")) +
  labs(x = "Date", y = "Sapflow (Total daily zeroed HR)") +
  theme_minimal() 

VPD <- ggplot(NULL)+
  geom_point(data= data_combined_daily, aes(x=Date, y = mean_VPD))+
  geom_line(data= data_combined_daily, aes(x=Date, y = mean_VPD)) +
  labs(x= "Date", y = "Mean daily VPD (kPa)") +
  theme_minimal() +
    xlab(NULL)
  
Rain <- ggplot(NULL)+
  geom_col(data= data_combined_daily, aes(x=Date, y = total_Rain), col="blue") +
  geom_col(data= data_combined_daily, aes(x=Date, y = total_HR_cont), alpha=0) +
  labs(x= "Date", y = "Total daily rain (mm)") +
  theme_minimal() +
  xlab(NULL)

Soilmoisture <- ggplot(data_combined_daily) +
  geom_line(aes(x = Date, y = mean_SM_T, color = "Treatment")) +
  geom_line(aes(x = Date, y = mean_SM_C, color = "Control")) +
  scale_color_manual(name = "", values = c("Treatment" = "red", "Control" = "darkblue")) +
  labs(x = "Date", y = "Soil Water Potential (MPa)") +
  theme_minimal() +
  xlab(NULL)

MaxTemps <- ggplot(NULL) +
  geom_point(data = data_combined_daily, aes(x = Date, y = max_AirTemp), color = "black") +
  geom_line(data = data_combined_daily, aes(x = Date, y = max_AirTemp), color = "black") +
  geom_point(data = data_combined_daily,aes(x = Date, y = max_tc_cont, fill = "Control"), 
             shape = 21, size = 3, alpha = 0.5) +
  geom_point(data = data_combined_daily, aes(x = Date, y = max_tc_treat, fill = "Treatment"), 
             shape = 21, size = 3, alpha = 0.5) +
  scale_fill_manual(name = "Leaf temp", values = c("Control" = "blue", "Treatment" = "red")) +
  labs(x = "Date", y = "Max Air Temp (°C)") +
  ylim(0, 40) +
  theme_minimal() +
  xlab(NULL)

combined_plot <- ggarrange(MaxTemps, VPD, Rain, Soilmoisture,Sapflow, ncol=1, align="v", legend = "right")
combined_plot

ggsave(paste0(mainDir,"/results/plot2.jpg"),combined_plot,width=25, height=30, dpi=300, units="cm")


#remove unneeded plots
rm(combined_plot,MaxTemps, VPD, Sapflow,Rain, Soilmoisture)

##t test HR 4 sections

datestart1 <- ymd("2024-10-10")
dateend1 <- ymd("2024-10-26")
Sect1 <- data_combined_daily %>%
  filter(Date >= datestart1 & Date <= dateend1)
t.test(Sect1$total_HR_cont, Sect1$total_HR_treat, paired = TRUE)
#mean differences are not significantly different. Occurs before major rainfall

datestart2 <- ymd("2024-10-27")
dateend2 <- ymd("2024-12-28")
Sect2 <- data_combined_daily %>%
  filter(Date >= datestart2 & Date <= dateend2)
t.test(Sect2$total_HR_cont, Sect2$total_HR_treat, paired = TRUE)
#means are very different, significant

datestart3 <- ymd("2025-02-21")
dateend3 <- ymd("2025-03-11")
Sect3 <- data_combined_daily %>%
  filter(Date >= datestart3 & Date <= dateend3)
t.test(Sect3$total_HR_cont, Sect3$total_HR_treat, paired = TRUE)
#very slight difference but still significant

datestart4 <- ymd("2025-03-13")
dateend4 <- ymd("2025-03-31")
Sect4 <- data_combined_daily %>%
  filter(Date >= datestart4 & Date <= dateend4)
t.test(Sect4$total_HR_cont, Sect4$total_HR_treat, paired = TRUE)
#rather different, significant

#leaf temp t test

datestart5 <- ymd("2025-02-21")
dateend5 <- ymd("2025-03-11")
Sect5 <- data_combined_daily %>%
  filter(Date >= datestart5 & Date <= dateend5)
t.test(Sect5$max_tc_cont, Sect5$max_tc_treat, paired = TRUE)
#slightly differnt, treatment leaves are hotter

datestart6 <- ymd("2025-03-13")
dateend6 <- ymd("2025-03-31")
Sect6 <- data_combined_daily %>%
  filter(Date >= datestart6 & Date <= dateend6)
t.test(Sect6$max_tc_cont, Sect6$max_tc_treat, paired = TRUE)



############

##Table

library(dplyr)
library(lubridate)
install.packages("flextable")
library(flextable)

citation("dplyr")
citation("lubridate")
citation("flextable")

Sect1 <- data_combined_daily %>% filter(Date >= ymd("2024-10-10") & Date <= ymd("2024-10-26"))
Sect2 <- data_combined_daily %>% filter(Date >= ymd("2024-10-27") & Date <= ymd("2024-12-28"))
Sect3 <- data_combined_daily %>% filter(Date >= ymd("2025-02-21") & Date <= ymd("2025-03-11"))
Sect4 <- data_combined_daily %>% filter(Date >= ymd("2025-03-13") & Date <= ymd("2025-04-02"))
Sect5 <- Sect3
Sect6 <- Sect4

# T-tests
tt1 <- t.test(Sect1$total_HR_cont, Sect1$total_HR_treat, paired = TRUE)
tt2 <- t.test(Sect2$total_HR_cont, Sect2$total_HR_treat, paired = TRUE)
tt3 <- t.test(Sect3$total_HR_cont, Sect3$total_HR_treat, paired = TRUE)
tt4 <- t.test(Sect4$total_HR_cont, Sect4$total_HR_treat, paired = TRUE)
tt5 <- t.test(Sect5$max_tc_cont, Sect5$max_tc_treat, paired = TRUE)
tt6 <- t.test(Sect6$max_tc_cont, Sect6$max_tc_treat, paired = TRUE)

# Mean values
mean_control <- c(
  mean(Sect1$total_HR_cont, na.rm = TRUE),
  mean(Sect2$total_HR_cont, na.rm = TRUE),
  mean(Sect3$total_HR_cont, na.rm = TRUE),
  mean(Sect4$total_HR_cont, na.rm = TRUE),
  mean(Sect5$max_tc_cont, na.rm = TRUE),
  mean(Sect6$max_tc_cont, na.rm = TRUE)
)

mean_treat <- c(
  mean(Sect1$total_HR_treat, na.rm = TRUE),
  mean(Sect2$total_HR_treat, na.rm = TRUE),
  mean(Sect3$total_HR_treat, na.rm = TRUE),
  mean(Sect4$total_HR_treat, na.rm = TRUE),
  mean(Sect5$max_tc_treat, na.rm = TRUE),
  mean(Sect6$max_tc_treat, na.rm = TRUE)
)

df_values <- c(tt1$parameter, tt2$parameter, tt3$parameter, tt4$parameter, tt5$parameter, tt6$parameter)

# Build summary table with df
results_table <- data.frame(
  Test_ID = paste0("T", 1:6),
  Period = c("HR Before Rainfall", "HR After Rainfall", "HR Before Rainfall 2", "HR After Rainfall 2", "Leaf Temp Early", "Leaf Temp Late"),
  Start_Date = as.Date(c("2024-10-10", "2024-10-27", "2025-02-21", "2025-03-13", "2025-02-21", "2025-03-13")),
  End_Date = as.Date(c("2024-10-26", "2024-12-28", "2025-03-11", "2025-04-02", "2025-03-11", "2025-04-02")),
  Mean_Control = mean_control,
  Mean_Treatment = mean_treat,
  Mean_Difference = mean_control - mean_treat,
  T_Statistic = c(tt1$statistic, tt2$statistic, tt3$statistic, tt4$statistic, tt5$statistic, tt6$statistic),
  DF = df_values,
  P_Value = c(tt1$p.value, tt2$p.value, tt3$p.value, tt4$p.value, tt5$p.value, tt6$p.value),
  Significant = c(tt1$p.value < 0.05, tt2$p.value < 0.05, tt3$p.value < 0.05, tt4$p.value < 0.05, tt5$p.value < 0.05, tt6$p.value < 0.05)
)

# Round numeric values
results_table <- results_table %>%
  mutate(
    Mean_Control = round(Mean_Control, 4),
    Mean_Treatment = round(Mean_Treatment, 4),
    Mean_Difference = round(Mean_Difference, 4),
    T_Statistic = round(T_Statistic, 4),
    DF = round(DF, 0),
    P_Value = round(P_Value, 4)
  )

# Format flextable with bigger size
ft <- flextable(results_table) %>%
  set_caption("Summary of Paired T-Tests with Mean Differences and Degrees of Freedom") %>%
  autofit() %>%
  theme_booktabs() %>%
  fontsize(size = 13, part = "all") %>%
  padding(padding = 10, part = "all") %>%       # Increased padding
  height_all(height = 1.2) %>%                  # Make all rows taller
  set_table_properties(width = .9, layout = "autofit")

# Show table
ft
ggsave(paste0(mainDir,"/results/plot3.jpg"),ft,width=25, height=30, dpi=300, units="cm")