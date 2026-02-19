#### for analyzing met and sapflow for BIO3013F project
### Adam, 8 April


rm(list=ls())

library(tidyverse)
library(ggpubr)

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

# subtract mean nighly value from mean HR value to zero
sf_cont_clean_zero_dynamic <- left_join(sf_cont_clean, cont_zero_dynamic,)  %>%
  mutate(Mean_Heat_Ratio_zero_dynamic = Mean_Heat_Ratio - night_ave)

sf_treat_clean_zero_dynamic <- left_join(sf_treat_clean, treat_zero_dynamic,)  %>%
  mutate(Mean_Heat_Ratio_zero_dynamic = Mean_Heat_Ratio - night_ave)

# summarize into daliy sums
data_combined_sf_cont_clean_zero_dynamic <- sf_cont_clean_zero_dynamic %>%
  filter(Hour >=7 & Hour <= 17) %>%
  group_by(Date) %>%
  summarise(total_HR_cont = sum(Mean_Heat_Ratio_zero_dynamic, na.rm = TRUE)) %>%
  ungroup()

data_combined_sf_treat_clean_zero_dynamic <- sf_treat_clean_zero_dynamic %>%
  filter(Hour >=7 & Hour <= 17) %>%
  group_by(Date) %>%
  summarise(total_HR_treat = sum(Mean_Heat_Ratio_zero_dynamic, na.rm = TRUE)) %>%
  ungroup()


#remove unneeded variables
rm(sf_treat_clean, sf_cont_clean,cont_zero_dynamic, treat_zero_dynamic, sf_cont_clean_zero_dynamic, sf_treat_clean_zero_dynamic)


###############################
# add in leaf temperature data
tc_raw <- read.csv(paste0(mainDir,"/data/Thermocouple DKR data combined.csv"))

tc_data <- tc_raw %>%
  mutate(Timestamp = ymd_hm(TIMESTAMP)) %>%
  mutate(Date = as.Date(Timestamp)) %>%
  mutate(tc_cont = as.numeric(Plot_6__Plant)) %>%
  mutate(tc_treat = as.numeric(Plot_2__Plant)) %>%
  group_by(Date) %>%
  summarize(max_tc_cont = max(tc_cont), max_tc_treat = max(tc_treat))

rm(tc_raw)

###################################################
# combine sap data and met data and leaf temp into one dataframe

#join sapflow data
data_combined_daily <- full_join(data_combined_sf_cont_clean_zero_dynamic,data_combined_sf_treat_clean_zero_dynamic)

# add the met data
data_combined_daily <- full_join(data_combined_daily, data_combined_met)

# add the lef temp data into final dataframe
data_combined_daily <- full_join(data_combined_daily, tc_data)


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
             shape = 21, size = 3, alpha = 0.75) +
  geom_point(data = data_combined_daily, aes(x = Date, y = max_tc_treat, fill = "Treatment"), 
             shape = 21, size = 3, alpha = 0.75) +
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






