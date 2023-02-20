#######################
# HOBO LOGGERS ICEBATH CALIBRATION 
#######################
# Holgerson Lab; Katie Gannon February 2023

# 0. Set Up R Environment
# ______________________________________________________________________________________________________________________________________________________
# 0. Set up R Environment 
library(dplyr)
library(sf)
library(raster)
library(ape) 
suppressMessages(library(mosaic))
suppressMessages(library(readxl))
library(tidyverse)
library(devtools)
library(lme4)
library(car)
library(effects)
library(mosaic)
library(writexl)
library(purrr)
library(ggplot2)
setwd("~/HolgersonLab_Helpful_Code")

# 2. Read in and format the temperature data from HOBO loggers

# List of all of the hobo files in folder rather than pulling them individually
# Desktop 
setwd("~/HolgersonLab_Helpful_Code/HOBO_Data/021423_IceBathCalibrationCheck_KG")
hobo_file_names <- list.files(pattern="*.xlsx") #Get a list of all of the .xlsx files in the working directory 
list_of_hobo_tbls <- lapply(hobo_file_names, read_xlsx, skip = 1)   #Read all of the files on that list into the R environment
list_of_hobo_dfs <- lapply(list_of_hobo_tbls, as.data.frame)
setwd("~/HolgersonLab_Helpful_Code")


hobo_dat <- list_of_hobo_dfs[[4]]

#Make a function to change column names of each HOBO data file and add serial number as a column  
clean_hobo_FUNC <- function(hobo_dat){
  names(hobo_dat)[names(hobo_dat) == "Date Time, GMT-05:00"] <- "Date_Time"  #the column name says GMT but I checked the time and it actually on EST 
  sn_full <- names(hobo_dat)[3]  #This takes the column name of the third column (which contains the serial number of the logger) and saves it as value "sn_full"
  serial_number <- substring(sn_full, 20, 27)  # This selects only the 20th to 27th character which corresponds to the serial number 
  names(hobo_dat)[3] <- "Temp_C"
  hobo_dat$Serial_Number <- serial_number 
  names(hobo_dat)
  hobo_dat <- as.data.frame(subset(hobo_dat, select = c("Serial_Number", "Date_Time", "Temp_C")))
}

#Apply function and make column names consistant 
    output_clean_hobo_fun <- lapply(list_of_hobo_dfs, clean_hobo_FUNC)
    head(output_clean_hobo_fun[[6]])
    lapply(output_clean_hobo_fun, nrow)
    
# Bind all dataframes from all loggers into one long df  
    hobo_comp <- bind_rows(output_clean_hobo_fun)
    
# Remove rows with NAs 
   hobo_comp <- hobo_comp[!is.na(hobo_comp$Temp_C), ]
    
   
# Plot to check 
    head(hobo_comp)
    hobo_icebath_check_plot  <- hobo_comp %>%
      ggplot(aes(x=Date_Time, y = Temp_C)) +
      geom_line(aes(y = Temp_C, color = Serial_Number)) + 
      geom_point(aes(y = Temp_C, color = Serial_Number)) + 
      theme_bw() +
      ylab("Temperature (C)") + xlab("Time") + ggtitle("HOBO Ice Bath Calibration Check")
    hobo_icebath_check_plot 
    
# Trim to only the time window you are interested in 
    
# Take Average temp and standard deviation of temps for each serial number 
   
    # Using Base R  
    aggregate(hobo_comp$Temp_C, list(hobo_comp$Serial_Number), FUN=mean)
    
    # Usign Dplyr 
    avg_temps_hobo <- hobo_comp %>%
      group_by(Serial_Number) %>%
      summarise_at(vars(Temp_C), list(avg_temp = mean, sd_temp = sd))
    
# Save the output as an excel document 
    #   write_xlsx(avg_temps_hobo, "OutputFiles/hobo_icebathcalib_022123.xlsx")
    
    
    

    