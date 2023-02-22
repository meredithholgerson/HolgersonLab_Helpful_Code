#######################
# HOBO LOGGERS ICEBATH CALIBRATION 
#######################
# Holgerson Lab; Katie Gannon February 2023
# My genius sometimes it is terrifying 

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

# 1. Read in and format the temperature data from HOBO loggers

  # List of all of the hobo files in folder rather than pulling them individually
    setwd("~/HolgersonLab_Helpful_Code/HOBO_Data/022123_IceBathCalibrationCheck_RR")
    hobo_file_names <- list.files(pattern="*.xlsx") #Get a list of all of the .xlsx files in the working directory 
    list_of_hobo_tbls <- lapply(hobo_file_names, read_xlsx, skip = 1)   #Read all of the files on that list into the R environment
    list_of_hobo_dfs <- lapply(list_of_hobo_tbls, as.data.frame)
    setwd("~/HolgersonLab_Helpful_Code")
  
      # pull a practice dataset out if the list 
      hobo_dat <- list_of_hobo_dfs[[4]]
      
  # Load dataframe with logger serial numbers and names 
      logger_names <- read_xlsx("HOBO_Data/HOBO_temp_light_logger_names.xlsx")
    


#2. Format the HOBO data 
    
    # Write a function to change column names of each HOBO data file and add serial number as a column  
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
    
    # Check the cleaned output to make sure it looks right and is doing what you think it is
      head(output_clean_hobo_fun[[6]])  # look at the top of the 6th dataframe 
      lapply(output_clean_hobo_fun, nrow)  # Get the number of rows in each data frame 
    
#3. Bind all dataframes from all loggers into one long df  
    hobo_comp <- bind_rows(output_clean_hobo_fun)
    
  # Fix serial number for logger that has a serial number with one fewer digit than the others 
    tally(~Serial_Number, data = hobo_comp)
    hobo_comp$Serial_Number <- ifelse(hobo_comp$Serial_Number == "9995411,", "9995411", hobo_comp$Serial_Number)
    

# 4. Remove rows with NAs 
    hobo_comp <- hobo_comp[!is.na(hobo_comp$Temp_C), ]
   
# 5. Plot to check 
    head(hobo_comp)
    hobo_icebath_check_plot  <- hobo_comp %>%
      ggplot(aes(x=Date_Time, y = Temp_C)) +
      geom_line(aes(y = Temp_C, color = Serial_Number)) + 
      geom_point(aes(y = Temp_C, color = Serial_Number)) + 
      theme_bw() +
      ylab("Temperature (C)") + xlab("Time") + ggtitle("HOBO Ice Bath Calibration Check")
    hobo_icebath_check_plot 
    

# 6. Trim to only the time window you are interested in  tz = "EST", 
    str(hobo_comp)
    start_time <- as.POSIXct("2023-02-21 11:00:00", tz = "UTC", format = "%Y-%m-%d %H:%M")
    end_time <- as.POSIXct("2023-02-21 15:00:00", tz = "UTC",format = "%Y-%m-%d %H:%M")
    hobo_comp_trimmed <- hobo_comp[hobo_comp$Date_Time >= start_time & hobo_comp$Date_Time <= end_time , ]
    
    # Add logger names to trimmed data  
    str(logger_names)
    logger_names$Serial_Number <- as.character(logger_names$Serial_Number)
    hobo_comp_trimmed <- left_join(hobo_comp_trimmed, logger_names)
    head(hobo_comp_trimmed)
    
    # Plot again with trimmed data and logger names
    hobo_icebath_check_plot_trimmed  <- hobo_comp_trimmed %>%
      ggplot(aes(x=Date_Time, y = Temp_C)) +
      geom_line(aes(y = Temp_C, color = Logger_Name)) + 
      geom_point(aes(y = Temp_C, color = Logger_Name)) + 
      theme_bw() +
      ylab("Temperature (C)") + xlab("Time") + ggtitle("HOBO Ice Bath Calibration Check - 022123 RR")
    hobo_icebath_check_plot_trimmed 
    
    # Save plot of trimmed data with logger names 
    ggsave("OutputFiles/HOBO_IceBath_CalibrationCheck_022123_1802.png", hobo_icebath_check_plot_trimmed, width = 190, height = 120, units = "mm")
    
    
# 7. Take Average temp and standard deviation of temps for each serial number 
   
    # Using Base R  
    aggregate(hobo_comp_trimmed$Temp_C, list(hobo_comp_trimmed$Serial_Number), FUN=mean)
    
    # Usign Dplyr 
    avg_temps_hobo <- hobo_comp_trimmed %>%
      group_by(Serial_Number) %>%
      summarise_at(vars(Temp_C), list(avg_ice_bath_temp = mean, sd_ice_bath_temp = sd))
    
# 8. Add logger names 
    str(logger_names)
    logger_names$Serial_Number <- as.character(logger_names$Serial_Number)
    output <- inner_join(avg_temps_hobo, logger_names)
    
    head(output)
    output <- subset(output, select = c("Logger_Name", "Serial_Number", "avg_ice_bath_temp", "sd_ice_bath_temp"))
    
    mean(output$avg_ice_bath_temp)
    
# 9. Save the output as an excel document 
   #   write_xlsx(output, "OutputFiles/hobo_icebathcalib_022123_1802.xlsx")
   #   write_xlsx(hobo_comp_trimmed, "OutputFiles/hobo_icebathcalib_rawtemps_long_022123.xlsx")
    
    
    

    