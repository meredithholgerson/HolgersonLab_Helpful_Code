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

 setwd("~/HolgersonLab_Helpful_Code")  #Desktop 
# setwd("~/OneDrive/Holgerson_Lab/HolgersonLab_Helpful_Code") # Mac

# 1. Read in and format the temperature data from HOBO loggers

  # List of all of the hobo files in folder rather than pulling them individually
    setwd("~/HolgersonLab_Helpful_Code/HOBO_Data/022823_IceBathCalibrationCheck_KG_Compiled") # Desktop 
    # setwd("~/OneDrive/Holgerson_Lab/HolgersonLab_Helpful_Code/HOBO_Data/022323_IceBathCalibrationCheck_KG") # Mac
    hobo_file_names <- list.files(pattern="*.csv") #Get a list of all of the .xlsx files in the working directory 
    list_of_hobo_tbls <- lapply(hobo_file_names, read_csv, skip = 1)   #Read all of the files on that list into the R environment
    list_of_hobo_dfs <- lapply(list_of_hobo_tbls, as.data.frame)
     setwd("~/HolgersonLab_Helpful_Code") #Desktop
    # setwd("~/OneDrive/Holgerson_Lab/HolgersonLab_Helpful_Code") # Mac
  
      # pull a practice dataset out if the list 
      hobo_dat <- list_of_hobo_dfs[[4]]
      head(hobo_dat)
      
  # Load dataframe with logger serial numbers and names 
      # logger_names_M <- read_xlsx("HOBO_Data/HOBO_temp_light_logger_names.xlsx")
      # logger_names_T <- read_xlsx("HOBO_Data/HOBO_temp_only_logger_names.xlsx")
      # logger_names_B <- read_xlsx("HOBO_Data/HOBO_tempProV2_logger_names.xlsx")
      logger_names <- read_xlsx("HOBO_Data/Holgerson_Lab_HOBO_Logger_Names.xlsx")
      
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
      head(output_clean_hobo_fun[[3]])  # look at the top of the 6th dataframe 
      lapply(output_clean_hobo_fun, head)  # Get the number of rows in each data frame 
    
#3. Bind all dataframes from all loggers into one long df  
    hobo_comp <- bind_rows(output_clean_hobo_fun)
    head(hobo_comp)
    hobo_comp[1, "Date_Time"]
    
  # Fix serial number for logger that has a serial number with one fewer digit than the others 
    tally(~Serial_Number, data = hobo_comp)
    hobo_comp$Serial_Number <- ifelse(hobo_comp$Serial_Number == "9995411,", "9995411", hobo_comp$Serial_Number)
    
  # Formatt Columns 
    hobo_comp$Serial_Number <- as.factor(hobo_comp$Serial_Number)
    hobo_comp$Date_Time <- as.POSIXct(hobo_comp$Date_Time,tz = "UTC", format = "%m/%d/%Y %H:%M:%OS" )
    head(hobo_comp)
    hobo_comp$Temp_C <- as.numeric(hobo_comp$Temp_C)
    str(hobo_comp)
    
# 4. Remove rows with NAs 
    hobo_comp <- hobo_comp[!is.na(hobo_comp$Temp_C), ]
    

# 5. Trim to only the time window you are interested in  tz = "EST", 
    str(hobo_comp)
    head(hobo_comp)
    min(hobo_comp$Date_Time)
    max(hobo_comp$Date_Time)
    start_time <- as.POSIXct("0023-02-28 11:00:00", tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
    end_time <- as.POSIXct("0023-02-28 17:00:00", tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
    hobo_comp_trimmed <- hobo_comp[hobo_comp$Date_Time >= start_time & hobo_comp$Date_Time <= end_time , ]
    head(hobo_comp_trimmed)
    
    # Add logger names to trimmed data  
    str(logger_names)
    logger_names$Serial_Number <- as.character(logger_names$Serial_Number)
    hobo_comp_trimmed <- left_join(hobo_comp_trimmed, logger_names)
    head(hobo_comp_trimmed)
  
    
# 6. Plot to check with trimmed data and logger names
    hobo_icebath_check_plot_trimmed  <- hobo_comp_trimmed %>%
      ggplot(aes(x=Date_Time, y = Temp_C)) +
      geom_line(aes(y = Temp_C, color = Logger_Name)) + 
      geom_point(aes(y = Temp_C, color = Logger_Name)) + 
      theme_bw() +
      ylab("Temperature (C)") + xlab("Time") + ggtitle("HOBO Ice Bath Calibration Check - 022823 RR")
    hobo_icebath_check_plot_trimmed 
    
    # Save plot of trimmed data with logger names 
    ggsave("OutputFiles/230228_hobo_icebathcalib_Holgerson_Loggers.png", hobo_icebath_check_plot_trimmed, width = 190, height = 120, units = "mm")
    
    
# 7. Take Average temp and standard deviation of temps for each serial number 
   
    # Using Base R  
    aggregate(hobo_comp_trimmed$Temp_C, list(hobo_comp_trimmed$Serial_Number), FUN=mean)
    
    # Usign Dplyr 
    avg_temps_hobo <- hobo_comp_trimmed %>%
      group_by(Serial_Number) %>%
      summarise_at(vars(Temp_C), list(avg_ice_bath_temp = mean, sd_ice_bath_temp = sd))
    
    #You could use the same above code to get the average temp in all ponds at each depth by changing Serial Number to Depth 
    
# 8. Add logger names to the output 
    str(logger_names)
    logger_names$Serial_Number <- as.character(logger_names$Serial_Number)
    output <- inner_join(avg_temps_hobo, logger_names)
    
    head(output)
    output <- subset(output, select = c("Logger_Name", "Serial_Number", "avg_ice_bath_temp", "sd_ice_bath_temp"))
    head(output)
    mean(output$avg_ice_bath_temp)
    
# 9. Save the output as an excel document 
    
    # Change date time to a character so that it can pass into an excel file 
    hobo_comp_trimmed$Date_Time <- as.character(hobo_comp_trimmed$Date_Time)
    head(hobo_comp_trimmed)
    
       # write_xlsx(output, "OutputFiles/230228_hobo_icebathcalib_Holgerson_Loggers.xlsx")
       # write_xlsx(hobo_comp_trimmed, "OutputFiles/230228_hobo_icebathrawtemps_Holgerson_Loggers.xlsx")
    
# 10. Put together all calibration corrections from multiple days of ice bath 
    icebath_230223 <- read_xlsx("OutputFiles/230223_hobo_icebathcalib_MLoggers.xlsx")
    icebath_230228 <- read_xlsx("OutputFiles/230228_hobo_icebathcalib_Holgerson_Loggers.xlsx")
    head(icebath_230223)    
    head(icebath_230228)    
    all_icebaths <- rbind(icebath_230223, icebath_230228)
    all_icebaths <- subset(all_icebaths, select = c("Logger_Name", "Serial_Number", "avg_ice_bath_temp", "sd_ice_bath_temp"))
    head(all_icebaths)
    write_xlsx(all_icebaths, "OutputFiles/230228_Spring2023_Icebathcalibration_HolgersonLoggers.xlsx")
    