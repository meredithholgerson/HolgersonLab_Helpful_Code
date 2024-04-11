#________________________________________________________
#####  COMPILE TEMPERATURE DATA ####
#________________________________________________________
# Calculate the average temperature in the chamber for each sampling Run 
# Calculate the average ambient temperature at the site for each site day 

###### 0. Set Up R Environment ####

    # Working Directory 
    setwd("~/Atkinson_Aerator") # Mac 
    
    # Packages 
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

# Load Data 
    
   # Import dataframe with time windows
    chamber_meas <- read_xlsx("LGR_Field_Data_11April2024.xlsx") #updated chamber meas with start times to match LGR files and not loose data 
    chamber_meas <- as.data.frame(chamber_meas) 
    head(chamber_meas)
    str(chamber_meas)
    
    hobo_windows <- read_xlsx("Atkinson_Aerator_HOBO_Windows_11April2024.xlsx")
    
  # Import HOBO Data and format if all in one xlsx file 
    hobo_dat <- read_xlsx("HOBO_M40.xlsx")
    names(hobo_dat) <- c("Date_Time", "Temp_C")
    
  # Import HOBO Data and format if in separate xlsx files: 
      # List of all of the hobo files in folder rather than pulling them individually
      
      #Reset the working directory to just the folder you are pulling from
      # setwd("~/OneDrive/Holgerson_Lab/DECWetlandFlux/2023_Flux_Calculations/Raw_Data/HOBO_Data/Chamber_Temp") # Mac 
      setwd("~/Wetland_GHG_Survey/DECWetlandFlux-GitHubCopied/2023_Flux_Calculations/Raw_Data/HOBO_Data/Chamber_Temp") # desktop 
      
      cham_hobo_file_names <- list.files(pattern="*.xlsx") #Get a list of all of the .xlsx files in the working directory 
      list_of_cham_hobo_tbls <- lapply(cham_hobo_file_names, read_xlsx, skip = 1)   #Read all of the files on that list into the R environment
      list_of_cham_hobo_dfs <- lapply( list_of_cham_hobo_tbls, as.data.frame)  #Change all of the files in the list into dataframes
      
      # Reset the working directory back to the main folder 
      setwd("~/Wetland_GHG_Survey/DECWetlandFlux-GitHubCopied/2023_Flux_Calculations") # Desktop 
      # setwd("~/OneDrive/Holgerson_Lab/DECWetlandFlux/2023_Flux_Calculations") # Mac 

        # Write a function to formatt the HOBO data dfs 
        clean_hobo_FUNC <- function(hobo_dat){
          names(hobo_dat)[names(hobo_dat) == "Date Time, GMT-04:00"] <- "Date_Time"  #the column name says GMT but I checked the time and it actually on EST 
          names(hobo_dat)[names(hobo_dat) == "Temp, °C (LGR S/N: 21398137, SEN S/N: 21398137)"] <- "Temp_C"
          names(hobo_dat)[names(hobo_dat) == "Temp, °C (LGR S/N: 21398139, SEN S/N: 21398139)"] <- "Temp_C"
          names(hobo_dat)[names(hobo_dat) == "Date Time, GMT-05:00"] <- "Date_Time"  #the column name says GMT but I checked the time and it actually on EST 
          names(hobo_dat)[names(hobo_dat) == "#"] <- "Number"
          names(hobo_dat)
          hobo_dat <- as.data.frame(subset(hobo_dat, select = c(Date_Time, Temp_C)))
        }
        
        # Apply the function to clean the HOBO data dfs 
        hobo_cham_lst <- lapply(list_of_cham_hobo_dfs, clean_hobo_FUNC)
        
        #Check that it worked and is formatted correctly 
        lapply(hobo_cham_lst, head)
        
        # bind all of the dataframes together into one long df of HOBO data 
        hobo_cham <- bind_rows(hobo_cham_lst)
        
        # Remove rows with NA 
        hobo_cham <- as.data.frame(hobo_cham[!is.na(hobo_cham$Temp_C) , ])
        head(hobo_cham)
        str(hobo_cham)




#_____________________________________________________________________________________
# PART 1: TEMP IN CHAMBER
        
# FOrmat Chamber Data 
    #Remove Rows for runs with no saved TXT file 
    chamber_meas <- chamber_meas[chamber_meas$Txt_File == "Good", ]  
    
    # Add Run ID 
    chamber_meas$ID <- ifelse(chamber_meas$Pond == "air", paste("Air_", chamber_meas$Date, sep = ""), 
                              paste("Pond", chamber_meas$Pond, "_", chamber_meas$Date, "_R", chamber_meas$Rep, sep =""))
    
    #Format Times as date-times 
    chamber_meas$Date <- as.character(chamber_meas$Date)
    chamber_meas$Launch_Time <- as.character(chamber_meas$Launch_Time)
    chamber_meas$Launch_Time <- paste(chamber_meas$Date, chamber_meas$Launch_Time, sep = " ")
    chamber_meas$Launch_Time <- as.POSIXct(chamber_meas$Launch_Time, tz = "UTC", format = "%Y-%m-%d %H:%M")
    
    chamber_meas$Placement_Time <- as.character(chamber_meas$Placement_Time)
    chamber_meas$Placement_Time <- paste(chamber_meas$Date, chamber_meas$Placement_Time, sep = " ")
    chamber_meas$Placement_Time <- as.POSIXct(chamber_meas$Placement_Time, tz = "UTC", format = "%Y-%m-%d %H:%M")
    
    chamber_meas$End_Time <- as.character(chamber_meas$End_Time)
    chamber_meas$End_Time <- paste(chamber_meas$Date, chamber_meas$End_Time, sep = " ")
    chamber_meas$End_Time <- as.POSIXct(chamber_meas$End_Time, tz = "UTC", format = "%Y-%m-%d %H:%M")
    
    #Subset to only the columns that you need
    chamber_meas <- subset(chamber_meas, select = c("ID", "Date", "Analyzer_Type", "Chamber_Type", "Pond", 
                                                    "Rep", "Launch_Time", "Placement_Time", "End_Time"))
        


# Write a function 
    
    # Split up the Chamber measurement data into a list to make it easier to apply the function across it 
    # chamber_meas_list <- split(chamber_meas,seq(nrow(chamber_meas))) 
    
    # Format HOBO Windows data 
        hobo_windows <- as.data.frame(hobo_windows)
        head(hobo_windows)
        names(hobo_windows)[names(hobo_windows) == "Start_Time_Temp"] <- "Start_Time"
        names(hobo_windows)[names(hobo_windows) == "End_Time_Temp"] <- "End_Time"
        hobo_windows$Start_Time <- as.POSIXct(hobo_windows$Start_Time, tz = "UTC", format = "%Y-%m-%d %H:%M")
        hobo_windows$End_Time <- as.POSIXct(hobo_windows$End_Time, tz = "UTC", format = "%Y-%m-%d %H:%M")
        str(hobo_windows)
   
    # Split up the Chamber measurement data into a list to make it easier to apply the function across it 
    hobo_windows_list <- split(hobo_windows,seq(nrow(hobo_windows)))
    
    # data for writing function 
    windows_df <- hobo_windows_list[[7]]
    str(windows_df)
    head(windows_df)
    temp_df <- as.data.frame(hobo_dat)
    
    Avg_Temp_FUNC <- function(windows_df, hobo_dat){
      start_time <- windows_df[1, "Start_Time"]
      end_time <- windows_df[1, "End_Time"]
      run_window <- hobo_dat[hobo_dat$Date_Time >= start_time & hobo_dat$Date_Time <= end_time , ]
      run_window <- run_window[!is.na(run_window$Temp_C), ]
      temps_in_run_window <- run_window$Temp_C
      avg_temp <- mean(temps_in_run_window)
      Date <- as.character(windows_df$Date)
      output <- c(Date, avg_temp)
    }
    
    temp_day <- lapply(hobo_windows_list, hobo_dat = hobo_dat, FUN = Avg_Temp_FUNC)
    temp_day <- as.data.frame(temp_day)
    temp_day <- t(temp_day)
    temp_day <- as.data.frame(temp_day)
    names(temp_day) <- c("Date", "Temp_C")
    rownames(temp_day) <- seq(1:nrow(temp_day))
    
    write_xlsx(temp_day, "Output_Files/Temp_Day_11April2024.xlsx")
      
    
    
    
    
    

# write_xlsx(avg_temp_df, "Output_Files/avg_temp_cham_25jan2024.xlsx")