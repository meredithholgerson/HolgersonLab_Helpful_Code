# ////////////////////////////////////////
#### FORMATTING, TRIMMING, & QUALITY CHECKING LGR DATA ####
# ////////////////////////////////////////


##### 0) Set Up R Environment  #### 

    # Set working directory 
    setwd("~/HolgersonLab_Helpful_Code") # Desktop 
    # setwd("~/OneDrive/Holgerson_Lab/DECWetlandFlux/2023_Flux_Calculations")
    
    # Load Packages 
    suppressMessages(library(dplyr))
    suppressMessages(library(sf))
    suppressMessages(library(raster))
    suppressMessages(library(ape))
    suppressMessages(library(mosaic))
    suppressMessages(library(readxl))
    suppressMessages(library(tidyverse))
    suppressMessages(library(devtools))
    suppressMessages(library(lme4))
    suppressMessages(library(car))
    suppressMessages(library(effects))
    suppressMessages(library(mosaic))
    suppressMessages(library(writexl))
    suppressMessages(library(chron))
    suppressMessages(library(lubridate))
    suppressMessages(library(readxl))

# Load Data 

      # Chamber Measurements 
      chamber_meas <- read_xlsx("~/Lab_Manager/Data_Offload/Atkinson_Aerator/LGR_Field_Data_11April2024.xlsx") #updated chamber meas with start times to match LGR files and not loose data 
      chamber_meas <- as.data.frame(chamber_meas)   
      head(chamber_meas)   
      str(chamber_meas)
      
        
      # Annotation Notes 
      

# LGR Data 
    # setwd("~/OneDrive/Holgerson_Lab/Data_Offloading/LGR_Data/2023_Wetlands_LGR") #Mac # Set working directory to the folder where you have all of the LGR txt files 
      setwd("~/Lab_Manager/Data_Offload/Atkinson_Aerator/LGR_Aerator_Control")# Desktop 
      raw_file_names <- list.files(pattern="*.txt") #Get a list of all of the .txt files in the working directory 
      raw_LGR_lst <- lapply(raw_file_names, read.table, skip = 1, header = T, sep = ",", fill = TRUE) #Read all of the files on that list into the R environment (comes in as a list of dataframes)  
    # setwd("~/OneDrive/Holgerson_Lab/DECWetlandFlux/2023_Flux_Calculations") # mac
      setwd("~/HolgersonLab_Helpful_Code")  # Desktop 


# ________________________________________________________________________________________________
##### 1) Format Data   #### 
      
      # 1.1 Format Chamber Measurement Data  ________________
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
      
      
      # 1.2 Format column names of Raw LGR Files to format in R ________________
      Format_raw_LGR_FUNC <- function(dirty_raw_data){
        names(dirty_raw_data)[names(dirty_raw_data) == "Time"] <- "Date_Time"    #Fix/Format column names
        names(dirty_raw_data)[names(dirty_raw_data) == "X.CH4._ppm"] <- "CH4_ppm"
        names(dirty_raw_data)[names(dirty_raw_data) == "X.CO2._ppm"] <- "CO2_ppm"
        names(dirty_raw_data)[names(dirty_raw_data) == "X.CH4.d_ppm"] <- "CH4_d_ppm"
        names(dirty_raw_data)[names(dirty_raw_data) == "X.CO2.d_ppm"] <- "CO2_d_ppm"
        names(dirty_raw_data)[names(dirty_raw_data) == "GasT_C"] <- "Gas_Temp_C"
        names(dirty_raw_data)[names(dirty_raw_data) == "AmbT_C"] <- "Amb_Temp_C"
        
        dirty_raw_data <- dirty_raw_data[!is.na(dirty_raw_data$CH4_ppm), ]  #This gets rid of the annoying comment on the bottom 
        dirty_raw_data <- as.data.frame(subset(dirty_raw_data, select = c("Date_Time", "CH4_ppm", "CO2_ppm", "CH4_d_ppm", "CO2_d_ppm", "Gas_Temp_C", "Amb_Temp_C" )))
        
        #Change format of Date Time
        dirty_raw_data$Date_Time <- as.character(gsub("/","-",  dirty_raw_data$Date_Time))
        dirty_raw_data$Date_Time <- as.POSIXct(dirty_raw_data$Date_Time, format = "%m-%d-%Y %H:%M:%OS")  #Format date time 
        formatted_LGR_data <- as.data.frame(dirty_raw_data) #Save output 
      }
      
      # Apply the cleaning function to the raw LGR files 
      clean_LGR_lst <- lapply(raw_LGR_lst, Format_raw_LGR_FUNC)
      head(clean_LGR_lst[[27]])
      
      # 1.3 Format Time 
      FormattTime_FUNC <- function(conc_data){
        
        # Make a time column that is simply the number of second from the start of the run 
        conc_data$Striped_Time <- strptime(conc_data$Date_Time, format = "%Y-%m-%d %H:%M:%S")   #Re-format or "strip" the time so that you can treat it like a numeric and subtract with it
        stripped_start_time <- conc_data[1, "Striped_Time"]  #Save the start time as the first entry of striped time to use to remove the first 20 second
        conc_data$Time <-  conc_data$Striped_Time - stripped_start_time  #Calculate Time from Start (Time) by subtracting the start time from the time of each measurement 
        conc_data$Time <- as.numeric(conc_data$Time)
        
        # Remove the seconds from the start time so that you can match it up with recorded start time 
        conc_data <- as.data.frame(subset(conc_data, select=c("Date_Time", "Time","CH4_d_ppm", "CO2_d_ppm"))) 
        conc_data$Date_Time_Char <- as.character(conc_data$Date_Time)  # format the date time as a character string 
        conc_data$Date_Time <- substring(conc_data$Date_Time_Char, 1, 16)  # select only the first character through the 16th character (removing the seconds)
        conc_data$Date_Time <- as.POSIXct(conc_data$Date_Time, tz = "UTC", format = "%Y-%m-%d %H:%M") # formatt back to a POSIXct 
        conc_data <- as.data.frame(subset(conc_data, select=c("Date_Time", "Time","CH4_d_ppm", "CO2_d_ppm"))) # save the output columns that you want 
        conc_data <- as.data.frame(conc_data)
      }
      
      # Check that the function works 
      check <- clean_LGR_lst[[12]]
      test_result <- FormattTime_FUNC(check)
      head(test_result)
      
      # Apply the time formatting function to the list of cleaned LGR data
      clean_time_LGR_lst <- lapply(clean_LGR_lst, FormattTime_FUNC)
      
# Start here next time, add ID Column
      
          # Data to check function 
            conc_df <- (clean_time_LGR_lst[[6]])
            head(chamber_meas)
      
      # 1.4 Add ID as a column 
      AddID_FUNC <- function(conc_df, chamber_meas){
        conc_df$Date_Time <- as.character(conc_df$Date_Time)
        chamber_meas$Launch_Time <- as.character(chamber_meas$Launch_Time)
        start_time <- conc_df[1, "Date_Time"]
        start_ID <- chamber_meas[chamber_meas$Launch_Time == start_time , ]
        length_of_start_ID <- nrow(start_ID)
        ID <- start_ID[1, "ID"]
        ID
        conc_df$ID <- ifelse(length_of_start_ID == 0 , "NA", ID) # Name air if they don't have a corresponding ID from Cham Meas
        conc_df <- as.data.frame(conc_df)
      }
      
          # Check function 
          practice_conc_df <- clean_time_LGR_lst[[43]]
          conc_df <- AddID_FUNC(conc_df = practice_conc_df, chamber_meas = chamber_meas)  # This fucntion works to pull the correct ID and add it as a column to the df
          head(conc_df)
      
      # Apply the AddID function across all of the concentration 
      clean_time_names_LGR_lst <- lapply(clean_time_LGR_lst, chamber_meas = chamber_meas,AddID_FUNC)
            # Check that the function worked 
            head(clean_time_names_LGR_lst[[9]])
      
      # 1.5 Name all of the elements in the list 
      #write a function to pull the first entry in the ID column 
      ExtractID_FUNC <- function(df){
        Run_ID <- df[[1, "ID"]]
        Run_ID
      }
      
      #Apply that function across the full list of dataframes and save the output 
      IDs_in_order <- lapply(clean_time_names_LGR_lst, ExtractID_FUNC)
      
      # Rename all of the elements in the list 
      names(clean_time_names_LGR_lst) <- IDs_in_order
      names(clean_time_names_LGR_lst)
      
      #Make a list of all of the air runs 
      chamber_meas_air <- chamber_meas[chamber_meas$Rep == "air", ]
      air_IDs <- chamber_meas_air$ID
      air_IDs
      
      # Make Separate Lists of the Run files and the Air Files 
      LGR_lst <- clean_time_names_LGR_lst[!names(clean_time_names_LGR_lst) %in% air_IDs]
      LGR_lst_air <- clean_time_names_LGR_lst[names(clean_time_names_LGR_lst) %in% air_IDs]
      names(LGR_lst)

      
# ________________________________________________________________________________________________
##### 2) Trim: Placement Window ####  
# Trim LGR file to only the time when the chamber was in place on collar or on the water  
      
      # 2.1 Calculate Placement Delay 
      # Subtract the placement time and the launch time to get the number of seconds after launch that the chamber was placed (placement delay in seconds)
      chamber_meas$Launch_Time_LGR <- as.POSIXct(chamber_meas$Launch_Time_LGR, format = "%Y-%m-%d %H:%M:%OS") # format the time 
      chamber_meas$Placement_Time_LGR <- as.POSIXct(chamber_meas$Placement_Time_LGR, format = "%Y-%m-%d %H:%M:%OS") # Format the time 
      chamber_meas$Placement_Delay <- chamber_meas$Placement_Time_LGR - chamber_meas$Launch_Time_LGR #subtract launch time and placement time 
      chamber_meas$Placement_Delay <- as.numeric(chamber_meas$Placement_Delay) + 60 # Set the placement delay to the next full minute (top) after the chamber was placed 
      chamber_meas$Chamber_Removed <- chamber_meas$Placement_Delay + (60 * 4.5) #Set the time removed to 4 and a half minutes after started (we commonly get funky things in the last 30 sec as people walk up to the chamber to remove it)
      names(chamber_meas)[names(chamber_meas) == "Run_ID"] <- "ID"
      
      # 2.2  Write a function to trim based on placement delay and removal time 
      Trim_PlacementDelay_FUNC <- function(concentration_df, windows_df){
        concentration_df$ID <- as.character(concentration_df$ID)  # format ID to be a character in concentration df
        windows_df$ID <- as.character(windows_df$ID)  #format ID to be a character in windows_df
        unique_ID <- concentration_df[1, "ID"]  # save Unique ID as a variable 
        windows_df_specific <- windows_df[windows_df$ID == unique_ID , ]  # make a new dataframe from the windows_df with only the rows that correspond to the unique ID 
        
        placement_delay <- windows_df_specific[1, "Placement_Delay"]  #Using that new data frame that you made save the placement delay time 
        placement_delay <- as.numeric(placement_delay)  # format that placement delay as numeric 
        
        chamber_removed <- windows_df_specific[1, "Chamber_Removed"]  #Using that new data frame that you made save the placement delay time 
        chamber_removed <- as.numeric(chamber_removed)  # format that placement delay as numeric 
        
        concentration_df$start_time <- concentration_df[1, "Date_Time"]  # make a start time column 
        df <- concentration_df[concentration_df$Time >= placement_delay & concentration_df$Time <= chamber_removed , ]  # make a subsetted data frame that only contains the rows after the chamber was placed  
        trimmed_df <- as.data.frame(df) # Save output as a data frame 
      }
      
      # Check function 
      check <- Trim_PlacementDelay_FUNC(LGR_lst[[42]], chamber_meas)
      head(check)
      
      # Apply the function over the full list of LGR data to trim all to the placement time and save as a new list 
      LGR_lst_placement_trimmed <- lapply(LGR_lst, windows_df = chamber_meas, FUN = Trim_PlacementDelay_FUNC)
      head(LGR_lst_placement_trimmed[["HAM_SS_GLL_R1_Col3"]]) # Check 
      
# ________________________________________________________________________________________________
##### 3) Trim: Bubbles and Disturbance   ####        