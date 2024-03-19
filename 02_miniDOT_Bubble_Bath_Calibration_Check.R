# MINIDOT BUBBLE BATH CALIBRATION CHECK 
#________________________________________________________
# Holgerson Lab - Gannon 02-10-2023


# 0. Set up ####

  # Load Packages 
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
    #install.packages("ggpmisc") <- try on desktop 
    # library(ggpmisc)

  # Set working directory 
    setwd("~/HolgersonLab_Helpful_Code")# Desktop 
    # setwd("~/OneDrive/Holgerson_Lab/HolgersonLab_Helpful_Code") # Mac

  # Load data  
    
      # miniDOT Data 
      setwd("~/Lab_Manager/Data_Offload/MiniDOT/240306_miniDOT_CalibrationCheck") # Desktop - #Reset the working directory to the folder containing just the files that need to be formatted
      # setwd("~/OneDrive/Holgerson_Lab/HolgersonLab_Helpful_Code") # Mac
      file_names <- list.files(pattern="*.TXT") #Get a list of all of the .txt files in the working directory 
      list_of_miniDOT_SN <- lapply(file_names, read.table, skip = , header = T, sep = ",", fill = TRUE)
      list_of_miniDOT_data <- lapply(file_names, read.table, skip = 6, header = T, sep = ",", fill = TRUE) #this pulls in the data but looses the serial numbers   
      setwd("~/HolgersonLab_Helpful_Code")    # Reset the working directory to the base 

      #Logger names 
      logger_names <- read_xlsx("miniDOT_Logger_Names.xlsx")
      logger_names <- subset(logger_names, select = c("miniDOT_Logger_Name", "Serial_Number"))
      names(logger_names)[names(logger_names) == "miniDOT_Logger_Name"] <- "Logger_Name"
      
#1.Format Data ####

  # Write a function to clean up the raw miniDOT data and get it in a format that is easier to work with in R
  miniDOT_cleaning_FUNC <- function(df){
    names(df) <- c("Unix.Timestamp_sec", "UTC_Date_Time", "EST", "Batter_volt", "Temp_C", "Dissolved_Oxygen_mg_l", "Dissolved_Oxygen_Sat_%", "Q")
    df2 <- df[-1, ]
    head(df2)
    df2 <- subset(df2, select = c("EST", "Temp_C", "Dissolved_Oxygen_mg_l", "Dissolved_Oxygen_Sat_%"))
    names(df2) <- c("Date_Time","Temp_C", "DO_mgl", "DOSat_per")
    head(df2)
    df2$Temp_C <- as.numeric(df2$Temp_C)
    df2$DO_mgl <- as.numeric(df2$DO_mgl)
    df2$DOSat_per <- as.numeric(df2$DOSat_per)
    df2$Date_Time <- as.character(df2$Date_Time)  # Needs to be as a character to pass through forloop bellow
    #df2$Date_Time  <- as.POSIXct(df2$Date_Time, tz = "EST", format = "%Y-%m-%d %H:%M")
    formatted_output <- as.data.frame(df2)
  }

      # Test that your function works 
      test_df <- list_of_miniDOT_data[[8]]
      test_output <- miniDOT_cleaning_FUNC(test_df)
      head(test_output)

    # Apply your cleaning function over the full list of miniDOT data 
    list_cleaned_miniDOT_data <- lapply(list_of_miniDOT_data, miniDOT_cleaning_FUNC)
    head(list_cleaned_miniDOT_data[[6]]) #Check that cleaning everything together worked 


# 2. Extract meta data ####
    # Extract the meta data so that you can connect to serial number 

    # Extract MetaData  and add them to each file 
    head(list_of_miniDOT_SN[[1]])
    df_a <- list_of_miniDOT_SN[[1]]
    head(df_a)
    names(df_a)
    
    # Write a function to extract the metadata for each miniDOT data file 
    extractMetaData_FUNC <- function(df_a){
      sn_full <- df_a[1, "MiniDOT.Logger.Concatenated.Data.File"]
      sn <- substring(sn_full, 21, 31)
      concatDate_full <- df_a[2, "MiniDOT.Logger.Concatenated.Data.File"]
      concatDate <- substring(concatDate_full, 21,43) 
      output <- c(sn, concatDate)
    }
    
    # Test your metadata extraction function on one df 
    test_data <- list_of_miniDOT_SN[[4]]
    test_output <- extractMetaData_FUNC(test_data)
    test_output
    
    # Apply that metadata extraction function across all of the miniDOT SNs 
    metaData <- lapply(list_of_miniDOT_SN, extractMetaData_FUNC)
    
    # Format the output 
    metaData_df <- as.data.frame(metaData)
    metaData_df <- as.data.frame(t(metaData_df))
    names(metaData_df)
    names(metaData_df) <- c("SN", "Concat_Date")
    head(metaData_df)
    row_nums <- seq(1:nrow(metaData_df))
    row.names(metaData_df) <- row_nums
    row.names(metaData_df)
    
    # Name all of the dfs in your list of cleaned miniDOT data with their serial number 
    serial_numbers <- metaData_df$SN 
    names(list_cleaned_miniDOT_data) <- serial_numbers
    names(list_cleaned_miniDOT_data)
    
    serial_numbers

# 3. Add Serial Numbers ####
    # Add serial number as a column in each data frame and put them all into one big data frame

    #Practice 
    str(list_of_miniDOT_data[[5]])
    
    clean_df1 <- list_cleaned_miniDOT_data[[1]]
    sn_df1 <- metaData_df[1, "SN"]
    clean_df1$Serial_Number <- sn_df1
    head(clean_df1)
    
    # Make a blank data frame to put the results into
    miniDOT_cleaned_data_sn <- data.frame(matrix(ncol = 5, nrow = 1))
    colnames(miniDOT_cleaned_data_sn) <- c("Date_Time", "Temp_C", "DO_mgl", "DOSat_per", "Serial_Number")
    
    # Run through a for loop to add serial number to each dataframe and add the data frames together into one big df 
    total_rows <- length(list_cleaned_miniDOT_data)
    row_seq <- seq(1:total_rows)
    
    for (i in row_seq ){
      clean_df1 <- list_cleaned_miniDOT_data[[i]]
      sn_df1 <- metaData_df[i, "SN"]
      clean_df1$Serial_Number <- sn_df1
      miniDOT_cleaned_data_sn <- rbind(miniDOT_cleaned_data_sn, clean_df1)
    }
    
    miniDOT_cleaned_data_sn <- miniDOT_cleaned_data_sn[-1,] #Delete the first row of NAs from when you made the blank data set  
    head(miniDOT_cleaned_data_sn) # Check 
    miniDOT_data <- as.data.frame(miniDOT_cleaned_data_sn)
    
    # Format the output 
    str(miniDOT_data)
    miniDOT_data$Serial_Number <- as.factor(miniDOT_data$Serial_Number)
    head(miniDOT_data)
    tally(~Serial_Number, data = miniDOT_data)
    miniDOT_data$Date_Time  <- as.POSIXct(miniDOT_data$Date_Time, tz = "EST", format = "%Y-%m-%d %H:%M")
    head(miniDOT_data)
    str(miniDOT_data)


# 4. Trim to Time Window ####
    #Trim to only the time window that you are interested in 

    # Round 1 
    # round1_SNs <- c("7392-941101", "7392-094016", "7392-149054", "7392-591332", "7450-913951", "7450-917361", "7450-960183", "7450-981145", "7392-936968","7450-919756", "7450-000560", "7450-654299", "7450-542721", "7450-543142")
    # miniDOT_data_r1 <- miniDOT_data[miniDOT_data$Serial_Number %in% round1_SNs , ]
    start_time <- as.POSIXct("2024-03-06 13:00:00", tz = "EST", format = "%Y-%m-%d %H:%M")
    end_time <- as.POSIXct("2024-03-06 18:00:00", tz = "EST", format = "%Y-%m-%d %H:%M")
    miniDOT_data_trimmed <- miniDOT_data[miniDOT_data$Date_Time >= start_time & miniDOT_data$Date_Time <= end_time , ]

    #Add Logger Names 
    miniDOT_data_trimmed <- left_join(miniDOT_data_trimmed, logger_names)
    head(check)
    tally(~Serial_Number, data = check)
    
# Remove sensor that looks nutso 
    miniDOT_data_sub <- miniDOT_data_trimmed[miniDOT_data_trimmed$Logger_Name != "Sheel-7", ]
    miniDOT_data_sub <- miniDOT_data_sub[miniDOT_data_sub $Logger_Name != "mini14", ]

# 6. Plot DO over time for the desired interval 

# Round 1 of Calibration (first bucket in first time window)
miniDOT_calibration_check_plot  <- miniDOT_data_sub %>%
  ggplot(aes(x=Date_Time, y = DOSat_per)) +
  geom_line(aes(y = DOSat_per, color = Logger_Name)) + 
  geom_point(aes(y = DOSat_per, color = Logger_Name)) + 
  theme_bw() +
  ylab("Dissolved Oxygen Saturation (%)") + xlab("Time") + ggtitle("miniDOT Bubble Bath")
miniDOT_calibration_check_plot




# 7. Save Output 
?ggsave()
setwd("~/Equipment/miniDOT_Calibration_Check/Output_Figures")

ggsave("Calibration_Check_Plot_R1_Holgerson_and_Alex.png", miniDOT_calibration_check_plot1, width = 190, height = 120, units = "mm")
ggsave("~/HolgersonLab_Helpful_Code/Output_Figures/Calibration_Check_Plot_R2_Sheel.png", miniDOT_calibration_check_plot2, width = 190, height = 120, units = "mm")
