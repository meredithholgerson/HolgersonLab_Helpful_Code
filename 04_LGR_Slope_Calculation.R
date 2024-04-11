# ////////////////////////////////////////
#### FORMATTING, TRIMMING, & QUALITY CHECKING LGR DATA ####
# ////////////////////////////////////////


##### 0) Set Up R Environment  #### 

    # Set working directory 
    setwd("~/Atkinson_Aerator") # Desktop 
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
      chamber_meas <- read_xlsx("LGR_Field_Data_11April2024.xlsx") #updated chamber meas with start times to match LGR files and not loose data 
      chamber_meas <- as.data.frame(chamber_meas)   
      head(chamber_meas)   
      str(chamber_meas)
      
        
      # Annotation Notes 
      trim_annotations <- read_xlsx("Trimming_Windows_Annotated_11April2024.xlsx")
      

# LGR Data 
    # setwd("~/OneDrive/Holgerson_Lab/Data_Offloading/LGR_Data/2023_Wetlands_LGR") #Mac # Set working directory to the folder where you have all of the LGR txt files 
      setwd("~Atkinson_Aerator/LGR_Aerator_Control")# Desktop 
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
      chamber_meas <- chamber_meas[chamber_meas$Rep != "air", ]
      air_IDs <- chamber_meas_air$ID
      air_IDs
      
      # Make Separate Lists of the Run files and the Air Files 
      LGR_lst <- clean_time_names_LGR_lst[!names(clean_time_names_LGR_lst) %in% air_IDs]
      LGR_lst_air <- clean_time_names_LGR_lst[names(clean_time_names_LGR_lst) %in% air_IDs]
      names(LGR_lst)

      
# ________________________________________________________________________________________________
##### 2) Trim: Placement Window ####  
# Trim LGR file to only the time when the chamber was in place on collar or on the water  
      
      # 2.1 Calculate Placement Delay in seconds 
      # Subtract the placement time and the launch time to get the number of seconds after launch that the chamber was placed (placement delay in seconds)
      str(chamber_meas)
      chamber_meas$Launch_Time <- as.POSIXct(chamber_meas$Launch_Time, format = "%Y-%m-%d %H:%M:%OS") # format the time 
      chamber_meas$Placement_Time <- as.POSIXct(chamber_meas$Placement_Time, format = "%Y-%m-%d %H:%M:%OS") # Format the time 
      
      chamber_meas$Placement_Delay <- chamber_meas$Placement_Time - chamber_meas$Launch_Time #subtract launch time and placement time 
      chamber_meas$Placement_Delay <- as.numeric(chamber_meas$Placement_Delay) + 60 # Set the placement delay to the next full minute (top) after the chamber was placed 
      chamber_meas$Chamber_Removed <- chamber_meas$Placement_Delay + (60 * 4.5) #Set the time removed to 4 and a half minutes after started (we commonly get funky things in the last 30 sec as people walk up to the chamber to remove it)
      
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
      head(LGR_lst_placement_trimmed[["Pond208_2023-04-19_R2"]]) # Check 
      
# ________________________________________________________________________________________________
##### 3) Trim: Bubbles and Disturbance   ####    
# Remove bubble events and any disturbance of placing or removing the chamber
# If you have already set your trimming windows then you do not need to re-run this step 
      
      # 3.1 Make Output File    
      # Make an output file with a list of all of the runs where you can annotate 
      
      # write a function to pull the first entry in the ID column 
      ExtractID_FUNC <- function(df){
        Run_ID <- df[1, "ID"]
        Run_ID
      }
      
      #Check that function works 
      test_df <- LGR_lst[[14]]
      head(test_df)
      ExtractID_FUNC(test_df)
      
      
      # Apply that function across the full list of dataframes, format and save the output 
      IDs_in_order <- lapply(LGR_lst, ExtractID_FUNC)
      df <- data.frame(unlist(IDs_in_order))
      head(df)
      names(df)[names(df) == "unlist.IDs_in_order."] <- "Run_ID"
      head(df)
      num_of_rows <- nrow(df)
      df$Number <- seq(1:num_of_rows)
      df_of_run_IDs <- as.data.frame(subset(df, select = c("Number", "Run_ID")))
      names(df_of_run_IDs)[names(df_of_run_IDs) == "Run_ID"] <- "ID"
      head(df_of_run_IDs)
      
      # Add Placement Delay
      df_of_placement_times <- subset(chamber_meas, select = c("ID", "Placement_Delay", "Chamber_Removed"))
      df_to_annotate <- left_join(df_of_run_IDs,df_of_placement_times)
      head(df_to_annotate)
      
      # Save as an output file to use to annotate 
      df_to_annotate <- as.data.frame(df_to_annotate)
      # write_xlsx(df_to_annotate, "~/Lab_Manager/Data_Offload/Atkinson_Aerator/IDs_to_annotate_11April2024.xlsx")
      
      
      # 3.2 Plot CO2      
      
      # Write a function to create CO2 plots 
      PlotCO2_FUNC <- function(df){
        
        #Calculate R2 
        lmCO2 <- lm(CO2_d_ppm ~ Time, data = df) #Linear Regression of CH4 concentration over time 
        CO2_R2 <- summary(lmCO2)$r.squared   #pull the R^2 of the regression line and save to a variable
        CO2_pvalue <- summary(lmCO2)$coefficients[2,4]   #pull the p-value of the regression line and save to a variable
        
        #Save ID as a variable 
        Run_ID <- df[1, "ID"]
        
        #Plot
        df %>%
          ggplot(aes(x=Time, y = CO2_d_ppm)) +
          geom_line(aes(y = CO2_d_ppm), color = "steelblue") + 
          geom_point(aes(y = CO2_d_ppm), color = "steelblue") + 
          theme_bw() +
          annotate(geom = 'text', label = paste("R2=", round(CO2_R2, 4), "   ", "P-value =", round(CO2_pvalue, 7), sep = " "), x = -Inf, y = Inf, hjust = 0, vjust = 1) + 
          ylab("CO2 (ppm)") + xlab("Time") + ggtitle(paste(Run_ID, "(CO2)", sep = " "))
      }
      
      # Check that the function works 
      practice_plot <- LGR_lst_placement_trimmed[[4]]
      PlotCO2_FUNC(practice_plot)   
      
      # Apply the plotting function across all of your data frames and view the plots  par(ask = TRUE)  
      CO2Plots_lst <- lapply(LGR_lst_placement_trimmed, PlotCO2_FUNC)
      CO2Plots_lst[["Pond211_2023-10-04_R2"]]
      
          # You can also check the full plot (not placement trimmed if that helps with context )
          CO2Plots_lst_NOT_trimmed <- lapply(LGR_lst, PlotCO2_FUNC)
          CO2Plots_lst_NOT_trimmed[[13]]
      
      
      #3.3 Plot CH4            
      
      # Write a function to create the CH4 plots 
      PlotCH4_FUNC <- function(df){
        
        #Calculate R2 
        lmCH4 <- lm(CH4_d_ppm ~ Time, data = df) #Linear Regression of CH4 concentration over time 
        CH4_R2 <- summary(lmCH4)$r.squared   #pull the R^2 of the regression line and save to a variable
        CH4_pvalue <- summary(lmCH4)$coefficients[2,4] # Pull p-value 
        
        #Save ID as a variable 
        Run_ID <- df[1, "ID"]
        
        #Plot
        CH4_Plot <- df %>%
          ggplot(aes(x=Time, y = CH4_d_ppm)) +
          geom_line(aes(y = CH4_d_ppm), color = "maroon") + 
          geom_point(aes(y = CH4_d_ppm), color = "maroon") + 
          theme_bw() +
          annotate(geom = 'text', label = paste("R2=", round(CH4_R2, 4), "    P-value =", round(CH4_pvalue, 4), sep = " "), x = -Inf, y = Inf, hjust = 0, vjust = 1) + 
          ylab("CH4 (ppm)") + xlab("Time") + ggtitle(paste(Run_ID, "(CH4)", sep = " "))
        CH4_Plot 
      }
      
          # Check that the function works 
          practice_plot <- LGR_lst_placement_trimmed[[14]]
          PlotCH4_FUNC(practice_plot)
      
      # Apply the plotting function across all of your data frames and view the plots   
      CH4Plots_lst <- lapply(LGR_lst_placement_trimmed, PlotCH4_FUNC)
      CH4Plots_lst[["Pond211_2023-10-04_R2"]]
      
          # You can also check the full plot (not placement trimmed if that helps with context )
          CH4Plots_lst_NOT_trimmed <- lapply(LGR_lst, PlotCH4_FUNC)
          CH4Plots_lst_NOT_trimmed["Pond208_2023-07-12_R1"]
      
      
# 3.4 Trim LGR Files based on annotations from bubbles and disturbance   
          
   # Write a function to trim based on time [CO2] __________________________
          TrimCO2_FUNC <- function(concentration_df, windows_df){
            concentration_df$ID <- as.character(concentration_df$ID)  # format ID to be a character in concentration df
            windows_df$ID <- as.character(windows_df$ID)  #format ID to be a character in windows_df
            unique_ID <- concentration_df[1, "ID"]  # save Unique ID as a variable 
            windows_df_specific <- windows_df[windows_df$ID == unique_ID , ]  # make a new dataframe from the windows_df with only the rows that correspond to the unique ID 
            window_start_co2 <- windows_df_specific[1, "CO2_Start"]  #Using that new data frame that you made save the CO2 start time 
            window_start_co2 <- as.numeric(window_start_co2)  # format that start time as numeric 
            window_end_co2 <- windows_df_specific[1, "CO2_End"]  # Using the new data frame save the CO2 end time 
            window_end_co2 <- as.numeric(window_end_co2)  # format the end time as a numeric 
            # concentration_df$Time <- as.numeric(concentration_df$Time)
            concentration_df$Start_Time_LGR <- concentration_df[1, "Date_Time"]  # make a start time column 
            df <- concentration_df[concentration_df$Time >= window_start_co2 & concentration_df$Time <= window_end_co2, ]  # make a subsetted data frame that only contains the rows that fall within the designated window 
            trimmed_df <- as.data.frame(df) # Save output as a data frame 
          }
          
          # Check function 
          check <- TrimCO2_FUNC(LGR_lst[[12]], trim_annotations)
          head(check)
          
    # Write a function to trim based on time [CH4] __________________________
          TrimCH4_FUNC <- function(concentration_df, windows_df){
            # concentration_df$ID <- as.factor(concentration_df$ID)
            unique_ID <- concentration_df[1, "ID"]
            windows_df_specific <- windows_df[windows_df$ID == unique_ID , ]
            window_start_ch4 <- windows_df_specific[1, "CH4_Start"]
            window_start_ch4 <- as.numeric(window_start_ch4)
            window_end_ch4 <- windows_df_specific[1, "CH4_End"]
            window_end_ch4 <- as.numeric(window_end_ch4)
            concentration_df$Time <- as.numeric(concentration_df$Time)
            concentration_df$Start_Time_LGR <- concentration_df[1, "Date_Time"]
            df <- concentration_df[concentration_df$Time >= window_start_ch4 & concentration_df$Time <= window_end_ch4, ]
            trimmed_df <- as.data.frame(df)
          }
          
          # Check function 
          check <- TrimCH4_FUNC(LGR_lst[[12]], trim_annotations)
          head(check)
          
    # Run trimming functions over the full list of data sets__________________ 
          
          #  Trim all Data Frames for CO2 and save as a new list 
          list_of_trimmed_for_co2 <- lapply(LGR_lst, windows_df = trim_annotations, FUN = TrimCO2_FUNC)
          head(list_of_trimmed_for_co2[[13]]) # Check 
          
          #  Trim all Data Frames for CH4 and save as a new list 
          list_of_trimmed_for_ch4 <- lapply(LGR_lst, windows_df =  trim_annotations, FUN = TrimCH4_FUNC)
          head(list_of_trimmed_for_ch4[[22]]) # Check 
          
          
      # Remove the runs that you decided to toss in annotation step 
          
          # Remove runs for CO2
          co2_runs_to_remove <-trim_annotations[trim_annotations$CO2_Notes_Cat == "remove" , ]  #Subset the trim annotated df to only the runs that are annotated to remove
          co2_names_of_runs_to_remove <- as.character(co2_runs_to_remove$ID) # Make a list of all of the names of runs to be removed 
          list_of_trimmed_for_co2 <- list_of_trimmed_for_co2[!names(list_of_trimmed_for_co2) %in% co2_names_of_runs_to_remove ] #subset list to only include runs whose names are not in the list of runs to remove 
          
          # Remove runs for CH4
          ch4_runs_to_remove <-trim_annotations[trim_annotations$CH4_Notes_Cat == "remove" , ]  #Subset the trim annotated df to only the runs that are annotated to remove
          ch4_names_of_runs_to_remove <- as.character(ch4_runs_to_remove$ID) # Make a list of all of the names of runs to be removed 
          list_of_trimmed_for_ch4 <- list_of_trimmed_for_ch4[!names(list_of_trimmed_for_ch4) %in% ch4_names_of_runs_to_remove ] #subset list to only include runs whose names are not in the list of runs to remove 
          
          
  # 3.5 Check plots again after trimming    
          
      # Methane   
      CH4Plots_lst <- lapply(list_of_trimmed_for_ch4, PlotCH4_FUNC)
      CH4Plots_lst[["Pond211_2023-09-06_R1"]]
      
      # Carbon Dioxide
      CO2Plots_lst <- lapply(list_of_trimmed_for_co2, PlotCO2_FUNC)
      CO2Plots_lst[["Pond211_2023-10-04_R2"]]
      
# ________________________________________________________________________________________________
##### 4) Calc: Slope, p-value, R2 and starting conc  #### 

  # 4.2 Compile the starting concentration, R2, P-value, and slope in ppm/sec for each of the chamber runs  
      
      # CO2 _______________________________________
      # Write Function to calc
      CO2_SlopeCalc_FUNC <- function(trimmed_CO2){
        
        # Start time as Index 
        Run_Start_Time <- as.character(trimmed_CO2[1, "Start_Time_LGR"])
        
        # Starting Concetrations 
        start_conc_co2 <- mean(trimmed_CO2[1:5, "CO2_d_ppm"]) # starting concentration at the begining of the trimmed window
        
        #Linear models and extracting regression coefficients 
        lmCO2 <- lm(CO2_d_ppm ~ Time, data = trimmed_CO2) #Linear Regression of CO2 concentration over time 
        CO2_R2 <- summary(lmCO2)$r.squared   #pull the R^2 of the regression line and save to a variable 
        CO2_pvalue <- summary(lmCO2)$coefficients[2,4]   #pull the R^2 of the regression line and save to a variable 
        CO2_Slope <- summary(lmCO2)$coefficients[2,1]     #pull the slope of the regression line and save to a variable
        
        # Save Results 
        run_results <- c(Run_Start_Time, CO2_Slope, CO2_R2, CO2_pvalue, start_conc_co2)
      }
      
          # Check that the function works on one df 
          df <- list_of_trimmed_for_co2[[16]]
          result <-  CO2_SlopeCalc_FUNC(df)
          result
      
      # Apply the function across data 
      CO2_SlopeCalc_Output <- lapply(list_of_trimmed_for_co2, CO2_SlopeCalc_FUNC)
      
      # Clean up CO2 Output 
      CO2_SlopeCalc_Output <- as.data.frame(CO2_SlopeCalc_Output)  #Save output as a dataframe notice that it saves the results of each GC_data_df into a new column rather than a new row
      t_CO2_SlopeCalc_Output <- t(CO2_SlopeCalc_Output)  #So transpose the results output 
      t_CO2_SlopeCalc_Output <- as.data.frame(t_CO2_SlopeCalc_Output)  #And save the results as a dataframe to make them easier to work wtith
      
      names(t_CO2_SlopeCalc_Output)
      names(t_CO2_SlopeCalc_Output) <- c("Start_Time_LGR", "CO2_ppm_per_sec", "CO2_R2", "CO2_p_value", "CO2_start_ppm")  #name the columns of the results output
      
      # Save the row names, these are the Unique IDs and add them as a column 
      Unique_IDs <- rownames(t_CO2_SlopeCalc_Output)  
      t_CO2_SlopeCalc_Output$Unique_ID <- Unique_IDs
      num_rows <- nrow(t_CO2_SlopeCalc_Output) # reset row names  
      rownames(t_CO2_SlopeCalc_Output) <- seq(1:num_rows)  
      
      CO2_SlopeCalc_Output_formatted <- as.data.frame(t_CO2_SlopeCalc_Output)  #save this with a nicer name now that you have it set 
      
      head(CO2_SlopeCalc_Output_formatted)
      
      # CH4 _______________________________________  
      # Write Function to calc  
      CH4_SlopeCalc_FUNC <- function(trimmed_ch4){
        # Start time as Index 
        Run_Start_Time <- as.character(trimmed_ch4[1, "Start_Time_LGR"])
        
        # Starting Concetrations 
        start_conc_ch4 <- mean(trimmed_ch4[1:5, "CH4_d_ppm"]) # starting concentration at the begining of the trimmed window
        
        #Linear models and extracting regression coefficients 
        lmCH4 <- lm(CH4_d_ppm ~ Time, data = trimmed_ch4) #Linear Regression of CH4 concentration over time 
        CH4_R2 <- summary(lmCH4)$r.squared   #pull the R^2 of the regression line and save to a variable 
        CH4_pvalue <- summary(lmCH4)$coefficients[2,4]   #pull the p-value of the regression line and save to a variable 
        CH4_Flux <- summary(lmCH4)$coefficients[2,1]     #pull the slope of the regression line and save to a variable
        
        summary(lmCH4)
        summary(lmCH4)$coefficients[2,4]
        
        # Save Results 
        run_results <- c(Run_Start_Time, CH4_Flux, CH4_R2, CH4_pvalue, start_conc_ch4)
      }
      
            # Check that the function works on one df 
            df <- list_of_trimmed_for_ch4[[2]]
            result <-  CH4_SlopeCalc_FUNC(df)
            result
      
      # Apply the Function to all data 
      CH4_SlopeCalc_Output <- lapply(list_of_trimmed_for_ch4, CH4_SlopeCalc_FUNC)
      
      # Format output  
      CH4_SlopeCalc_Output <- as.data.frame(CH4_SlopeCalc_Output)  #Save output as a dataframe notice that it saves the results of each GC_data_df into a new column rather than a new row
      t_CH4_SlopeCalc_Output <- t(CH4_SlopeCalc_Output)  #So transpose the results output 
      t_CH4_SlopeCalc_Output <- as.data.frame(t_CH4_SlopeCalc_Output)  #And save the results as a dataframe to make them easier to work wtith
      
      names(t_CH4_SlopeCalc_Output)
      names(t_CH4_SlopeCalc_Output) <- c("Start_Time_LGR", "CH4_ppm_per_sec", "CH4_R2", "CH4_p_value", "CH4_start_ppm")  #name the columns of the results output
      
      # Save the row names, these are the Unique IDs and add them as a column 
      Unique_IDs <- rownames(t_CH4_SlopeCalc_Output)  
      t_CH4_SlopeCalc_Output$Unique_ID <- Unique_IDs
      num_rows <- nrow(t_CH4_SlopeCalc_Output) # reset row names  
      rownames(t_CH4_SlopeCalc_Output) <- seq(1:num_rows)  
      
      CH4_SlopeCalc_Output_formatted <- as.data.frame(t_CH4_SlopeCalc_Output)  #save this with a nicer name now that you have it set 
      
      head(CH4_SlopeCalc_Output_formatted)
      
      # Put together into one big df 
      head(CH4_SlopeCalc_Output_formatted)
      head(CO2_SlopeCalc_Output_formatted)
      SlopeCalc_Output <- full_join(CH4_SlopeCalc_Output_formatted, CO2_SlopeCalc_Output_formatted )
      head(SlopeCalc_Output)
      
# ________________________________________________________________________________________________
##### 5) QC: Starting Concentration  #### 

# 5.1 Calculate ambient concentrations of CH4 and CO2 
      
  #Write a function to calculate Average and median ambient air concentration 
          
          # Data to write function 
          conc_df <- LGR_lst_air[[4]]
          head(conc_df)
          head(chamber_meas_air)
      
      Ambient_Air_Conc_FUNC <- function(conc_df){
        mean_co2 <- mean(conc_df$CO2_d_ppm)
        median_co2 <- median(conc_df$CO2_d_ppm)
        mean_ch4 <- mean(conc_df$CH4_d_ppm)
        median_ch4 <- median(conc_df$CH4_d_ppm)
        ID <- conc_df[1, "ID"]
        output <- c(ID, mean_co2, median_co2, mean_ch4, median_ch4)
      }
      
          #Check Function 
          check <- Ambient_Air_Conc_FUNC(LGR_lst_air[[8]])
          check
          
  # Apply function across all ambient air runs 
          ambient_air_conc_output <- lapply(LGR_lst_air, Ambient_Air_Conc_FUNC)
          
          # Format the output 
          ambient_air_conc_output <- as.data.frame(ambient_air_conc_output)  #Save output as a dataframe notice that it saves the results of each GC_data_df into a new column rather than a new row
          t_ambient_air_conc_output <- t(ambient_air_conc_output)  #So transpose the results output 
          t_ambient_air_conc_output <- as.data.frame(t_ambient_air_conc_output)  #And save the results as a dataframe to make them easier to work wtith
          
          names(t_ambient_air_conc_output) <- c("ID", "mean_co2", "median_co2", "mean_ch4", "median_ch4")
          row.names(t_ambient_air_conc_output) <- seq(1:nrow(t_ambient_air_conc_output))
          
         ambient_air_conc_output <- as.data.frame(t_ambient_air_conc_output)
         ambient_air_conc_output$Date <- substring(ambient_air_conc_output$ID, 5,15)
          
# Add ambient concentrations to Slope Calc output 
      head(SlopeCalc_Output)
      SlopeCalc_Output$Date <- substring(SlopeCalc_Output$Start_Time_LGR, 1, 10) 
      str(SlopeCalc_Output)
      str(ambient_air_conc_output)
      head(ambient_air_conc_output)
      
      SlopeCalc_Output <- left_join(SlopeCalc_Output, ambient_air_conc_output)
      #write_xlsx(SlopeCalc_Output, "Output_Files/SlopeCalc_Output_26March2024.xlsx")
      head(SlopeCalc_Output)
      
      # Remove runs where the starting concentration is greater than 25% higher than ambient and the slope is negative
      
      # Here the decision is automated but you can also visually check each and then bring in a spreadsheet of what you decided to remove
      SlopeCalc_Output$median_ch4 <- as.numeric(SlopeCalc_Output$median_ch4)
      SlopeCalc_Output$median_co2 <- as.numeric(SlopeCalc_Output$median_co2)
      SlopeCalc_Output$Start_CH4_Decision <- ifelse(SlopeCalc_Output$CH4_start_ppm > (SlopeCalc_Output$median_ch4 * 1.25) & SlopeCalc_Output$CH4_ppm_per_sec < 0, "remove", "good" )
      SlopeCalc_Output$Start_CO2_Decision <- ifelse(SlopeCalc_Output$CO2_start_ppm > (SlopeCalc_Output$median_co2 * 1.25) & SlopeCalc_Output$CO2_ppm_per_sec < 0, "remove", "good" )
      
      # Remove the slope data for runs that you decided to remove 
      SlopeCalc_Output$CH4_ppm_per_sec <- ifelse(SlopeCalc_Output$Start_CH4_Decision == "remove", NA, SlopeCalc_Output$CH4_ppm_per_sec)
      SlopeCalc_Output$CO2_ppm_per_sec <- ifelse(SlopeCalc_Output$Start_CO2_Decision == "remove", NA, SlopeCalc_Output$CO2_ppm_per_sec)
      
# ________________________________________________________________________________________________
##### 6) QC: Linearity  #### 
      
      # all of the runs that had either a high p-value or a low R2 or both where inspected and had flux near zero
      # therefore these runs where retained in the dataset because it is likely that they represent "real" low fluxes
      # and do not present a quality control issue 
      
      
      ##### Save output 
      # write_xlsx(SlopeCalc_Output, "Output_Files/SlopeCalc_output_11April2024.xlsx")
      
      ### Save Ambient Air Concentrations 
      # write_xlsx(ambient_air_conc_output,"Output_Files/Ambient_Air_Conc_11April2024.xlsx" )
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      