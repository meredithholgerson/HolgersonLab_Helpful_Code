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
    
# Just r bind it all together into one 
    hobo_comp <- bind_rows(output_clean_hobo_fun)
    
    
# Plot to check 
    hobo_icebath_check_plot  <- hobo_comp %>%
      ggplot(aes(x=Date_Time, y = DOSat_per)) +
      geom_line(aes(y = DOSat_per, color = Serial_Number)) + 
      geom_point(aes(y = DOSat_per, color = Serial_Number)) + 
      theme_bw() +
      ylab("Dissolved Oxygen Saturation (%)") + xlab("Time") + ggtitle("miniDOT Calibration Check: Holgerson Lab and Alex")
    miniDOT_calibration_check_plot1 
    
# Take Average temp for each serial number 
    
    
    
#### OLD 
    
  

# Put data from all loggers into a single dataframe with each column being the temp from a different sensor for each timepoint 
    hobo_dat_1 <- output_clean_hobo_fun[[1]]
    hobo_dat_2 <- output_clean_hobo_fun[[2]]
    hobo_dat_3 <- output_clean_hobo_fun[[3]]
    hobo_dat_4 <- output_clean_hobo_fun[[4]]
    hobo_dat_5 <- output_clean_hobo_fun[[5]]
    hobo_dat_6 <- output_clean_hobo_fun[[6]]
    hobo_dat_7 <- output_clean_hobo_fun[[7]]
    hobo_comp <- full_join(hobo_dat_1, hobo_dat_2)
    hobo_comp <- full_join(hobo_comp, hobo_dat_3)
    hobo_comp <- full_join(hobo_comp, hobo_dat_4)
    hobo_comp <- full_join(hobo_comp, hobo_dat_5)
    hobo_comp <- full_join(hobo_comp, hobo_dat_6)
    hobo_comp <- full_join(hobo_comp, hobo_dat_7)  # only keeping the times that they all have in common 
    
# Cut to only the time window that you are interested in 
    
    
    
    
    

    

# Being Lazy and trying to figure out how to pull all of the data frames out of the list and into the environment as seperate data frames without doing it one at a time 
    #You could do it with a fore loop 
    
    # or just make a dataframe 
    datacompile <- data.frame(x=c('a', 'b', 'c'),
                              y = c(1,2,3))
    datacompile <- data.frame(output_clean_hobo_fun[[1]][2],output_clean_hobo_fun[[2]][2])
    
    
    names(output_clean_hobo_fun)
    do.call("rbind", output_clean_hobo_fun)
    
    ?list2env()
    list2env(output_clean_hobo_fun,globalenv())
    
    L <- list(a = 1, b = 2:4, p = pi, ff = gl(3, 4, labels = LETTERS[1:3]))
    list2env(L)
    
    obj <- list(a=1:5, b=2:10, c=-5:5)
    ls()
    list2env(obj,globalenv())
    ls()
    lapply(seq_along(obj), function(i) assign(names(obj)[i], obj[[i]], envir = .GlobalEnv))
    