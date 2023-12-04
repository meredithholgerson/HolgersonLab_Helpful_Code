######################################
# Concatenating MiniDOT Data  
######################################
# Holgerson Lab - Gannon 05-09-2023

# Folder of folders for each of the loggers
# ---> this is clunky and UGLY but works 

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
#install.packages("ggpmisc") <- try on desktop 
# library(ggpmisc)

# PUll in all files For each sensor 
# for list file names can also pull recursive and bull all of the dfs but then just need to separate by sensor, name by file 

# Alex 01 
setwd("~/K Gannon/Data_Offload/MiniDOT/230509_TXH_MUD_KG/Alex_01") #Reset the working directory to the folder containing just the files that need to be formatted
raw_file_names <- list.files(pattern="*.txt") #Get a list of all of the .txt files in the working directory 
list_Alex_01 <- lapply(raw_file_names, read.table, skip = 2, header = T, sep = ",", fill = TRUE) #Read all of the files on that list into the R environment (comes in as a list of dataframes)  
Alex_01 <- bind_rows(list_Alex_01, .id = "column_label")
setwd("~/K Gannon/Data_Offload/MiniDOT/230509_TXH_MUD_KG")
write_xlsx(Alex_01, "Concatenated/Alex_01_ConCat_050923.xlsx")

# Alex 04 
setwd("~/K Gannon/Data_Offload/MiniDOT/230509_TXH_MUD_KG/Alex_04") #Reset the working directory to the folder containing just the files that need to be formatted
raw_file_names <- list.files(pattern="*.txt") #Get a list of all of the .txt files in the working directory 
list_Alex_04 <- lapply(raw_file_names, read.table, skip = 2, header = T, sep = ",", fill = TRUE) #Read all of the files on that list into the R environment (comes in as a list of dataframes)  
Alex_04 <- bind_rows(list_Alex_04, .id = "column_label")
setwd("~/K Gannon/Data_Offload/MiniDOT/230509_TXH_MUD_KG")
write_xlsx(Alex_04, "Concatenated/Alex_04_ConCat_050923.xlsx")

# Alex 02 
setwd("~/K Gannon/Data_Offload/MiniDOT/230509_TXH_MUD_KG/Alex_02") #Reset the working directory to the folder containing just the files that need to be formatted
raw_file_names <- list.files(pattern="*.txt") #Get a list of all of the .txt files in the working directory 
list_Alex_02 <- lapply(raw_file_names, read.table, skip = 2, header = T, sep = ",", fill = TRUE) #Read all of the files on that list into the R environment (comes in as a list of dataframes)  
Alex_02 <- bind_rows(list_Alex_02, .id = "column_label")
setwd("~/K Gannon/Data_Offload/MiniDOT/230509_TXH_MUD_KG")
write_xlsx(Alex_02, "Concatenated/Alex_02_ConCat_050923.xlsx")

# Alex 04 
setwd("~/K Gannon/Data_Offload/MiniDOT/230509_TXH_MUD_KG/Alex_09") #Reset the working directory to the folder containing just the files that need to be formatted
raw_file_names <- list.files(pattern="*.txt") #Get a list of all of the .txt files in the working directory 
list_Alex_09 <- lapply(raw_file_names, read.table, skip = 2, header = T, sep = ",", fill = TRUE) #Read all of the files on that list into the R environment (comes in as a list of dataframes)  
Alex_09 <- bind_rows(list_Alex_09, .id = "column_label")
setwd("~/K Gannon/Data_Offload/MiniDOT/230509_TXH_MUD_KG")
write_xlsx(Alex_09, "Concatenated/Alex_09_ConCat_050923.xlsx")








