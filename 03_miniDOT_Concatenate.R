######################################
# Concatenating MiniDOT Data  
######################################
# Holgerson Lab - Gannon 05-09-2023

# Folder of folders for each of the loggers

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

# MUD BTM 070857 
#Desktop
setwd("~/Wetland_GHG_Survey/DECWetlandFlux-GitHubCopied/RawData/Portable_Analyzer_2022/LGR_Files_Formatt_in_R") #Reset the working directory to the folder containing just the files that need to be formatted
# Mac #setwd("~/OneDrive/Holgerson_Lab/DECWetlandFlux/RawData/LGR_Files_Formatt_in_R")
raw_file_names <- list.files(pattern="*.txt") #Get a list of all of the .txt files in the working directory 
list_of_raw_LGR_dfs <- lapply(raw_file_names, read.table, skip = 1, header = T, sep = ",", fill = TRUE) #Read all of the files on that list into the R environment (comes in as a list of dataframes)  
#Desktop
setwd("/Users/kag326/Documents/Wetland_GHG_Survey/DECWetlandFlux-GitHubCopied")    # Reset the working directory to the base 
# Mac setwd("~/OneDrive/Holgerson_Lab/DECWetlandFlux")
