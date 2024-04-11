# ////////////////////////////////////////
# LINEAR FLUX CALCULATIONS ####
# ////////////////////////////////////////


# ________________________________________________________________________________________________
## 0 Set up R Environment ###

    # Set Working Directory   
    setwd("~/Wetland_GHG_Survey/DECWetlandFlux-GitHubCopied/2023_Flux_Calculations")  # Desktop 
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
chamber_meas <- read_xlsx("Output_Files/chamber_meas_15feb2024.xlsx") #updated chamber meas with start times to match LGR files and not loose data 
chamber_meas <- as.data.frame(chamber_meas)   
head(chamber_meas)   
str(chamber_meas)

# Temp Data 
temp_data <- read_xlsx("Output_Files/avg_temp_cham_28March2024.xlsx")


# LGR Data 
SlopeCalc_Output <- read_xlsx("Output_Files/SlopeCalc_output_27March2024.xlsx")

# ________________________________________________________________________________________________
## 1) Convert to Flux #### 

# 3.1 Bring in and format data that you need to do conversion from slope to flux        
head(SlopeCalc_Output)
head(chamber_meas) 
head(temp_data)

temp_data <- as.data.frame(temp_data)
temp_data$Temp_C <- as.numeric(temp_data$Temp_C)
temp_data$Temp_K <- temp_data$Temp_C + 273.15
temp_data <- subset(temp_data, select = c(Run_ID, Temp_K))

conv_data <- subset(chamber_meas, select = c( "Run_ID", "Atmospheric_Pressure_atm","Total_Vol_m3", "Chamber_Area_m2"))
conv_data <- left_join(conv_data, temp_data)

names(conv_data)[names(conv_data) == "Atmospheric_Pressure_atm"] <- "Pressure_atm"
names(conv_data)[names(conv_data) == "Total_Vol_m3"] <- "Volume_m3"
names(conv_data)[names(conv_data) == "Run_ID"] <- "Unique_ID"
head(conv_data)

flux_data <- left_join(SlopeCalc_Output, conv_data)

# Formatt everything to numeric 
flux_data <- flux_data %>% mutate_at(c("CO2_ppm_per_sec", "CO2_R2", "CO2_p_value", "CO2_start_ppm", "CH4_ppm_per_sec", "CH4_R2", "CH4_p_value", "CH4_start_ppm" ), as.numeric)
str(flux_data)
head(flux_data)

# 3.2 Convert CH4_ppm_per_sec and CO2_ppm_per_sec (slope values from portable or GC) from ppm_sec to atm_sec so that they match the R 
flux_data$CH4_atm_per_sec <- (flux_data$CH4_ppm_per_sec/ 1E6 ) * flux_data$Pressure_atm   #Divide parts per million by one million and multiply by the atmospheric pressure at the site 
flux_data$CO2_atm_per_sec <- (flux_data$CO2_ppm_per_sec / 1E6 ) * flux_data$Pressure_atm    #Divide parts per million by one million and multiply by the atmospheric pressure at the site 
# The first part of this is to go from ppm per second (which we assume is equal to microatmospheres per second) to atmospheres per second 
# and the second part is to the elevation adjusted atmophere per second  

# 3.3 Solve for moles per second of CH4 and CO2 using ideal gas law (PV= nRT)         # This is where you could experiment with using the temp from the LGR 
R <- (8.20573 * (10^(-5)))  #Ideal Gas Constant ( m^3 * atm * mol^(-1))
flux_data$CH4_mol_per_sec <- ( flux_data$CH4_atm_per_sec * flux_data$Volume_m3) / (R * flux_data$Temp_K)
flux_data$CO2_mol_per_sec <- ( flux_data$CO2_atm_per_sec * flux_data$Volume_m3) / (R * flux_data$Temp_K)

# 3.4 Standardize flux by the the area in the chamber (mols per second per m2) 
flux_data$CH4_mol_per_sec_per_m2 <- flux_data$CH4_mol_per_sec / flux_data$Chamber_Area_m2
flux_data$CO2_mol_per_sec_per_m2 <- flux_data$CO2_mol_per_sec / flux_data$Chamber_Area_m2
str(flux_data)

# 3.5 Change to per hour instead of per second 
flux_data$CH4_mol_hr_m2 <- flux_data$CH4_mol_per_sec_per_m2 * 60 * 60 
flux_data$CO2_mol_hr_m2 <- flux_data$CO2_mol_per_sec_per_m2 * 60 * 60 

# 3.6 Change from mol to mico-moles 
flux_data$CH4_mmol_hr_m2 <- flux_data$CH4_mol_hr_m2 * 1000
flux_data$CO2_mmol_hr_m2 <- flux_data$CO2_mol_hr_m2 * 1000


# 3.7 Subset to only columns you need 
names(flux_data)
flux_data <- as.data.frame(subset(flux_data, select = c("Unique_ID", "CH4_mmol_hr_m2","CH4_R2","CH4_p_value", "CH4_start_ppm", "CO2_mmol_hr_m2", "CO2_R2", "CO2_p_value", "CO2_start_ppm")))
head(flux_data)

# 3.8 Save output
# write_xlsx(flux_data, "Formatted_Data/flux_data_28March2024.xlsx")

## 4) Initial Look #### , scales = "free", ~Site , title = "Wetlands 2023 CH4 Flux"
head(flux_data)
flux_data$Site <- substring(flux_data$Unique_ID, 1, 10)
flux_data$Round <- substring(flux_data$Unique_ID, 12, 13)
flux_data$Visit <- ifelse(flux_data$Round == "R1", "June/July", "August")
flux_data$Visit <- factor(flux_data$Visit, levels = c("June/July", "August"))
flux_data$Wetland_Type <- substring(flux_data$Unique_ID, 5, 6)
flux_data$Wetland_Type_full <- ifelse(flux_data$Wetland_Type == "EM", "Emergent", 
                                      ifelse(flux_data$Wetland_Type == "FO", "Forested", "Shrub/Scrub"))


CH4_summary_plot <- ggplot(data = flux_data, aes(x = Visit, y = CH4_mmol_hr_m2)) + 
  geom_boxplot(color = "black", fill = "coral1") + 
  theme_bw() + 
  ylim(-0.25, .50) +
  geom_point(aes(color = Site)) +
  labs(x = "Sampling Date", y = "CH4 flux (mmol/hr m2)") +
  facet_wrap(~Wetland_Type_full, scales = "free")
CH4_summary_plot

CO2_summary_plot <- ggplot(data = flux_data, aes(x = Visit, y = CO2_mmol_hr_m2)) + 
  geom_boxplot(color = "black", fill = "cyan3") + 
  theme_bw() +
  geom_point(aes(color = Site)) +
  labs(x = "Sampling Date", y = "CO2 Flux (mmol/hr m2)") +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray") + 
  facet_wrap(~Wetland_Type_full,)
CO2_summary_plot

#install.packages("ggpubr")
library(ggpubr)
ggarrange(CH4_summary_plot, CO2_summary_plot , nrow = 2) 

# Save outputs 