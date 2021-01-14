#Master for generating all country chapters figures

#################################################################################
#                        TO FILL BY COUNTRY TEAM                                #
#################################################################################

# "Argentina", "Australia", "Brazil", "Canada",
# "China", "Colombia", "Ethiopia", "Finland",
# "Germany", "India", "Indonesia", "Malaysia",
# "Mexico", "Norway", "Russia", "Rwanda",
# "Sweden", "SouthAfrica", "UK", "USA"

country = "India" #HERE COPY/PASTE YOUR COUNTRY's NAME USING THE LIST ABOVE

# Libraries ---------------------------------------------------------------

libraries <- c("tidyr", "dplyr", "ggplot2", "reshape2", "RColorBrewer", "ggforce", 
               "conflicted", "cowplot", "patchwork", "egg", "readxl", "png", 
               "grid", "scales", "wesanderson", "tidyverse", "ggforce", "latex2exp", 
               "stringr", "shadowtext")
lapply(libraries, library, character.only = TRUE)

require(moonBook)
require(webr)

conflict_prefer("arrange", "dplyr")
conflict_prefer("summarise", "dplyr")

# Path --------------------------------------------------------------------

path <- "C:/Users/Clara Douzal/OneDrive - SDSN Association Paris/FABLE/FABLE Report 2020 Figures/"#edit this yourself

path_data <- paste0(path, "Data/")
path_fct <- paste0(path, "/Programs/Functions Figures/")
outpath <- paste0(path, "Outputs/")

# Data --------------------------------------------------------------------

date_data <- "20201209"
date_product <- "20201209"

data <- read.csv(paste0(path_data, date_data, "_FullDataBase.csv"), sep = "")

product <- read.csv(paste0(path_data, date_product, "_FullProductDataBase.csv"), sep = "")

#UNFCCC data for figure 3
data_F3 <- as.data.frame(read_excel(paste0(path_data, "Figure 3/20201104_FullData.xlsx")))
data_F3$Category <- factor(data_F3$Category, levels = c("AFOLU", "Waste", "Energy", "IPPU", "Other"))

if(country == "Colombia"){
  data_F3 <- as.data.frame(read_excel(paste0(path_data, "Figure 3/Historical_data_COL.xlsx"), 
                        sheet = "Feuil1"))
  data_F3$Category <- factor(data_F3$Category, levels = c("AFOLU", "Waste", "Energy", "IPPU", "Other"))
  
}
# EAT-lancet, FAO and additional data for figure 6
mapping_F6 <- read_excel(paste0(path_data, "Figure 6/DataForFoodFigures.xlsx"), 
                         sheet = "prod groups map")
EAT_data <- read_excel(paste0(path_data, "Figure 6/DataForFoodFigures.xlsx"), 
                       sheet = "EAT-LANCET", range = "A3:J17") 
FOOD_missing <- read_excel(paste0(path_data, "Figure 6/MissingFoodProducts.xlsx"), sheet = "Figure6")
FAO_F6 <- read_excel(paste0(path_data, "Figure 6/FAO_2015.xlsx"))
product <- left_join(product, mapping_F6, by = c("Product" = "PRODUCT"))
magpie <- read_excel(paste0(path_data, "Figure 6/MAgPIE.xlsx"), sheet = "Fig6")

#AQUASTAT data for figure 7
data_F7 <- read_excel(paste0(path_data, "Figure 7/20200601_FullDataWater.xlsx"))

#India's data for figure 9
F9_India <- read_xlsx(paste0(path_data, "Figure 9/20201027_Figure11_India.CKJ.xlsx"),
                      sheet = "clean")

# Colour palettes ---------------------------------------------------------

myColors_GHG_AFOLU <- c("AFOLU" = "#8F4B07",
                        "Waste" = "#8C8C80",
                        "Energy" = "#DBDBCA",
                        "IPPU"= "#DFDE7F",
                        "Other" = "#615D42")

myColors_AFOLU <- colorRampPalette(brewer.pal(8, "Set1"))(nlevels(as.factor(data_F3[which(data_F3$Category == "AFOLU"), "Sub.Category"])))
names(myColors_AFOLU) <- levels(as.factor(data_F3[which(data_F3$Category == "AFOLU"), "Sub.Category"]))

myColors_GHG <- c("darkgrey", "Forestgreen", "Lightgreen")
names(myColors_GHG) <- c("CT", "SS", "SSPlus")

myColors_scen_water <- c("darkgrey", "Forestgreen", "Lightgreen")
names(myColors_scen_water) <- c("CT", "Sust", "SustPlus")


myColorsCO2AFOLU <- c("#B84D50", wes_palette("Darjeeling1", n = 4)[c(2,4)], "#FFC748","#91341d", "#145a32", "#7d7979") 

names(myColorsCO2AFOLU) <- c("CalcLiveAllCO2e", "CalcDeforCO2", 
                             "CalcOtherLUCCO2","CalcCropAllCO2e", "CalcSequestCO2",
                             "CalcPeatCO2","GHGbiofuels")

myColors_GHG_fig4 <- c("CalcLiveAllCO2e" = "#B84D50",
                       "CalcCropAllCO2e" = "#FFC748",
                       "GHGbiofuels" = "#F2BBC9",
                       "CalcAllLandCO2e" = "#287A11", 
                       "CalcLandCO2eNeg" = "#287A11") 


# Countries with only 1 Sust scenario -------------------------------------

OnlyOneSust <- as.factor(c("Argentina", "Australia", "China", "Colombia", "Ethiopia", "Finland", "India", "Indonesia",
                           "Malaysia", "Mexico", "Norway", "R_AFR", "R_ASIPAC", "R_LAM", "R_MECAS", "R_NEU", "R_OEU", "Russia", "Rwanda", "SouthAfrica"))


# Source function figures -------------------------------------------------

source(paste0(path_fct, "fct_Figure1.R"))
source(paste0(path_fct, "fct_Figure2.R"))
source(paste0(path_fct, "fct_Figure3.R"))
source(paste0(path_fct, "fct_Figure4.R"))
source(paste0(path_fct, "fct_Figure5.R"))
source(paste0(path_fct, "fct_Figure6.R"))
source(paste0(path_fct, "fct_Figure7_PieDonutFABLE.R"))#contains a function called in 20210113_fct_Figure7
source(paste0(path_fct, "fct_Figure7.R"))
source(paste0(path_fct, "fct_Figure8.R"))
source(paste0(path_fct, "fct_Figure9.R"))
source(paste0(path_fct, "fct_Figure10.R"))


# Render Function ---------------------------------------------------------

#A MODIFIER
render_report <- function(data, product, data_F3, mapping_F6, 
                          EAT_data, FOOD_missing, data_F7,
                          FAO_F6, magpie, F9_India, country){
  
  
  data <- droplevels(data[which(data$Country == country),])
  product <- droplevels(product[which(product$Country == country),])
  if(country != "Colombia"){
    data_F3 <- droplevels(data_F3[which(data_F3$Country == country),])
  }else{
    data_F3 <- droplevels(data_F3)}
  FAO_F6 <- droplevels(FAO_F6[which(FAO_F6$Country == country),])
  FOOD_missing <- droplevels(FOOD_missing[which(FOOD_missing$Country == country),])
  data_F7 <- droplevels(data_F7[which(data_F7$Country == country),])
  
  
  if(country %in%OnlyOneSust){
    data <- droplevels(data[-which(data$Pathway == "SustPlus"),])
    product <- droplevels(product[-which(product$Pathway == "SustPlus"),])
  }
  

  # # 
  fct_Figure1(data, outpath, country)
  #
  fct_Figure2(data, outpath, country)
  #
  fct_Figure3(data_F3, outpath, country)
  #
  fct_Figure4(data, outpath, data_F4, country)
  #
  fct_Figure5(data, outpath, country)
  #
  fct_Figure6(data, product, mapping_F6, EAT_data, FOOD_missing, outpath, FAO_F6, magpie, country)
  #
  fct_Figure7(data_F7, outpath, country)
  # 
  fct_Figure8(data, outpath, country)
  # 
  fct_Figure9(product, outpath, F9_India, country)
  #
  fct_Figure10(product, outpath, country)
  #  
}


# Running over multiple parameters ----------------------------------------


for (country in country){
  render_report(data, product, data_F3, mapping_F6, 
                EAT_data, FOOD_missing, data_F7, 
                FAO_F6, magpie, F9_India, country)
}

