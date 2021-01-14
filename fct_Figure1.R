# Figure 1 in country chapters
# Author: Clara Douzal (SDSN)
# Last updated: 20201209

fct_Figure1 <- function(data, outpath, country){
  

# Handling data -----------------------------------------------------------

  
  #total surface of the country
  totalland <- data[which(data$Year == "2010" & data$Pathway == "CT"), 'TotalLand']
  #keep only the variable we are going to use
  vect_variables <- c("Year", "CalcCropland", "CalcPasture", "CalcForest", "CalcNewForest",
                      "CalcOtherLand", "CalcUrban", "ProtectedAreasForest",
                      "ProtectedAreasOtherNat", "ProtectedAreasOther", "TotalLand", "Pathway")
  #clean data
  data <- data %>% 
    select(any_of(vect_variables)) %>% 
    #compute protected area
    mutate(ComputedProtected = rowSums(data[, c("ProtectedAreasForest",
                                                  "ProtectedAreasOtherNat", "ProtectedAreasOther")])) %>% 
    #30% of total land
    mutate(Protected_30 = 0.3 * TotalLand) %>% 
    slice(which(data$Year %in% seq(2000, 2050, 10))) %>% 
    arrange(Year)
  
  
  data <- data %>% 
    select(-starts_with("ProtectedAreas")) %>% 
    select(- TotalLand)
  
  #Create 3 different data set for each pathway
  data_CT <- data[which(data$Pathway == "CT"),]
  data_Sust <- data[which(data$Pathway == "Sust"),]
  data_SustPlus <- data[which(data$Pathway == "SustPlus"),]
  #Remove the pathway variable
  data_CT$Pathway <- NULL
  data_Sust$Pathway <- NULL
  data_SustPlus$Pathway <- NULL
  #Rename the variables
  colnames(data_CT) <- c("Year", paste("CT", colnames(data_CT)[2:ncol(data_CT)], sep = "."))
  colnames(data_Sust) <- c("Year", paste("SS", colnames(data_Sust)[2:ncol(data_Sust)], sep = "."))
  colnames(data_SustPlus) <- c("Year", paste("SSPlus", colnames(data_SustPlus)[2:ncol(data_SustPlus)], sep = "."))
  #Pivot the dataset to longer format: a row for each year x variable
  melted_CT <- melt(data_CT, "Year")
  melted_Sust <- melt(data_Sust, "Year")
  melted_SustPlus <- melt(data_SustPlus, "Year")
  #Merge all 3 datasets together again
  melted_full <- rbind(melted_CT,
                       melted_Sust,
                       melted_SustPlus)
  #Change the unit from 1000 ha to Mha
  melted_full$value <- melted_full$value/1000
  
  #add back the pathway variable
  melted_full$cat <- ''
  melted_full$cat[grep("CT",melted_full$variable)] <- "CT"
  melted_full$cat[grep("SS",melted_full$variable)] <- "SS"
  melted_full$cat[grep("SSPlus",melted_full$variable)] <- "SSPlus"
  
  #Nice name for legend for each cover type
  melted_full$type <- ''
  melted_full[grep(".CalcForest", melted_full$variable),]$type <- "Forest"
  melted_full[grep(".CalcCropland", melted_full$variable),]$type <- "Cropland"
  melted_full[grep(".CalcPasture", melted_full$variable),]$type <- "Pasture"
  melted_full[grep(".CalcNewForest", melted_full$variable),]$type <- "New Forest"
  melted_full[grep(".CalcOtherLand", melted_full$variable),]$type <- "Other Land"
  melted_full[grep(".CalcUrban", melted_full$variable),]$type <- "Urban"
  #Colour palette for land cover
  myColors_Land <- c(Cropland = "#fed400",
                     Forest = "#24971e",
                     "New Forest" = "#11610d",
                     "Other Land" = "#D3FFBE",
                     Pasture = "#98E600",
                     Urban = "#FF0000")
  #Set linetype for graph
  myLinetype_Protected <- c("Computed protected area" = "solid",
                            "30% protected area" = "dotted")
  #remove NAs
  if (length(which(is.na(melted_full$value)))>0){
    melted_full <- melted_full[-which(is.na(melted_full$value)),]
  }
  
  #Only Land cover type in stacked bar graphs
  melted_bar <- melted_full[which(melted_full$type %in% c("Forest", "Cropland", "Pasture", "New Forest", "Other Land", "Urban")),]
  melted_bar$type <- factor(melted_bar$type, levels = c("Urban", "Cropland", "Pasture", "Other Land", "New Forest", "Forest"))
  #Protected areas in line graphs
  melted_line <- melted_full[grep(".ComputedProtected", melted_full$variable),]
  melted_line_target <- melted_full[grep(".Protected_30", melted_full$variable),]
  

# Graph -------------------------------------------------------------------
  
  p = ggplot(melted_full, aes(x = Year)) 
  
  p_bar <- p + geom_bar(aes(y = value, 
                            fill = type), 
                        stat = 'identity', 
                        position = 'stack', 
                        data = melted_bar) 
  
  if(!(country %in% c("Argentina", "Australia", "Brazil", "China", "Ethiopia", "Indonesia", "Malaysia", "SouthAfrica"))){
    #Do not show protected area values for theses countries because they used the old default value of 17% 
    #p_both = bar + lines
    p_both <- p_bar +
      geom_line(aes(y = value,
                    group = 1,
                    linetype = "Computed protected area"),
                size = 1.4,
                data = melted_line)+
      geom_line(aes(y = value,
                    group = 1,
                    linetype = "30% protected area"),
                size = 1.5,
                data = melted_line_target)
  }else{
    #p_both is only the stached bars
    p_both <- p_bar
  }
  
  
  cat.labs <- c(CT = "Current Trends", 
                SS = "Sustainable", 
                SSPlus = "Sustainable +")
  
  p_final <- p_both +
    facet_grid(cat~.,
               labeller = labeller(cat = cat.labs),
               drop = T, 
               space = "free_x", 
               switch = "x", 
               scale = "free") + 
    scale_fill_manual(values = myColors_Land, 
                      name = "")+
    scale_linetype_manual(values = myLinetype_Protected, 
                          name = NULL,
                          label = c("30% Protected \nArea",
                                    "Computed \nProtected Area"))+
    ylab("Land Surface (Mha)")+
    scale_x_continuous(breaks = seq(2000, 2050, 10),
                       expand = c(0, 0))+
    theme(panel.background = element_rect(fill = 'white'),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          legend.background=element_blank(),
          legend.key = element_rect(fill = NA),
          #You can change the size of the text in the graph here:
          legend.text = element_text(size = 9/0.38),
          strip.text = element_text(size = 10/0.38),#this corresponds to Current Trends, Sustainable, Sustainable +
          legend.title = element_text(size = 10/0.38),
          axis.title.y = element_text(size = 9/0.38),
          axis.text = element_text(size = 7.5/0.38))

  
  outpath <- paste0(outpath, "Figure 1/")
  if(!dir.exists(outpath)){
    dir.create(outpath)
  }
  
  pdf(paste0(outpath, "Figure_1_", country, "_", gsub("-", "",Sys.Date()),".pdf"),  width = 8.75, height = 10)
  plot(p_final)
  dev.off()
  
  png(paste0(outpath, "Figure_1_", country, "_", gsub("-", "",Sys.Date()), ".png"),  width = 8.75, height = 10, units = "in", res = 600)
  plot(p_final)
  dev.off()
}
