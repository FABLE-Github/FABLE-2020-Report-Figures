#Diversification
#Use HHI to estimate trade concentration and Production concentration

fct_Figure10 <- function(dat, outpath, country){
  HHI_data <- dat %>%
    group_by(Pathway, Year) %>% 
    #Compute the share that each products represents for each products for Imports, exports and planted area
    mutate(Share_Import = 100*Import_quantity/sum(Import_quantity)) %>% 
    mutate(Share_Export = 100*Export_quantity/sum(Export_quantity)) %>% 
    mutate(Share_Planted = ifelse(name!= "India", 100*FeasPlantarea/sum(FeasPlantarea),
                              100*FeasHarvarea/sum(FeasHarvarea))) %>% 
    #Compute the HHI for exports, imports and planted area
    mutate(HHI_Export = sum(Share_Export^2, na.rm = T)) %>%
    mutate(HHI_Import = sum(Share_Import^2, na.rm = T)) %>%
    mutate(HHI_Planted = sum(Share_Planted^2, na.rm = T)) %>%
    select(Year, Pathway, HHI_Import, HHI_Export, HHI_Planted) %>% 
    unique() %>% 
    data.frame()
  
  #create 3 seperate dataframe for the 3 (or 2) pathways
  data_CT <- HHI_data[which(HHI_data$Pathway == "CT"),]
  data_Sust <- HHI_data[which(HHI_data$Pathway == "Sust"),]
  data_SustPlus <- HHI_data[which(HHI_data$Pathway == "SustPlus"),]
  #remove the variable pathway
  data_CT$Pathway <- NULL
  data_Sust$Pathway <- NULL
  data_SustPlus$Pathway <- NULL
  #rename the columns
  colnames(data_CT) <- c("Year", paste("CT", colnames(data_CT)[2:ncol(data_CT)], sep = "."))
  colnames(data_Sust) <- c("Year", paste("SS", colnames(data_Sust)[2:ncol(data_Sust)], sep = "."))
  colnames(data_SustPlus) <- c("Year", paste("SSPlus", colnames(data_SustPlus)[2:ncol(data_SustPlus)], sep = "."))
  #pivot the dataframes to a longer format
  melted_CT <- melt(data_CT, c("Year"))
  melted_Sust <- melt(data_Sust, c("Year"))
  melted_SustPlus <- melt(data_SustPlus, c("Year"))
  #Merge all the dataframes together
  melted_full <- rbind(melted_CT,
                       melted_Sust,
                       melted_SustPlus)
  #Create a cat variable for the pathway
  melted_full$cat <- ''
  melted_full$cat[grep("CT",melted_full$variable)] <- "CT"
  melted_full$cat[grep("SS",melted_full$variable)] <- "SS"
  melted_full$cat[grep("SSPlus",melted_full$variable)] <- "SSPlus"
  #Nice names of variables
  melted_full$Variable <- ''
  melted_full$Variable[grep("Import",melted_full$variable)] <- "Imports"
  melted_full$Variable[grep("Export",melted_full$variable)] <- "Exports"
  melted_full$Variable[grep("Planted",melted_full$variable)] <- "Cultivated Area"
  #define the linetype we want in the plot for each pathway
  mylinetype_GHG <- c(CT = "solid",
                      SS = "dotted",
                      SSPlus = "dashed")
  #Canada wanted its plot to be zoomed if on HHI ranging from 0 to 4000
  max_value <- ifelse(country == "Canada", 4000, 10000)
  
  p = ggplot(melted_full, aes(x = Year)) 
  #ploting the lines
  p_bar <- p +
    geom_line(aes(y = value, linetype = cat, size = cat),
              data = melted_full)+ 
    scale_linetype_manual(values = mylinetype_GHG,
                          name = "",
                          labels = c(CT = "Current Trends",
                                     SS = "Sustainable",
                                     SSPlus = "Sustainable +"))+
    scale_size_manual(values = c(CT = 1.2,
                                 SS = 1.7,
                                 SSPlus = 1.5),
                      name = "",
                      labels = c(CT = "Current Trends",
                                 SS = "Sustainable",
                                 SSPlus = "Sustainable +"))+
    guides(size = NULL)+
    ylim(0, max_value)
  
  
  
  # Horizontal --------------------------------------------------------------
  
  p_final <- p_bar +
    #dividing the line plot into three facets for exports, imports and cultivated area
    facet_grid(.~Variable,
               drop = T, 
               space = "free_x", 
               switch = "y", 
               scale = "free") + 
    #adding the greed, orange, and red rectangle delimiting the 3 categories for the HHI
    geom_rect(aes(xmin = 2000, xmax = 2050,
                  ymin = 0, ymax = 1500,
                  fill = "Unconcentrated"), colour = NA, alpha = 0.01)+
    geom_rect(aes(xmin = 2000, xmax = 2050, 
                  ymin = 1500, ymax = 2500,
                  fill = "Moderately Concentrated"), colour = NA, alpha = 0.01)+
    geom_rect(aes(xmin = 2000, xmax = 2050, 
                  ymin = 2500, ymax = max_value,
                  fill = "Concentrated"), colour = NA, alpha = 0.01)+
    scale_fill_manual(name = "",
                      values = c( "Unconcentrated" = "#a9f2a0",
                                  "Moderately Concentrated" = "#f7ce65",
                                  "Concentrated" = "#f77465"),  
                      guide = guide_legend(override.aes = list(alpha = 0.13)))+
    ylab("HHI")+
    scale_x_continuous(breaks = seq(2000, 2050, 10),
                       expand = c(0, 0))+
    theme(panel.background = element_rect(fill = 'white'),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          legend.background=element_blank(),
          legend.key = element_rect(fill = NA),
          legend.key.width = unit(14, "mm"),
          legend.text = element_text(size = 9/0.51),
          strip.text = element_text(size = 9/0.51),
          axis.text = element_text(size = 6.5/0.51),
          axis.title.y = element_text(size = 8/0.51),
          legend.position = "bottom",
          legend.box="vertical",
          panel.spacing.x = unit(15, "mm"),
          plot.margin = margin(l = 0.3, t =  0.3, r = 1.5, unit = "lines"))
  
  outpath <- paste0(outpath, "Figure 10/")
  if(!dir.exists(outpath)){
    dir.create(outpath)
  }
  
  pdf(paste0(outpath, "Figure_10_", country, "_", gsub("-", "",Sys.Date()),".pdf"),  height = 6, width = 10)
  plot(p_final)
  dev.off()
  
  png(paste0(outpath, "Figure_10_", country, "_", gsub("-", "",Sys.Date()),".png"),  height = 6, width = 10, unit = "in", res = 600)
  plot(p_final)
  dev.off()
}
