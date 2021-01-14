#Comparison of cumulated projected GHG emissions reduction over 
#2020-2050 by AFOLU type compared to the current trends pathway 

# include crops, livestock, peat (Indonesia, Malaysia), deforestation, 
# other land use change, sequestration, and biofuels.

fct_Figure5 <- function(data, outpath, country){
  
  #Keep only necessary variables
  dat <- select(data, Year, Pathway, CalcCropAllCO2e, CalcLiveAllCO2e, CalcPeatCO2,
                CalcDeforCO2, CalcOtherLUCCO2, CalcSequestCO2,
                GHGbiofuels)
  
  #Pivot the dataframe to a longer format 
  dat_long <- data.frame(pivot_longer(data = dat, 
                                      cols = c("CalcCropAllCO2e", 
                                               "CalcLiveAllCO2e", 
                                               "CalcPeatCO2", 
                                               "CalcDeforCO2",
                                               "CalcOtherLUCCO2",
                                               "CalcSequestCO2",
                                               "GHGbiofuels"),
                                      names_to = "AFOLU_type", 
                                      values_to = "Emissions"))
  #Keep only Sustainable pathways values to later compare with CT values
  data_compare <- dat_long %>% 
    slice(which(Pathway %in% c("Sust", "SustPlus")))
  
  #Keep only CT values
  data_CT <- dat_long %>% 
    slice(which(Pathway == "CT"))
  data_CT <- rename(data_CT, Emissions_CT = Emissions)
  data_CT$Pathway <- NULL
  
  #Compare emissions from AFOLU in Sustainable pathways and CT pathway for each 5-year time step from 2020 to 2050
  data_compare <- data_compare %>% 
    left_join(data_CT, by = NULL) %>% 
    mutate(Reduction = (Emissions - Emissions_CT)) %>% ## !! Here it should be: mutate(Reduction = 5*(Emissions - Emissions_CT)) %>% 
                                                       ## The mistake is in the figures that are in the country chapters and the full report 
                                                       ## You are advised to use the correct line of code if you were to generate new figures
    slice(which(Year %in% seq(2020, 2050, 5)))
  #Get the total difference between pathways over the 2020 to 2050 period
  data_final <- data.frame(aggregate(Reduction ~ AFOLU_type + Pathway, data_compare, sum))
  
  
  p <- ggplot(data = data_final, aes(x= 1))+
    geom_bar(aes( y = Reduction,
                  fill = AFOLU_type),
             stat = "identity",
             position = "stack",
             width = 0.5)+
    scale_y_reverse(breaks = scales::pretty_breaks(n = 10))+
    xlab(NULL)
  
  scenario.labs <- c(CT = "Current Trends", 
                     Sust = "Sustainable", 
                     SustPlus = "Sustainable +")
  
  p_plot <- p+
    facet_grid(.~Pathway,
               labeller = labeller(Pathway = scenario.labs),
               drop = T, 
               space = "free_x", 
               switch = "y", 
               scale = "free") +
    scale_fill_manual(values = myColorsCO2AFOLU, 
                      name = "AFOLU GHG \nSources and \nSinks", 
                      labels = c("CalcLiveAllCO2e" = "Livestock",
                                 "CalcDeforCO2" = "Deforestation",
                                 "CalcOtherLUCCO2" = "Other Land Use",
                                 "CalcCropAllCO2e" = "Crops",
                                 "CalcSequestCO2" = "Sequestration (Sink)",
                                 "CalcPeatCO2" = "Peat",
                                 "GHGbiofuels" = "GHG Biofuels (Sink)"))+
    ylab(expression("Absolute Change in CO"[2]*" Removals and Emissions (MtCO"[2]*"e)"))+
    geom_hline(yintercept=0, 
               color = "black", size=2)+
    theme(panel.background = element_rect(fill = 'white'),
          strip.placement = "outside",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 7/0.46),
          axis.title = element_text(size = 7.5/0.46),
          legend.title = element_text(size = 8/0.46),
          legend.text = element_text(size = 7.5/0.46),
          strip.text = element_text(size = 8.5/0.46))
  
  outpath <- paste0(outpath, "Figure 5/")
  if(!dir.exists(outpath)){
    dir.create(outpath)
  }
  
  width_plot = 5.75
  if(length(levels(data_final$Pathway))>2){
    width_plot = 7.1
  }
  
  pdf(paste0(outpath, "Figure_5_", country, "_", gsub("-", "",Sys.Date()),".pdf"), bg = "white", height = 7, width = width_plot)
  plot(p_plot)
  dev.off()
  
  png(paste0(outpath, "Figure_5_", country, "_", gsub("-", "",Sys.Date()),".png"), bg = "white", height = 7, width = width_plot, unit = "in", res = 600)
  plot(p_plot)
  dev.off()
}
