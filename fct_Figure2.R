# Figure 2:  area where natural processes predominate over time
# Author: Clara Douzal (SDSN)
# Last update: 20201209

fct_Figure2 <- function(data, outpath, country){
  
  #Compute Area where natural processes predominate in Mha
  data <- data %>% 
    mutate(NPP = CalcBiodivLnd * TotalLand /1000) %>% 
    mutate(TotalLand = TotalLand/1000) %>% 
    slice(which(Year %in% seq(2010, 2050, 5)))
  
  #Will be needed to adjust y-axis in graph
  PercentageTotalLand <- 100/unique(data$TotalLand)
  #Min & max used to play with the y-axis
  min <- min(data[which(data$Year %in% seq(2010, 2050, 5)), "NPP"])
  max <- max(data[which(data$Year %in% seq(2010, 2050, 5)), "NPP"])
  
  #linetype used in the graph 
  myLinetype <- c("CT" = "solid",
                  "Sust" = "solid",
                  "SustPlus" = "solid")
  
  
  if(unique(data$country_id) == 31){#Norway
    myLinetype = c("CT" = "solid",
                   "Sust" = "dashed")
  }
  
  p <- ggplot(data, aes(Year, NPP))+
    geom_line(data = data, 
              aes(Year, 
                  NPP, 
                  color = Pathway, 
                  linetype = Pathway),
              size = 2.5)+
    scale_color_manual(values = myColors_scen_water,#this is defined in the Master script
                       labels = c(CT = "Current Trends",
                                  Sust = "Sustainable",
                                  SustPlus = "Sustainable +"))+
    scale_linetype_manual(values = myLinetype)+
    guides(linetype = FALSE) +
    scale_y_continuous(limits = if((max - min)/min < 0.1)
      c(0, 1.05*max) else 
        c(0.9*min, 1.05*max),
      sec.axis = sec_axis(~.*PercentageTotalLand,
                          name = "% of Total Land"),
      expand = c(0,0))+
    xlab(NULL)+
    ylab("Surface (Mha)")+
    theme_minimal()+
    
    scale_x_continuous(breaks = seq(2000, 2050, 5))+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(1.5, "cm"),
          #you can change the size of the text in the figure here:
          legend.text = element_text(size = 9/0.39),
          axis.text = element_text(size = 8/0.39),
          axis.title.y = element_text(size = 9/0.39),
          axis.ticks.y = element_line(),
          axis.line.y.right = element_line(colour = "lightgrey"))
  
  
  outpath <- paste0(outpath, "Figure 2/")
  if(!dir.exists(outpath)){
    dir.create(outpath)
  }
  
  pdf(paste0(outpath, "Figure_2_", country, "_", gsub("-", "",Sys.Date()),".pdf"),  width = 8.75, height = 8.5)
  plot(p)
  dev.off()
  
  png(paste0(outpath, "Figure_2_", country, "_", gsub("-", "",Sys.Date()), ".png"),  width = 8.75, height = 8.5, units = "in", res = 600)
  plot(p)
  dev.off()
  
}




