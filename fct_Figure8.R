fct_Figure8 <- function(data, outpath, country){
  
  df <- data %>% 
    select(Pathway, Year, CalcWFblue)
  #All countries but China and India (both major blue water users) have the graph in Mm3
  lab_title <- expression("Blue Water Footprint (Mm"^"3"*")")
  
  if(unique(data$ALPHA3 %in% c("CHN", "IND"))){
    df <- df %>% 
      mutate(CalcWFblue = CalcWFblue/1000)
    lab_title <- expression("Blue Water Footprint (km"^"3"*")")
  }
  min <- min(df[which(df$Year == 2050), "CalcWFblue"])
  max <- max(df[which(df$Year == 2050), "CalcWFblue"])
  
  min_axe <-min(df[, "CalcWFblue"])
  max_axe <- max(df[, "CalcWFblue"])
  df_hist <- df %>% slice(which(Pathway == "CT" & Year %in% seq(2000, 2010, 5)))
  df <- df %>% 
    slice(which(Year %in% seq(2010, 2050, 5))) %>% 
    rbind(df_hist)
  
  
  
  p <- ggplot(df, aes(x = Year))+
    geom_line(aes(y = CalcWFblue, colour = Pathway), size = 1.8)+
    scale_y_continuous(limits = if((max - min)/min < 0.1)
      c(max(0.8*min_axe, 0), max_axe + 0.2*min_axe) else 
        c(min_axe, max_axe))+
    scale_color_manual(values = myColors_scen_water, 
                       name = NULL,
                       labels = c(CT = "Current Trends",
                                  Sust = "Sustainable",
                                  SustPlus = "Sustainable +"))+
    theme_minimal()+
    xlab(NULL)+
    scale_x_continuous(breaks = seq(2000,2050,5))+
    ylab(lab_title)+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.text = element_text(size = 8/0.3),
          axis.title = element_text(size = 8/0.3),
          axis.text = element_text(size = 7/0.3),
          legend.key.width = unit(0.75, "cm"),
          axis.ticks = element_line(),
          legend.position = "bottom")
  
  outpath <- paste0(outpath, "Figure 8/")
  if(!dir.exists(outpath)){
    dir.create(outpath)
  }
  
  pdf(paste0(outpath, "Figure_8_", country, "_", gsub("-", "",Sys.Date()),".pdf"), bg = "white", height = 8.5, width = 11)
  plot(p)
  dev.off()
  
  png(paste0(outpath, "Figure_8_", country, "_", gsub("-", "",Sys.Date()),".png"), bg = "white", height = 8.5, width = 11, unit = "in", res = 600)
  plot(p)
  dev.off()
  
}