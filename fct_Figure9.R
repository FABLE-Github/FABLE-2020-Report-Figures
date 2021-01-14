#Figure 9 on Self sufficiency

fct_Figure9 <- function(dat, outpath, F9_India, country){
  
  if(country != "India"){
    #Build the Self-Sufficiency ratio dataframe
  data_Demand <- dat %>% 
    group_by(Pathway, 
             Year, 
             PROD_GROUP) %>% 
    #Compute the total internal demand
    mutate(TID = sum(FoodQ, 
                     FeedQ, 
                     BiofuelUseQ, 
                     NonFoodQ, 
                     FoodwasteQ, 
                     ProcQ,
                     ProdLossesQ, 
                     na.rm = T)) %>% 
    #compute total production
    mutate(PROD = sum(ProdQ_feas, StockVar, na.rm = T)) %>% 
    mutate(SelfSufficiency = PROD/TID) %>% 
    select(Pathway, 
           Year, 
           PROD_GROUP, 
           SelfSufficiency) %>% 
    data.frame()
  #One dataframe for each pathway
  data_CT <- data_Demand %>% slice(which(Pathway == "CT"))
  data_Sust <- data_Demand %>% slice(which(Pathway == "Sust"))
  data_SustPlus <- data_Demand %>%  slice(which(Pathway == "SustPlus"))
  
  data_CT$Pathway <- NULL
  data_Sust$Pathway <- NULL
  data_SustPlus$Pathway <- NULL
  #Change columns' name, steep need for the merge later in the script
  colnames(data_CT) <- c("Year", "PROD_GROUP", paste("CT", colnames(data_CT)[3:ncol(data_CT)], sep = "."))
  colnames(data_Sust) <- c("Year", "PROD_GROUP", paste("SS", colnames(data_Sust)[3:ncol(data_Sust)], sep = "."))
  colnames(data_SustPlus) <- c("Year", "PROD_GROUP", paste("SSPlus", colnames(data_SustPlus)[3:ncol(data_SustPlus)], sep = "."))
  #pivot to a longer format
  melted_CT <- melt(data_CT, c("Year", "PROD_GROUP"))
  melted_Sust <- melt(data_Sust, c("Year", "PROD_GROUP"))
  melted_SustPlus <- melt(data_SustPlus, c("Year", "PROD_GROUP"))
  #merge the separate pathways dataframe back together
  melted_full <- rbind(melted_CT,
                       melted_Sust,
                       melted_SustPlus)
  #Add the cat variable to have the pathway information
  melted_full$cat <- ''
  melted_full$cat[grep("CT",melted_full$variable)] <- "CT"
  melted_full$cat[grep("SS",melted_full$variable)] <- "SS"
  melted_full$cat[grep("SSPlus",melted_full$variable)] <- "SSPlus"
  #Renaming of group of products
  melted_full$type <- ''
  melted_full[grep("BEVSPICES", melted_full$PROD_GROUP),]$type  <- "Beverages, Spices and Tobacco"
  melted_full[grep("CEREALS", melted_full$PROD_GROUP),]$type    <- "Cereals"
  melted_full[grep("EGGS", melted_full$PROD_GROUP),]$type       <- "Eggs"
  melted_full[grep("FIBERINDUS", melted_full$PROD_GROUP),]$type <- "Fiber & Indus. Crops"
  melted_full[grep("FRUVEG", melted_full$PROD_GROUP),]$type     <- "Fruits & Veg."
  melted_full[grep("MILK", melted_full$PROD_GROUP),]$type       <- "Milk and Dairy"
  melted_full[grep("NUTS", melted_full$PROD_GROUP),]$type       <- "Nuts"
  melted_full[grep("OLSCAKE", melted_full$PROD_GROUP),]$type    <- "Oil Cakes"
  melted_full[grep("OLSOIL", melted_full$PROD_GROUP),]$type     <- "Oilseeds and Veg. Oils"
  melted_full[grep("PORK", melted_full$PROD_GROUP),]$type       <- "Pork"
  melted_full[grep("POULTRY", melted_full$PROD_GROUP),]$type    <- "Poultry meat"
  melted_full[grep("PULSES", melted_full$PROD_GROUP),]$type     <- "Pulses"
  melted_full[grep("REDMEAT", melted_full$PROD_GROUP),]$type    <- "Beef, Goat & Lamb"
  melted_full[grep("ROOTS", melted_full$PROD_GROUP),]$type      <- "Roots and Tubers"
  melted_full[grep("SUGAR", melted_full$PROD_GROUP),]$type      <- "Sugar and Sugar Crops"
  #remove products without data
  if (length(which(is.na(melted_full$PROD_GROUP)))>0){
    melted_full <- melted_full[-which(is.na(melted_full$PROD_GROUP)),]
  }
  #Don't keep pork, fiberindus and olscake 
  melted_full <- melted_full %>% slice(which(!(melted_full$PROD_GROUP %in% c("PORK", "FIBERINDUS", "OLSCAKE")))) %>% 
    unique()
  #keep only 2010 and 2050
  melted_plot <- melted_full %>% 
    slice(which(Year %in% c("2010", "2050"))) %>% 
    droplevels() %>% 
    mutate(Year = as.factor(Year))
  #remove group of product with not significant internal demand
  filter_crop <- aggregate(value ~ PROD_GROUP, melted_plot, sum)
  filter_crop <- filter_crop$PROD_GROUP[which(filter_crop$value < 0.01)]
  if(length(filter_crop) >0){
    melted_plot <- droplevels(melted_plot[-which(melted_plot$PROD_GROUP %in% filter_crop),])
  }
  
  #Nice products labels for the figure
  prod.labs <- c(BEVSPICES = "Beverages, \nSpices & Tobacco",
                 CEREALS   = "Cereals",
                 EGGS      = "Eggs",
                 FRUVEG    = "Fruits \n& Veg.",
                 MILK      = "Milk & Dairy",
                 NUTS      = "Nuts",
                 OLSOIL    = "Oilseeds \n& Veg. Oils",
                 POULTRY   = "Poultry",
                 PULSES    = "Pulses",
                 REDMEAT   = "Beef, Goat \n& Lamb",
                 ROOTS     = "Roots & \nTubers",
                 SUGAR     = "Sugar & \nSugar Crops")
  
  melted_plot <- melted_plot %>% 
    #rename pathway for CT in 2010 as CT_2010
    slice(which(!(Year == 2010 & cat != "CT"))) %>% 
    mutate(cat   = ifelse(Year == 2010 & cat == "CT", "CT_2010", cat)) %>% 
    mutate(cat   = factor(melted_plot$cat, levels = c("SSPlus", "SS", "CT", "CT_2010"))) %>% 
    mutate(group = PROD_GROUP)
  
  for (cur_row in 1:nrow(melted_plot)){#add values to have a discontinuous effect where the SSR is higher than 2
    if(melted_plot[cur_row, "value"] > 2){
      melted_plot[cur_row, "value"] <- 2
      cur_prod <- melted_plot[cur_row, "PROD_GROUP"]
      melted_plot <- rbind(melted_plot,
                           cbind.data.frame(Year = melted_plot[cur_row, "Year"],
                                            PROD_GROUP = melted_plot[cur_row, "PROD_GROUP"],
                                            variable = melted_plot[cur_row, "variable"],
                                            value = rep(0.01,4),
                                            cat = melted_plot[cur_row, "cat"],
                                            type = melted_plot[cur_row, "type"],
                                            group = c(1,paste0(cur_prod,"X"),2,paste0(cur_prod,"Y"))))
    }
  }
  #colour palette
  myColors_Food <- c(BEVSPICES = "#645974",
                     CEREALS   = "#fbb30a", 
                     EGGS      = "#ffe1a8", 
                     FRUVEG    = "#B6CFAF",
                     MILK      = "#a8bbc5", 
                     NUTS      = "#a44e12", 
                     OLSOIL    = "#FFEC4D",
                     POULTRY   = "#e26d5c", 
                     PULSES    = "#472d30", 
                     REDMEAT   = "#723d46", 
                     ROOTS     = "#8C806F",
                     SUGAR     = "#434955",
                     "1"       = "white", 
                     "2"       = "white")
  }else{#For India only
    
    melted_plot <- F9_India
    #Nice products labels for the figure
    prod.labs <- c(BEVSPICES          = "Beverages, \nSpices & \nTobacco",
                   CEREALS            = "Cereals",
                   EGGS               = "Eggs",
                   FRUVEG             = "Fruits \n& Veg.",
                   FRUVEGNUTS         = "Fruits & \nVeg. & Nuts",
                   MILK               = "Milk & Dairy",
                   NUTS               = "Nuts",
                   OLSOIL             = "Oilseeds \n& Veg. \nOils",
                   OILS               = "Oils",
                   POULTRY            = "Poultry",
                   PULSES             = "Pulses",
                   REDMEAT            = "Beef, Goat \n& Lamb",
                   ROOTS              = "Roots & \nTubers",
                   SUGAR              = "Sugar & \nSugar Crops",
                   Maiz               = "Corn",
                   "Ground Nuts"      = "Ground Nuts",
                   "Monogastric Meat" = "Monogastric Meat",
                   "Oil Crops"        = "Oil Crops",
                   "oilcake"          = "Oil cakes",
                   "Ruminent"         = "Ruminant Meat",
                   "Soya bean"        = "Soya Bean",
                   "TROPICAL ROOTS"   = "Tropical Roots")
    #rename pathway for CT in 2010 as CT_2010
    melted_plot <- melted_plot %>% 
      mutate(cat = ifelse(Year == 2010 & cat == "CT",
                          "CT_2010",
                          cat)) %>% 
      mutate(cat = factor(melted_plot$cat, levels = c("Sust", "CT", "CT_2010"))) %>% 
      mutate(group = PROD_GROUP) %>% 
      data.frame()
    
    for (cur_row in 1:nrow(melted_plot)){#add values to have a discontinuous effect where the SSR is higher than 2
      if(melted_plot[cur_row, "value"] > 2){
        melted_plot[cur_row, "value"] <- 2
        cur_prod <- melted_plot[cur_row, "PROD_GROUP"]
        melted_plot <- rbind(melted_plot,
                             cbind.data.frame(Year = melted_plot[cur_row, "Year"],
                                              PROD_GROUP = melted_plot[cur_row, "PROD_GROUP"],
                                              value = rep(0.01,4),
                                              cat = melted_plot[cur_row, "cat"],
                                              group = c(1,paste0(cur_prod,"X"),2,paste0(cur_prod,"Y"))))
      }
    }
    #colour palette
    myColors_Food <- c(BEVSPICES          = "#645974",
                       CEREALS            = "#fbb30a",
                       EGGS               = "#ffe1a8", 
                       FRUVEG             = "#B6CFAF",
                       FRUVEGNUTS         = "#B6CFAF",
                       MILK               = "#a8bbc5", 
                       NUTS               = "#a44e12", 
                       OLSOIL             = "#FFEC4D", 
                       OILS               = "#FFEC4D",
                       POULTRY            = "#e26d5c", 
                       PULSES             = "#472d30", 
                       REDMEAT            = "#723d46", 
                       "Ruminent"         = "#723d46",
                       ROOTS              = "#8C806F",
                       "TROPICAL ROOTS"   = "#8C806F",
                       SUGAR              = "#434955",
                       "Monogastric Meat" = "brown",
                       "Oil Crops"        = "#F29B80",
                       "oilcake"          = "orange",
                       "Soya bean"        = "#AC8A4D",
                       "Ground Nuts"      = "#8C5230",
                       Maiz               = "#FCE875",
                       "1"                = "white",
                       "2"                = "white")

    
    melted_plot <- melted_plot %>% 
      mutate(PROD_GROUP = factor(as.character(PROD_GROUP), 
                                 levels = c("CEREALS", "Maiz", 
                                            "Soya bean", "PULSES",
                                            "OILS", "Oil Crops", 
                                            "oilcake", "FRUVEGNUTS", 
                                            "Ground Nuts", "MILK", 
                                            "EGGS", "Monogastric Meat",
                                            "Ruminent", "POULTRY",
                                            "SUGAR", "TROPICAL ROOTS")))
    
  }
  
  #Needed to create the discontinued blocks of colours
  myColors_Food2 <- myColors_Food
  names(myColors_Food2) <- paste0(names(myColors_Food), "X")
  myColors_Food3 <- myColors_Food
  names(myColors_Food3) <- paste0(names(myColors_Food), "Y")
  
  myColors_Food <- c(myColors_Food, myColors_Food2, myColors_Food3)
  
  cat.labs <- c(CT      = "Current Trends",
                SS      = "Sustainable",
                SSPlus  = "Sustainable +",
                CT_2010 = "2010")
  
  p = ggplot(melted_plot, aes(x = type)) 
  
  p_bar <- p + geom_col(aes(y = value,
                            x = cat, 
                            fill = group, 
                            alpha = cat, 
                            group = PROD_GROUP),
                        data = melted_plot,
                        position = position_stack(reverse = T),
                        show.legend = F)+
    scale_alpha_manual(values = c(CT = 1,
                                  SS = 1,
                                  SSPlus = 1,
                                  CT_2010 = 0.35))+
    coord_flip()+
    scale_x_discrete(labels = cat.labs)+
    scale_fill_manual(values = myColors_Food, 
                      name = "",
                      labels = prod.labs)+
    scale_y_continuous(limits = c(0,max(aggregate(value ~ Year + PROD_GROUP + cat, melted_plot, sum)$value, 1.05)),
                       breaks =  seq(0, max(aggregate(value ~ Year + PROD_GROUP + cat, melted_plot, sum)$value, 1.05), by = 0.2))
  
  
  
  height_plot = 8.5
  if(length(levels(droplevels(melted_plot$cat)))>3){
    height_plot = 12
  }
  
  p_final <- p_bar +
    facet_grid(PROD_GROUP~.,
               labeller = labeller(PROD_GROUP = prod.labs),
               drop = T, 
               space = "free_x", 
               switch = "y", 
               scale = "free") + 
    geom_hline(yintercept = 1, linetype = "dashed")+
    ylab("Self-Sufficiency Ratio")+
    theme(panel.background = element_rect(fill = 'white'),
          strip.placement = "outside",
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.background=element_blank(),
          legend.key = element_rect(fill = NA),
          legend.text = element_text(size = 8/0.37),
          strip.text = element_text(size = 7/0.37),
          strip.text.y.left = element_text(angle = 0, margin = margin(2, 1, 2, 1, "cm")),
          axis.text = element_text(size = 6/0.37),
          axis.title.x = element_text(size = 7/0.37))
  
  
  outpath <- paste0(outpath, "Figure 9/")
  if(!dir.exists(outpath)){
    dir.create(outpath)
  }
  
  
  
  pdf(paste0(outpath, "Figure_9_", country, "_", gsub("-", "",Sys.Date()),".pdf"), bg = "white", height = height_plot, width = 15)
  plot(p_final)
  dev.off()
  
  png(paste0(outpath, "Figure_9_", country, "_", gsub("-", "",Sys.Date()),".png"), bg = "white", height = height_plot, width = 15, unit = "in", res = 600)
  plot(p_final)
  dev.off()
}
