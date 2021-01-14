fct_Figure7 <- function(df, outpath, country){
  
  #change the unit
  df$Value <- 1000*df$Value
  #Call function PieDonut_FABLE
  p <- PieDonut_FABLE(df,aes(Sector,count=Value),r0=0.7,explode=1,start=0, 
                      showPieName = T, showRatioPie = T, labelposition = 1)
  
  outpath <- paste0(outpath, "Figure 7/")
  if(!dir.exists(outpath)){
    dir.create(outpath)
  }
  
  pdf(paste0(outpath, "Figure_7_", country, "_", gsub("-", "",Sys.Date()),".pdf"), bg = "white", height = 8.5, width = 8.5)
  plot(p)
  dev.off()
  
  png(paste0(outpath, "Figure_7_", country, "_", gsub("-", "",Sys.Date()),".png"), bg = "white", height = 8.5, width = 8.5, unit = "in", res = 600)
  plot(p)
  dev.off()
}

