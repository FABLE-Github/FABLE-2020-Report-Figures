# FABLE-2020-Report-Figures
FABLE 2020 Report Figures

Instructions:

(1) These scripts run in R, a general use coding language and environment. If you are unfamiliar with R, start here (https://cran.r-project.org/) and install both R and R-Studio (https://www.rstudio.com/products/rstudio/download/) on your computer. Both are free.

(2) Copy the zipped folder "FABLE 2020 Report Figures.zip" and extract its contents into the directory of your choice, here called "\~/yourpath/" for convenience. (If you are using Windows, this will look like "C://\~/path/"; and if you're using a Mac, "User//\~/path/".) The structure of the folder inside this .zip is the one used in the Master.R script.

(3) Copy the scripts "Master.R", "fct_Figure1.R", "fct_Figure2.R", "fct_Figure3.R", "fct_Figure4.R", "fct_Figure5.R", "fct_Figure6.R", 
"fct_Figure7.R", "fct_Figure7_PieDonutFABLE.R", "fct_Figure8.R", "fct_Figure9.R", "fct_Figure10.R" from GitHub to "~/yourpath/FABLE 2020 Report Figures/Programs/" on your computer.

(4) Open the script "//\~/yourpath/Programs/Master.R" in R-Studio and change:
"path" to whatever you use for "\~/yourpath/". E.g., 
change the following
path <- "C:/Users/Clara Douzal/OneDrive - SDSN Association Paris/FABLE/FABLE 2020 Report Figures/" #edit this yourself
to
path <- "C:/\~/yourpath/FABLE 2020 Report Figures/" #edit this yourself. Make sure it ends with "FABLE 2020 Report Figures/", the name of the folder with the proper structure.

(5) Line 13 of Master.R, copy/paste between the quote marks your country name ,from the list line 7 to 11.
 
(6) Press Ctrl + Maj + Entr to run the entire Master.R script.

(7) From lines 151 to 169 you can add "#" before the fct_Function that you do not wish to run

(8) Your figures are stored automatically in individual figures folders in "\~/yourpath/Outputs/" and the date automatically added to the file's name.
