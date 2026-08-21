# Algorithm for extracting SOS dates from vegetation index time series produced by GEE script 
# using phenex R package.

# Author: Jakub Skoula
# Date: 13. 3. 2026
# Used packeges:
# WHICKHAM, H. (version 2.0.0): Tidyverse.https://tidyverse.org/. (13. 3. 2026)
# LANGE, M., DOKTOR, D. (2017): Package phenex. Verze 1.4-5. Aktualizováno 9. 5. 2026. https://cran.r-project.org/web/packages/phenex/index.html . (23. 6. 2026)
# WHICKHAM, H., CHANG, W,. HENRY, L., TAKAHASHI, K., WILKE, C., WOO, K., YUTANI, H., DUNNINGTON, D., VAN DEN BRAND, T. (verze 3.5.2) ggplot2. https://ggplot2.tidyverse.org/. (23. 6. 2026)
# forecast
#knihovny:
library(tidyverse);library(phenex);library(ggplot2);library(forecast)

# nahrání dat -------------------------------------------------------------
rm(list = ls())

#data předzpracována pomocí python skriptu (prohození řádků se sloupci a odstranění nepotřebných řádků)
# df <- read.csv('Data/Veget_indexy/habr - frýdlant/habr02/VI_TS_rendvi_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
 df <- read.csv('Data/Veget_indexy/jasan - vranovice/transform_jasan/jasan_mcari_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
# df <- read.csv('Data/Veget_indexy/buk - měděnec/buk_bor/buk_ndvi_CS2.csv', encoding = 'UTF-8', header = F, sep = ',')
#df <- read.csv('Data/Veget_indexy/borovice - Běleč nad orlicí/transform_borovice/borovice_mcari_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
# df <- read.csv('Data/Veget_indexy/smrk - modrava/transform_smrk/smrk_ndvi_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
# df <- read.csv('Data/Veget_indexy/dub_chribska/dub02/VI_TS_ndvi_dub_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
# příprava dat ------------------------------------------------------------

head(df)# pohled na data
colnames(df) <- c('date', 'VI')# pojmenování sloupců 

# sloupec s daty musí být class date, je nutný převod z numeric class na Date
df <- df |> 
  mutate(date = ymd(df$date))

class(df$date)# kontrola class
class(df$VI)# kontrola class

df$year <- format(df$date, "%Y") # přidání sloupce year
df$doy <- yday(df$date)

plot(df$date,df$VI)


df <- df |> 
  filter(VI > 0 & VI < 1) |>  #MTCI 0;8 EVI 0;1 NDVI 0;1
  arrange(date)# sloupec s informací o dni v roci

#strom <- c("habr_2018","habr_2019","habr_2020")
# strom <- c("buk_2018","buk_2019","buk_2020")
#strom <- c("dub_2018","dub_2019","dub_2020","dub_2021","dub_2022","dub_2023","dub_2024")
#strom <- c("borovice_2018","borovice_2019","borovice_2020","borovice_2021","borovice_2022","borovice_2023","borovice_2024")
#strom <- c("smrk_2018","smrk_2019","smrk_2020","smrk_2021","smrk_2022","smrk_2023","smrk_2024")
strom <- c("jasan_2018","jasan_2019","jasan_2020")
#strom <- c("jasan_2018","jasan_2019","jasan_2020","jasan_2021","jasan_2022","jasan_2023","jasan_2024")

plot(df$date,df$VI,type="b", col="black",
     yaxt="n", lty=3, xlab="", ylab="") |> 
    title(main="NDVI Time Series", col.main="red", col.sub="blue",
      xlab="Dates of observation", ylab="RENDVI values",
      col.lab="black", cex.lab=0.75)
#df<-df[-c(18,59),]

#NDMI threshold = 0.25, ostatní 0.5, borovice 0.25

# PHENEX ------------------------------------------------------------------
##
greenup.ndvi.smoothed = data.frame()# příprava tabulky pro greenup smoothed data
Thr = data.frame()
rm(df_filter)

# smoothed data -----------------------------------------------------------
# loop pro smoothed data smoothed
for (smoothed in 1:length(strom)) {

 #filtering data by year  
  df_filter <- df |> 
    filter(format(df$date, "%Y") == 2017 + smoothed )
 # 
 if (leapYears(2017 + smoothed) == TRUE)
 {full_doy <- data.frame(doy = 1:366)}
 else
 {full_doy <- data.frame(doy = 1:365)}

 result <- merge(full_doy, df_filter, by = "doy", all.x = TRUE)
 result$year <- 2017 + smoothed
 result$date <- as.Date(result$doy - 1, origin = paste0(result$year, "-01-01"))
 # df_filter <- df_filter |>
 #  filter(VI <= 0.5)

 # df_filter1 <- df_filter |>
 # mutate(VI = range01(df_filter$VI))
 # df_filter1 <- df_filter |>
 # mutate(VI = df_filter$VI/10000)

 # calculating threshold for function PhenoPhase for each year
 # tb <- df_filter |>
 # filter(VI >= 0)
 # threshold <- (max(no_outliers)-min(no_outliers))
 # 
 # threshold <- (max(tb$VI)-min(tb$VI))
 # 
 # threshold <- min(df_filter$VI)+((max(df_filter$VI)-min(df_filter$VI))*0.5)
 # 
 # threshold_mcari <- (abs(thresh - min(df_filter$VI))/(max(df_filter$VI)-min(df_filter$VI)))
 # Thr = rbind(Thr,threshold)
 #  
  model.ndvi<- modelNDVI(ndvi.values = result$VI, year.int = (2017 + smoothed),
                       multipleSeasons = FALSE, correction = 'ravg', window=7,
                       method = "DLogistic",asym = T,  MARGIN=2, doParallel=FALSE,
                       silent=TRUE)
  
# nutné změnit složku pro ukládání grafů!
 #png(filename = paste0("Data/Veget_indexy/borovice/grafy/borovice_NDVI/plot_",strom[smoothed],".png"), 
  #    units = "px", width = 800, height = 500)
  
  for (ndvi.ob in model.ndvi){ plot(ndvi.ob) }
  title(c("NDVI časová řada DLogistic funkce pro",strom[smoothed]))
  legend("topright",legend=c("Original values","Modelled values","Fitted model"),
         pch=c(1,1,NA),col=c('black','red','blue'),lty=c(NA,NA,1),cex=0.6,
         inset=0.085)
  #dev.off()
  
  output_green <- phenoPhase(model.ndvi[[1]], phase="greenup", method="local", 
                        threshold=0.45, n=1000)

  greenup.ndvi.smoothed = rbind(greenup.ndvi.smoothed, result[output_green[[1]][1],])
  

  print(paste('Done so far:', strom[smoothed], Sys.time(), sep = ' '))
} # konec loopu i
greenup.ndvi.smoothed
# export tabulek
# write.table(greenup.ndvi.smoothed, file = "Data/Veget_indexy/borovice/borovice_greenup/DLogistic/borovice_RENDVI_greenup_DLogistic.txt", sep = '\t', row.names = FALSE, col.names = TRUE)

# vysledky phenex bez prodlouzeni casove rady
greenup.ndvi.smoothed = data.frame()# příprava tabulky pro greenup smoothed data
Thr = data.frame()
rm(df_filter)
# smoothed data -----------------------------------------------------------
# loop pro smoothed data smoothed
for (smoothed in 1:length(strom)) {
  
  # filtering data by year  
  df_filter <- df |> 
    filter(format(df$date, "%Y") == 2017 + smoothed )
  
  model.ndvi<- modelNDVI(ndvi.values = df_filter$VI,year.int = (2017 + smoothed),
                         multipleSeasons = FALSE, correction = 'ravg', window=5,
                         method = "Gauss",asym = T,  MARGIN=2, doParallel=FALSE, 
                         silent=TRUE)
  
  # nutné změnit složku pro ukládání grafů!
  #png(filename = paste0("Data/Veget_indexy/borovice/grafy/borovice_NDVI/plot_",strom[smoothed],".png"), 
  #    units = "px", width = 800, height = 500)
  
  for (ndvi.ob in model.ndvi){ plot(ndvi.ob) }
  title(c("NDVI časová řada Gauss funkce pro",strom[smoothed]))
  legend("topright",legend=c("Original values","Modelled values","Fitted model"),
         pch=c(1,1,NA),col=c('black','red','blue'),lty=c(NA,NA,1),cex=0.6,
         inset=0.085)
  #dev.off()
  
  output_green <- phenoPhase(model.ndvi[[1]], phase="greenup", method="local", 
                             threshold=0.25, n=1000)
  
  greenup.ndvi.smoothed = rbind(greenup.ndvi.smoothed, df_filter[output_green[[1]][1],])
  
  print(paste('Done so far:', strom[smoothed], Sys.time(), sep = ' '))
} # konec loopu i
greenup.ndvi.smoothed
#write.table(greenup.ndvi.smoothed, file = "Data/Veget_indexy/chribska/dub_greenup/Gauss/dub_NDMI_greenup_Gauss.txt", sep = '\t', row.names = FALSE, col.names = TRUE)




