# Script fo LSP detection using phenofit package
# Author: Jakub Skoula
# Date 30. 07. 2026
# Citations:

# WHICKHAM, H. (version 2.0.0): Tidyverse.https://tidyverse.org/. (13. 3. 2026)
# LANGE, M., DOKTOR, D. (2017): Package phenex. Verze 1.4-5. Aktualizováno 9. 5. 2026. https://cran.r-project.org/web/packages/phenex/index.html . (23. 6. 2026)
# KONG, D., XIAO, M., ZHANG, Y., GU, X., CUI, J. (2026): phenofit : Extract Remote Sensing Vegetation Phenology. Verze 0. 3. 11.  https://CRAN.R-project.org/package=phenofit. 

# install and load R packages
#install.packages("phenofit")
library(phenofit); library(phenex); library(tidyverse)

rm(list = ls())

# load VI data ------------------------------------------------------------

# df <- read.csv("Data/Veget_indexy/jasan/transform_jasan/jasan_mcari_CS.csv", encoding = "UTF-8", header = F, sep = ",")
 df <- read.csv("Data/Veget_indexy/habr/transform_habr/habr_mcari_CS.csv", encoding = "UTF-8", header = F, sep = ",")
 df <- read.csv("Data/Veget_indexy/buk/buk_bor/buk_mcari_CS.csv", encoding = "UTF-8", header = F, sep = ",")
 df <- read.csv("C:/Users/skoul/Documents/R_projects/BK/Data/Veget_indexy/dub - chribska/transform_dub/dub_mcari_CS.csv", encoding = "UTF-8", header = F, sep = ",")
 df <- read.csv("Data/Veget_indexy/borovice - Běleč nad Orlicí/transform_bor/borovice_ndvi_CS.csv", encoding = "UTF-8", header = F, sep = ",")
# df <- read.csv("Data/Veget_indexy/modrava/transform_smrk/smrk_ndvi_CS.csv", encoding = "UTF-8", header = F, sep = ",")


# Data pre-processing -----------------------------------------------------

head(df) #

colnames(df) <- c("date", "VI")# naming columns

# Date as type date
if (class(df$date) != "Date"){
   df <- df|> 
    mutate(date = lubridate::ymd(df$date))
}

# day to doy (day of year)
df$doy <- yday(df$date)

roky<- c("2018","2019","2020")
rows <- list()


#loop
for (i in 1:length(roky)) {
  df_filter<- df |>
    filter(format(date,"%Y") == 2017 + i)


  if (phenex::leapYears(2017+i) == TRUE){
    full_doy <- data.frame(doy = 1:366)
    } else {
      full_doy <- data.frame(doy = 1:365)
      }
  # 
  result <- merge(full_doy, df_filter, by = "doy", all.x = TRUE)
  
  result$date <- as.Date(result$doy - 1, origin = paste0(2017+i,"-01-01"))
  
  result$VI[is.na(result$VI)] <- NA
  
  head(result)
  
  nptperyear = nrow(df_filter)
  input<-check_input(t = df_filter$date, y = df_filter$VI,
                     na.rm=F,
                     nptperyear = nptperyear,
                     maxgap = 50,
                     wmin = 0.1,
                     missval = min(df_filter$VI),
                     ymin = 0.01,
                     )

  plot_input(input)
  # curve fitting by year
  brks_mov <- season_mov(input,
                         options = list(
                           rFUN = "smooth_wHANTS", wFUN = "wTSM",
                           nf = 3,
                           r_min = 0.01, ypeak_min = 0.01,
                           verbose = TRUE,
                           iters = 3


                         )
  )
  brks_mov <- season_mov(
    input,
    options = list(
      rFUN = "smooth_wWHIT",
      wFUN = "wTSM",
      nf = 4,
      lambda = 20,
      r_min = 0.01,
      ypeak_min = 0.01,
      verbose = TRUE,
      len_min = 200,
      iters = 2
    )
  )
  
  plot_season(input, brks_mov)
  
  
  
  rfit <- brks2rfit(brks_mov)
  # fine curve fitting
  fits <- curvefits(
    input, brks_mov,
    options = list(
      methods = c("AG", "Beck", "Elmore", "Zhang"), #other options: "klos", "Gu"
      wFUN = "wTSM",
      use.rough = F,
      nextend = 2, maxExtendMonth = 2, minExtendMonth = 1, minPercValid = 0.2
    )
  )
  
  # Phenological Metrics from fitting
  r <- get_pheno(fits,TRS = c(0.2, 0.5), asymmetric = T, IsPlot = T, show.title = T)
  t <- get_pheno(rfit,TRS = c(0.2, 0.5), asymmetric = T, IsPlot = T, show.title = T)
  

  rows[[i]] <- data.frame(
    rok = 2017 + i,
    Z_TRS5 = r[["doy"]][["Beck"]][["TRS5.sos"]][1],
    Z_TRS2 = r[["doy"]][["Beck"]][["TRS2.sos"]][1],
    Z_DER  = r[["doy"]][["Beck"]][["DER.sos"]][1],
    R_TRS5 = t[["doy"]][["TRS5.sos"]][1],
    R_TRS2 = t[["doy"]][["TRS2.sos"]][1],
    R_DER  = t[["doy"]][["DER.sos"]][1],
    N_px = nrow(df_filter)
  )

}# end of loop

data_F <- do.call(rbind, rows)
data_T <- do.call(rbind, rows)
data_T
data_F

write.table(data, file = "Data/Phenofit/dub/SOS_ndmi.csv", sep = "\t", row.names = FALSE, col.names = TRUE)
  