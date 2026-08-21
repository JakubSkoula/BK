
library(tidyverse);library(ggplot2)
rm(list = ls())
# nahrání dat -------------------------------------------------------------


#data předzpracována pomocí python skriptu (prohození řádků se sloupci a odstranění nepotřebných řádků)
df <- read.csv('Data/Veget_indexy/jasan/transform_jasan/jasan_NDVI_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
df <- read.csv('Data/Veget_indexy/habr/transform_habr/habr_mcari_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
#df <- read.csv('Data/Veget_indexy/buk/buk_bor/buk_mcari_CS5.csv', encoding = 'UTF-8', header = F, sep = ',')
df <- read.csv('Data/Veget_indexy/chribska/transform_dub/dub_mcari_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
df <- read.csv('Data/Veget_indexy/borovice - Běleč nad Orlicí/transform_bor/borovice_mcari_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
df <- read.csv('Data/Veget_indexy/modrava/transform_smrk/smrk_ndvi_CS.csv', encoding = 'UTF-8', header = F, sep = ',')
# příprava dat ------------------------------------------------------------

head(df)# pohled na data
colnames(df) <- c('date', 'VI')# pojmenování sloupců 

# sloupec s daty musí být class date, je nutný převod z numeric class na Date
df <- df |> 
  mutate(date = ymd(df$date))

class(df$date)# kontrola class
class(df$VI)# kontrola class

#df$year <- format(df$date, "%Y") # přidání sloupce year
df$doy <- yday(df$date) # sloupec s informací o dni v roci
plot(df$date,df$VI)

# odstraneni zaznamu mimo meze VI 
df <- df |> 
  filter(VI > 0 & VI < 1) |>  #MTCI 0;8 EVI 0;1 NDVI 0;1
  arrange(date)# seradit podle data

#df<- df[-c(62,63),] # habr
# Plot
plot(df$date,df$VI,type="b", col="black",
     yaxt="n", lty=3, xlab="", ylab="")
title(main="NDVI Time Series", col.main="red", col.sub="blue",
      xlab="Dates of observation", ylab="NDVI values",
      col.lab="black", cex.lab=0.75)


##################### Test methods

##### Package phenopix
library(phenopix);library(phenex); 
SOS =  data.frame()
#transforming VI time series to an object of class 'time-series'

#strom <- c("smrk_2018","smrk_2019","smrk_2020","smrk_2021","smrk_2022","smrk_2023","smrk_2024")
#strom <- c("jasan_2018","jasan_2019","jasan_2020")
#strom <- c("habr_2018","habr_2019","habr_2020")
#strom <- c("buk_2018","buk_2019","buk_2020","buk_2021","buk_2022","buk_2023","buk_2024")
strom <- c("dub_2018","dub_2019","dub_2020")
strom <- c("borovice_2018","borovice_2019","borovice_2020")


for (i in 1:length(strom)) {
  result <- df |> 
    filter(format(date,"%Y") == 2017 + i)

  # if (leapYears(2017 + i) == TRUE)
  # {full_doy <- data.frame(doy = 1:366)}
  # else
  # {full_doy <- data.frame(doy = 1:365)}
  # 
  # result <- merge(full_doy, df_filter, by = "doy", all.x = TRUE)
  # result$VI[is.na(result$VI)] <- NA
  # 
  # if (2017+i == 2018)
  # {
  #   result <- result |>
  #     mutate(date = as.Date(result$doy, origin = "2017-12-31"))
  # }
  # if(2017+i == 2019)
  # {
  #   result <- result |>
  #     mutate(date = as.Date(result$doy, origin = "2018-12-31"))
  # }
  # if(2017+i == 2020)
  # {
  #   result <- result |>
  #     mutate(date = as.Date(result$doy, origin = "2019-12-31"))
  # }
  
 
  # result$VI <-  na.approx(result$VI, na.rm = FALSE)
  # result$VI <- na.locf(result$VI, na.rm = FALSE)
  # result$VI <- na.locf(result$VI, fromLast = TRUE)
  
#  gam_fit <- gam(result$VI ~ s(date, k = 10))
  
  lst <- ts(result$VI)

  ##function fits a DL curve to observed values using the function as described in Beck et al. (2006)
  beckfit <- BeckFit(lst)
  
  b <- PhenoExtract(beckfit, method = "derivatives",
                    uncert = FALSE, envelope = "quantiles",
                    quantiles = c(0.05, 0.95), plot = T)
  
  print(result[b[[1]][1],])
  SOS = rbind(SOS, result[b[[1]][1],])
  print(paste('Done so far:', strom[i], Sys.time(), sep = ' '))
}
SOS
#detecting SOS and EOS on raw data by Computation of breakpoints in regression relationships
p <-PhenoBP(lst, breaks=2, confidence=0.95, plot=TRUE)
lst



library(zoo)
data(bartlett2009.filtered)
bartlett2009.filtered
## fit without uncertainty estimation
fitted.beck <- BeckFit(bartlett2009.filtered)
days <- as.numeric(format(index(bartlett2009.filtered)))
plot(days, bartlett2009.filtered)
lines(fitted.beck$fit$predicted, col='red')
## look at fitting parameters
fitted.beck$fit$params
## look at fitting equation, where t is time
fitted.beck$fit$formula

