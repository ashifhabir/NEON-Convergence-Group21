library(tidyverse)
library(neonUtilities)


Desktop.path <- setwd("C:/Ashif/OneDrive - USNH/Research Stuffs/")

#***************** Downloading Stream Water Quality Data from NEON ***********
#************ NEON Stream Water Quality [pH, fDOM] ***********
#************ Data Downloaded [09/05/2025] ****************
#*
aqsites = c(
  "ARIK", "BARC", "BIGC", "BLDE", "BLUE", "BLWA",
  "CARI", "COMO", "CRAM", "CUPE", "FLNT", "GUIL",
  "HOPB", "KING", "LECO", "LEWI", "LIRO", "MART",
  "MAYF", "MCDI", "MCRA", "OKSR", "POSE",
  "PRIN", "PRLA", "PRPO", "REDB", "SUGG", "SYCA", "TECR", "TOMB",
  "TOOK", "WALK", "WLOU"
)

aqsites <- str_sort(aqsites)

dataID.waterqual = "DP1.20288.001"   
tkn <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJhc2hpZmhhc2FuLmFiaXJAdW5oLmVkdSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTgyMDIyNjQ2NSwiaWF0IjoxNjYyNTQ2NDY1LCJlbWFpbCI6ImFzaGlmaGFzYW4uYWJpckB1bmguZWR1In0.g7wvrb9QmJPG9tIUvnVHjLjIem-8NqFkZ4NEQXYG-SxlB5bUFWecc8PN3KsJ47bkQGt-JsFmhMDgPzK8JQFN4g"
for (i in 1:length(aqsites)) {
  # for (i in 1) {
  
  siteID.waterqual <- aqsites[i]
  waterqual <- loadByProduct(dpID=dataID.waterqual, 
                             site=siteID.waterqual,
                             
                             package="basic",
                             nCores=6,
                             token = tkn,
                             release = "RELEASE-2025",
                             check.size=F)
  write.csv(
    waterqual$waq_instantaneous,
    paste0("MSB_NEON/RawData/stream.water.quality/",
           "str.qual.",siteID.waterqual,
           ".csv"),
    row.names = FALSE)
  
  rm(siteID.waterqual)
  rm(waterqual)
  
}


#*************************** Stream fDOM Aggregating to Daily and Annual Averages ****************
aqsites = c(
  "ARIK", "BARC", "BIGC", "BLDE", "BLUE",
  "BLWA", "CARI", "COMO", "CRAM", "CUPE", 
  "FLNT", "GUIL",
  "HOPB", "KING", "LECO", "LEWI", "LIRO", "MART",
  "MAYF", "MCDI", "MCRA", "OKSR", "POSE",
  "PRIN", "PRLA", "PRPO", "REDB", 
  "SUGG", "SYCA", "TECR", "TOMB", "TOOK", "WALK", "WLOU"
)

aqsites <- str_sort(aqsites)


# for (k in 9) {

for (k in 1:length(aqsites)) {
  print(k)
  mysite <- aqsites[k]
  
  
  
  filename.str.tempC <- list.files(path = "MSB_NEON/RawData/stream.water.quality/",  
                                   pattern = mysite,
                                   full.names = TRUE)
  
  str.fdom.main <- read_csv(filename.str.tempC,
                            col_select = c(siteID, endDateTime, fDOM,
                                           horizontalPosition, verticalPosition, fDOMFinalQF))
  
  str.fdom.grouped <- str.fdom.main %>% 
    filter(fDOMFinalQF == "0") %>% 
    mutate(day = as.Date(endDateTime, format = "%Y-%m-%d")) %>% 
    mutate(year = year(day), month = month(day), dayofmonth = day(day)) %>% 
    pivot_wider(
      names_from = c(horizontalPosition, verticalPosition),
      values_from = c(fDOM),
      names_prefix = "str.fdom.",
      names_sep = ".")  %>%  
    group_by(day) 
  
  
  
  str.fdom.daily <- str.fdom.grouped %>%
    summarise(
      across(
        .cols = starts_with("str.fdom."),
        
        .fns = ~mean(.x, na.rm = TRUE),
        .names = "{.col}"
      )) %>%
    mutate(siteID = mysite) %>% 
    # complete(day = seq.Date(min(as.Date(soil.temp.reformed$day)), max(as.Date(soil.temp.reformed$day)), by = "day")) %>% 
    relocate(any_of(c("siteID", "year", "month"))) %>% 
    rowwise() %>%
    mutate(stream.mean.fdom = mean(c_across(starts_with("str.fdom")), na.rm = TRUE)) %>% 
    ungroup()
  
  
  
  ### Annual
  fdom.annual <- str.fdom.daily %>% 
    mutate(year = year(day), month = month(day), dayofmonth = day(day)) %>% 
    group_by(year) %>% 
    summarise(
      across(
        .cols = c(names(str.fdom.daily)[4]),
        .fns = ~mean(.x, na.rm = TRUE),
        .names = "fdom"
      )) %>% 
    mutate(siteID = mysite) %>% 
    relocate(any_of(c("siteID", "year", "month")))
  
  
  
  write.csv(str.fdom.daily,
            paste0("MSB_NEON/Aggregated_Data/daily_aggregate/daily.stream.fdom/",
                   "stream.fdom.daily.",mysite, ".csv"), row.names = FALSE)
  
  
  write.csv(fdom.annual,
            paste0("MSB_NEON/Aggregated_Data/annual_aggregate/annual.stream.fdom/",
                   "stream.fdom.annual.",mysite, ".csv"), row.names = FALSE)
  
  rm(str.fdom.daily)
  rm(str.fdom.grouped)
  rm(str.fdom.main)
  rm(mysite)
  rm(fdom.annual)
}




############################ Reading Data

gw.chem.all <- read.csv("MSB_NEON/Aggregated_Data/summary_data/gw.chem.all.compiled.csv") %>% 
  select(siteID, siteID.aq, day, DOC.gw.mgperL)

gw.chem.annual <- gw.chem.all %>% 
  group_by(siteID.aq) %>% 
  summarise(
    across(
      .cols = c(names(gw.chem.all)[c(4)]),
      .fns = ~mean(.x, na.rm = TRUE),
      .names = "gw.DOC"
    )) 



neon.terr.aq.pairs <- read.csv("MSB_NEON/NEON_Terrestrial_aquatic.csv") %>% 
  select(siteID.aquatic, siteID.terrestrial, state) %>%
  distinct() %>% 
  filter(siteID.aquatic != "WLOU") %>% 
  filter(siteID.aquatic != "OKSR") %>% 
  rename(siteID.aq = siteID.aquatic,
         siteID = siteID.terrestrial)


### Annual fdom allsites 

annual.fdom.all <- list.files(path = "MSB_NEON/Aggregated_Data/annual_aggregate/annual.stream.fdom/",  
           pattern = ".csv",
           full.names = TRUE) %>%
  lapply(read_csv) %>% 
  bind_rows %>% 
  drop_na(fdom) %>% 
  left_join(neon.terr.aq.pairs, by = c("siteID" = "siteID.aquatic")) %>% 
  na.omit() %>% 
  rename(siteID.aq = siteID,
         siteID = siteID.terrestrial)


annual.fdom.siteAVG <- annual.fdom.all %>% 
  group_by(siteID.aq) %>% 
  summarise(
    across(
      .cols = c(names(annual.fdom.all)[c(3)]),
      .fns = ~mean(.x, na.rm = TRUE),
      .names = "fdom"
    )) 

meg.pit <- read.csv("NEON_data/LatestData/soil.properties/soil.megapit/mgp_perbiogeosample.csv") %>% 
  # filter(siteID == mysite) %>% 
  filter(biogeoBottomDepth <=10) %>% 
  select(siteID, estimatedOC)
  

meg.pit <- meg.pit %>% 
  group_by(siteID) %>% 
  summarise(
    across(
      .cols = c(names(meg.pit)[2]),
      .fns = ~mean(.x, na.rm = TRUE),
      .names = "OC"
    ))


alldata <- annual.fdom.siteAVG %>%
  full_join(neon.terr.aq.pairs)


alldata <- alldata %>% 
  full_join(meg.pit) %>% 
  na.omit()


alldata <- alldata %>% 
  full_join(gw.chem.annual)



alldata.few <- alldata %>% 
  filter(siteID == "TOOL"|
        siteID == "HARV" |
        siteID == "KONZ" |
        siteID == "ORNL" |
        siteID == "OSBS")

plot(alldata.few$OC, alldata.few$fdom)



library(ggplot2)

ggplot(alldata.few, aes(x = OC, y = fdom)) +
  geom_smooth(method = "lm", color = "black", se = F) +
  geom_text(aes(label = siteID), size = 3.5, fontface = "bold") +
  
  theme_classic(base_size = 14) + 
  
  labs(x = "Soil Organic Carbon (gC/Kg-soil)", 
       y = "fDOM",
       title = "")



ggplot(alldata, aes(x = OC, y = fdom)) +
  geom_text(aes(label = siteID), size = 3.5, fontface = "bold") +
  geom_smooth(method = "lm", color = "black", se = F) +
  theme_classic(base_size = 14) + 
  
  labs(x = "Soil Organic Carbon (gC/Kg-soil)", 
       y = "fDOM",
       title = "")



ggplot(alldata, aes(x = gw.DOC, y = fdom)) +
  geom_text(aes(label = siteID), size = 3.5, fontface = "bold") +
  geom_smooth(method = "lm", color = "black", se = F) +
  theme_classic(base_size = 14) + 
  
  labs(x = "GW DOC (mg/L)", 
       y = "fDOM",
       title = "")



ggplot(alldata.few, aes(x = gw.DOC, y = fdom)) +
  geom_text(aes(label = siteID), size = 3.5, fontface = "bold") +
  geom_smooth(method = "lm", color = "black", se = F) +
  theme_classic(base_size = 14) + 
  
  labs(x = "GW DOC (mg/L)", 
       y = "fDOM",
       title = "")
