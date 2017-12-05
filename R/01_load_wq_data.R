# Copyright 2017 Province of British Columbia 
  
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
# You may obtain a copy of the License at 
  
# http://www.apache.org/licenses/LICENSE-2.0 
  
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and limitations under the License. 


############################################################################### 
# Module 1. LOAD PACKAGES, FUNCTIONS AND NORTHEAST WATER QUALTIY DATA FROM EMS
############################################################################### 

install.packages("devtools")
library(devtools)
install_github("bcgov/rems") 
install_github("bcgov/wqbc", ref = "clean-nodrop")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("scales")
library(rems)
library(wqbc)
library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)
library(lubridate)



## LOAD TWO YEAR AND HISTORIC EMS WATER QUALITY DATASET from BC Data Catalogue using bcgov/rems package

#Remove_data_cache("2yr")
#Remove_data_cache("Historic")
twoyear <- get_ems_data(which = "2yr", ask = TRUE)

filtered_twoyear <- filter_ems_data(twoyear, 
                                    emsid = c("0410060","0410094","0410097","E206227","E206228","E206229","E206232","E206319","E206521","E206526","E206972","0410042","E206705","E206959","0410039","0400134","0400492","0410028","E250094","E207448","E207449","0400561","E206585","E253393","E249798","0400545","E256834","E256840","E228061","E228062","E249801","E249804","E290869","E243640","E256837","0400560","0400145","E253394","E290871","E282116","1177702","E306397","E308499","E260100","E306398","E308498",
                       "E308497","E308496","E306399","E277175","E277174","E277176",
                       "E206319","E206757","E277178","E277172","E304933",
                       "E273200","E277177","E273199","E234064","E273195","E273196",
                       "E206322","E308494","E308493","E273193","E273194","E306408",
                       "E306409","E298950","E277176","E277173","E308815","E241806",
                       "E308686","E308677","E308812","E308814","E308811","E308670",
                       "E308813","E308669","E234066","E308668","E309376","E274887",
                       "E308810","E308809","E234065","E274889","E309386","E309378",
                       "E309377","E309385","E309383","E309384","E309382","E309394",
                       "E308687","E309392","E274888","E309390","E309381","E309389",
                       "E309391","E309380","E309393","0410092","E308672","0410059",
                       "E308994","E308993","E206321","E206973","E308667","E308679",
                       "E304951","E308667","E308986","E308988","E309387","E309388",
                       "E206323","0410057","E304936","E308989","E308997","E308818",
                       "E206324","E308995","E305987","E305989","0410061","0410092",
                       "E308994","E206526","E309379","E242344","E308990","E289554",
                       "0410099","0410100","E206755","0410058","E207460","E289553",
                       "E295109","E289556","E308820","E277611","E277610","E277609",
                       "E308495","E289552","0400552","E210870","E279733","E206225","E206226","E277612","E277613","E277614","E289555","0410041","0410028","0400562","E283430","E257094","E219248","E253405","E253399","E253406","E253398","E253404","E253397","E253396","E253402","E253401","E253400","E253395","E253403","E283432","E283431","E260101","E260102","1177731","E265925","E265929","1177729","1177730","1177733","1177737","1177736","1177732","1177738","1177739","1177743","1177742","E207905","E260099","E250092","E249805","E256842","1134019","E277186","E286794","E263630","E286795","1177720","E273189","E273188","E276709","E273190","E286793","E286792","E273191","E273192","E234188","E234187","E234186","E306405","E286789","E238662","E306402","E306401","E306403","E295021","1132028","E309217","E309218","E309547","E243581","E303831","E303845","E305437","E305433","E306404","1132051","1100081","E309216","E309215","E309189","E277179","E303830","E303851","E305434"), to_date = "2017/11/09")

## This function just needs to be run once, the first time you run the script
#download_historic_data(ask = FALSE)

hist_db <- attach_historic_data()
filtered_historic <- hist_db %>% 
  select(EMS_ID,MONITORING_LOCATION,LOCATION_TYPE,COLLECTION_START,LOCATION_PURPOSE,SAMPLE_CLASS,
         SAMPLE_STATE,SAMPLE_DESCRIPTOR,PARAMETER_CODE,PARAMETER,ANALYTICAL_METHOD_CODE,ANALYTICAL_METHOD,
         RESULT_LETTER,RESULT,UNIT, METHOD_DETECTION_LIMIT) %>% 
  filter(EMS_ID %in% c("0410060","0410094","0410097","E206227","E206228","E206229","E206232","E206319","E206521","E206526","E206972","0410042","E206705","E206959","0410039","0400134","0400492","0410028","E250094","E207448","E207449","0400561","E206585","E253393","E249798","0400545","E256834","E256840","E228061","E228062","E249801","E249804","E290869","E243640","E256837","0400560","0400145","E253394","E290871","E282116","1177702","E306397","E308499","E260100","E306398","E308498",
                       "E308497","E308496","E306399","E277175","E277174","E277176",
                       "E206319","E206757","E277178","E277172","E304933",
                       "E273200","E277177","E273199","E234064","E273195","E273196",
                       "E206322","E308494","E308493","E273193","E273194","E306408",
                       "E306409","E298950","E277176","E277173","E308815","E241806",
                       "E308686","E308677","E308812","E308814","E308811","E308670",
                       "E308813","E308669","E234066","E308668","E309376","E274887",
                       "E308810","E308809","E234065","E274889","E309386","E309378",
                       "E309377","E309385","E309383","E309384","E309382","E309394",
                       "E308687","E309392","E274888","E309390","E309381","E309389",
                       "E309391","E309380","E309393","0410092","E308672","0410059",
                       "E308994","E308993","E206321","E206973","E308667","E308679",
                       "E304951","E308667","E308986","E308988","E309387","E309388",
                       "E206323","0410057","E304936","E308989","E308997","E308818",
                       "E206324","E308995","E305987","E305989","0410061","0410092",
                       "E308994","E206526","E309379","E242344","E308990","E289554",
                       "0410099","0410100","E206755","0410058","E207460","E289553",
                       "E295109","E289556","E308820","E277611","E277610","E277609",
                       "E308495","E289552","0400552","E210870","E279733","E206225","E206226","E277612","E277613","E277614","E289555","0410041","0410028","0400562","E283430","E257094","E219248","E253405","E253399","E253406","E253398","E253404","E253397","E253396","E253402","E253401","E253400","E253395","E253403","E283432","E283431","E260101","E260102","1177731","E265925","E265929","1177729","1177730","1177733","1177737","1177736","1177732","1177738","1177739","1177743","1177742","E207905","E260099","E250092","E249805","E256842","1134019","E277186","E286794","E263630","E286795","1177720","E273189","E273188","E276709","E273190","E286793","E286792","E273191","E273192","E234188","E234187","E234186","E306405","E286789","E238662","E306402","E306401","E306403","E295021","1132028","E309217","E309218","E309547","E243581","E303831","E303845","E305437","E305433","E306404","1132051","1100081","E309216","E309215","E309189","E277179"))                    

filtered_historic <- collect(filtered_historic) %>% 
  mutate(COLLECTION_START = ems_posix_numeric(COLLECTION_START))

bind_data <- bind_ems_data(filtered_twoyear, filtered_historic) 
bind_data <- filter(bind_data, SAMPLE_STATE == "Fresh Water")
#params <- distinct(bind_data, PARAMETER)

## TIDY DATASET
## 
## Tidies water quality data downloaded from EMS database using the bcgov/rems package. 
## It retains and renames required columns and sets the timezone to PST. 
## It sets values that are flagged as being less than the detection limit to the MDL.
## Some values show up in the summary tables as less than the MDL because the RESULT_LETTER column 
## did not have "<" in it, so the code did not recognize to convert this value to equal the MDL.  
## If RESULT_LETTER has '>', delete those rows as means greater than instrument reading.
## Remove variables not of interest to a fresh water analysis, like Biomass. 
tidy_data <- tidy_ems_data(bind_data, mdl_action = "mdl")
all_data <- filter(tidy_data, ResultLetter != ">" | is.na(ResultLetter))
all_data <- filter(all_data,!grepl('Barometric|Biomass|Chlorophyll|Flow|Silica|Air|Streptococcus
                                   |Salinity|Tannin|Surfactant|Moisture|Phaeophytin|Extractable|Extrac.
                                   |Extractble|Extractbl', Variable))

