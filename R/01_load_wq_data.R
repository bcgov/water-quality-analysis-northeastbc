<div id="devex-badge"><a rel="Exploration" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a></div>
  # Copyright 2017 Province of British Columbia 
  
  # Licensed under the Apache License, Version 2.0 (the "License"); 
  # you may not use this file except in compliance with the License. 
  # You may obtain a copy of the License at 
  #  
  # http://www.apache.org/licenses/LICENSE-2.0 
  #  
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



## LOAD TWO YEAR AND HISTORIC EMS WATER QUALITY DATASET from BC Data Catalogue using bcgov/rems package

#Remove_data_cache("2yr")
#Remove_data_cache("Historic")
twoyear <- get_ems_data(which = "2yr", ask = TRUE)
twoyearparams <- distinct(twoyear, PARAMETER)
filtered_twoyear <- filter_ems_data(twoyear, 
                                    emsid = c("0410060","0410094","0410097","E206227","E206228","E206229","E206232",
                                              "E206319","E206521","E206526","E206972","0410042","E206705","E206959",
                                              "0410039","0400134","0400492","0410028","E250094","E207448","E207449",
                                              "0400561","E206585","E253393","E249798","0400545","E256834","E256840",
                                              "E228061","E228062","E249801","E249804","E290869","E243640","E256837",
                                              "0400560","0400145","E253394","E290871", "E282116"), 
                                    to_date = "2017/05/24") 

## This function just needs to be run once, the first time you run the script
#download_historic_data(ask = FALSE)

hist_db <- attach_historic_data()
filtered_historic <- hist_db %>% 
  select(EMS_ID,MONITORING_LOCATION,LOCATION_TYPE,COLLECTION_START,LOCATION_PURPOSE,SAMPLE_CLASS,
         SAMPLE_STATE,SAMPLE_DESCRIPTOR,PARAMETER_CODE,PARAMETER,ANALYTICAL_METHOD_CODE,ANALYTICAL_METHOD,
         RESULT_LETTER,RESULT,UNIT, METHOD_DETECTION_LIMIT) %>% 
  filter(EMS_ID %in% c("0410060","0410094","0410097","E206227","E206228","E206229","E206232","E206319",
                       "E206521","E206526","E206972","0410042","E206705","E206959","0410039","0400134",
                       "0400492","0410028","E250094","E207448","E207449","0400561","E206585","E253393",
                       "E249798","0400545","E256834","E256840","E228061","E228062","E249801","E249804",
                       "E290869","E243640","E256837","0400560","0400145","E253394","E290871","E282116"))                    

filtered_historic <- collect(filtered_historic) %>% 
  mutate(COLLECTION_START = ems_posix_numeric(COLLECTION_START))

bind_data <- bind_ems_data(filtered_twoyear, filtered_historic) 
bind_data <- filter(bind_data, SAMPLE_STATE == "Fresh Water")

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

