# Copyright 2017 Province of British Columbia
# 
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
# Module 2. CLEAN & CALCULATE WQGS
############################################################################### 

## Have to fill in directory for location of raw data csv for this script to run.

## Standarizes EMS parameter codes and parameter names
## Changes negative temperature values to 0 and deletes rows with missing values
## Replicates on same day are averaged. 
## Identifies outliers. These can be kept or deleted (`delete_outliers = TRUE` or `delete_outliers = FALSE`). 
all_data_clean <- clean_wqdata(all_data, by = "EMS_ID", delete_outliers = TRUE)


## UNIT CONVERSION FROM mg/L to ug/L. Define your list of parameters once and use it repeatedly.
## ONLY RUN IF WANT UNITS CONVERTED FOR CLEAN DATA. DOESN'T WORK IF WANT TO USE CALC LIMITS
params_ug_L <- c("Arsenic Total", "Cadmium Dissolved", "Cobalt Total", "Copper Total", "Cyanide WAD", 
                 "Lead Total", "Mercury Total", "Napthalene (C10H8)", "Nickel Total", "Selenium Total", 
                 "Silver Total", "Thallium Total", "Toluene", "Uranium Total", "Zinc Total")

all_data_clean <- mutate(all_data_clean, 
                         Value = ifelse(Variable %in% params_ug_L, Value * 1000, Value),
                         Units = ifelse(Variable %in% params_ug_L, "µg/L", Units))

## WRITE CSV FOR RAW DATASET
write.csv(all_data_clean, 'all_data_clean.csv', row.names = FALSE)

## View summary of parameters included in the dataframe
parameters <- distinct(all_data_clean, Variable) 
sample_state <- distinct(bind_data, SAMPLE_STATE, SAMPLE_DESCRIPTOR)
sites <- distinct(all_data_clean, EMS_ID)

## CALCULATE AQUATIC LIFE WATER QUALITY GUIDELINES
## Drops parameters without limits (water quality guidelines (WQGs))
## To use the long-term WQGs on daily values set `term = "long-daily`. 
## If a limit depends on another variable such as pH, Total Chloride, or Total Hardness and no value was 
## recorded for the date of interest then the pH, Total Chloride or Total Hardness value is assumed to 
## be the average recorded value over the 30 day period. The one exception is if 
## `estimate_variables = TRUE`, in which case a parametric model is used to predict the pH, Total Chloride
## and Total Hardness for all dates with a value of any variable. 
## Deletes any parameters which don't have limits
all_data_limits <- calc_limits(all_data_clean, by = "EMS_ID", 
                               term = "long-daily", estimate_variables = TRUE, 
                               clean = FALSE, messages = TRUE)

## WATERSHED DATAFRAMES
## Add watershed column to dataframe and assign watershed name to a set of monitoring sites

all_data_clean$Watershed <- NA

colnames(all_data_clean)[which(names(all_data_clean) == "Monitoring_Site")] <- "EMS_ID"

all_data_clean$Watershed[all_data_clean$EMS_ID %in% c("1177702","E306397","E308499","E260100","E306398","E308498",
                                                      "E308497","E308496","E306399","E277175","E277174","E277176",
                                                      "E206319","E206757","E277178","E277172","0410097","E304933",
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
                                                      "E206324","E308995","E305987","E305989","0410060","0410092",
                                                      "E308994","E206526","E309379","E242344","E308990","E289554",
                                                      "0410099","0410100","E206755","0410058","E207460","E289553",
                                                      "E295109","E289556","E308820","E277611","E277610","E277609",
                                                      "E308495","E289552","0400552","E210870","E279733")]  <- "Murray River"

all_data_clean$Watershed[all_data_clean$EMS_ID %in% c("0410039","E206959","E206705","0410042")]  <- "Pouce Coupe River"

all_data_clean$Watershed[all_data_clean$EMS_ID %in% c("0400492","0400134","E249801")]  <- "Upper Peace River"

all_data_clean$Watershed[all_data_clean$EMS_ID=="0410028"]  <- "Upper Pine River"

all_data_clean$Watershed[all_data_clean$EMS_ID=="E250094"] <- "Blueberry River"

all_data_clean$Watershed[all_data_clean$EMS_ID %in% c("E207448","E207449","0400145")]  <- "Lower Beatton River"

all_data_clean$Watershed[all_data_clean$EMS_ID %in% c("0400560","0400561")]  <- "Lower Pine River"

all_data_clean$Watershed[all_data_clean$EMS_ID=="E206585"]  <- "Lower Peace River"

all_data_clean$Watershed[all_data_clean$EMS_ID %in% c("E253393","E253394")]  <- "Lynx Creek"

all_data_clean$Watershed[all_data_clean$EMS_ID=="E249798"]  <- "Moberly River"

all_data_clean$Watershed[all_data_clean$EMS_ID %in% c("0400545","E256834","E256837")]  <- "Middle Kiskatinaw River"

all_data_clean$Watershed[all_data_clean$EMS_ID=="E256840"]  <- "East Kiskatinaw River"

all_data_clean$Watershed[all_data_clean$EMS_ID %in% c("E228061","E228062")]  <- "Lower Kiskatinaw River"

all_data_clean$Watershed[all_data_clean$EMS_ID=="E249804"]  <- "Milligan Creek"

all_data_clean$Watershed[all_data_clean$EMS_ID %in% c("E290869","E282116")]  <- "Lower Petitot River"

all_data_clean$Watershed[all_data_clean$EMS_ID=="E290871"]  <- "Middle Petitot River"

all_data_clean$Watershed[all_data_clean$EMS_ID=="E243640"]  <- "Klua Creek"

colnames(all_data_clean)[which(names(all_data_clean) == "EMS_ID")] <- "Monitoring_Site"

## UPPER PEACE RIVER

up_peace <- filter(all_data_clean, Watershed == "Upper Peace River")
up_peace$Date <- as.Date(up_peace$Date,"%Y-%m-%d")
up_peace$Day <- as.integer(format(up_peace$Date, '%d'))
up_peace$Year <- as.numeric(format(up_peace$Date,'%Y'))
up_peace$Month <- as.character(format(up_peace$Date,'%b'))
Yearsofdata <- distinct(up_peace, Year)
numberofparams <- distinct(up_peace, Variable)

## UPPER PINE RIVER

up_pine <- filter(all_data_clean, Watershed == "Upper Pine River")
up_pine$Date <- as.Date(up_pine$Date,"%Y-%m-%d")
up_pine$Day <- as.integer(format(up_pine$Date, '%d'))
up_pine$Year <- as.numeric(format(up_pine$Date,'%Y'))
up_pine$Month <- as.character(format(up_pine$Date,'%b'))
Yearsofdata <- distinct(up_pine, Year)
numberofparams <- distinct(up_pine, Variable)

## Pouce Coupe

pc <- filter(all_data_clean, Watershed == "Pouce Coupe River")
pc$Date <- as.Date(pc$Date,"%Y-%m-%d")
pc$Day <- as.integer(format(pc$Date, '%d'))
pc$Year <- as.numeric(format(pc$Date,'%Y'))
pc$Month <- as.character(format(pc$Date,'%b'))
Yearsofdata <- distinct(pc, Year)
numberofparams <- distinct(pc, Variable)

## BLUEBERRY RIVER

blue <- filter(all_data_clean, Watershed == "Blueberry River")
blue$Date <- as.Date(blue$Date,"%Y-%m-%d")
blue$Day <- as.integer(format(blue$Date, '%d'))
blue$Year <- as.numeric(format(blue$Date,'%Y'))
blue$Month <- as.character(format(blue$Date,'%b'))
Yearsofdata <- distinct(blue, Year)
numberofparams <- distinct(blue, Variable)

## Lynx Creek
## 
lynx <- filter(all_data_clean, Watershed == "Lynx Creek")
lynx$Date <- as.Date(lynx$Date,"%Y-%m-%d")
lynx$Day <- as.integer(format(lynx$Date, '%d'))
lynx$Year <- as.numeric(format(lynx$Date,'%Y'))
lynx$Month <- as.character(format(lynx$Date,'%b'))
Yearsofdata <- distinct(lynx, Year)
numberofparams <- distinct(lynx, Variable)

## MOBERLY RIVER

mob <- filter(all_data_clean, Watershed == "Moberly River")
mob$Date <- as.Date(mob$Date,"%Y-%m-%d")
mob$Day <- as.integer(format(mob$Date, '%d'))
mob$Year <- as.numeric(format(mob$Date,'%Y'))
mob$Month <- as.character(format(mob$Date,'%b'))
Yearsofdata <- distinct(mob, Year)
numberofparams <- distinct(mob, Variable)

## EAST KISKATINAW RIVER

e_kisk <- filter(all_data_clean, Watershed == "East Kiskatinaw River")
e_kisk $Date <- as.Date(e_kisk $Date,"%Y-%m-%d")
e_kisk $Day <- as.integer(format(e_kisk $Date, '%d'))
e_kisk $Year <- as.numeric(format(e_kisk $Date,'%Y'))
e_kisk $Month <- as.character(format(e_kisk $Date,'%b'))
Yearsofdata <- distinct(e_kisk , Year)
numberofparams <- distinct(e_kisk , Variable)

## MILLIGAN CREEK

mill <- filter(all_data_clean, Watershed == "Milligan Creek")
mill $Date <- as.Date(mill$Date,"%Y-%m-%d")
mill $Day <- as.integer(format(mill $Date, '%d'))
mill $Year <- as.numeric(format(mill $Date,'%Y'))
mill $Month <- as.character(format(mill $Date,'%b'))
Yearsofdata <- distinct(mill , Year)
numberofparams <- distinct(mill , Variable)

## MID PETITOT

midpet <- filter(all_data_clean, Watershed == "Middle Petitot River")
midpet$Date <- as.Date(midpet$Date,"%Y-%m-%d")
midpet$Day <- as.integer(format(midpet$Date, '%d'))
midpet$Year <- as.numeric(format(midpet$Date,'%Y'))
midpet$Month <- as.character(format(midpet$Date,'%b'))
Yearsofdata <- distinct(midpet, Year)
numberofparams <- distinct(midpet, Variable)

## KLUA CREEK

klua <- filter(all_data_clean, Watershed == "Klua Creek")
klua$Date <- as.Date(klua$Date,"%Y-%m-%d")
klua$Day <- as.integer(format(klua$Date, '%d'))
klua$Year <- as.numeric(format(klua$Date,'%Y'))
klua$Month <- as.character(format(klua$Date,'%b'))
Yearsofdata <- distinct(klua, Year)
numberofparams <- distinct(klua, Variable)

## LOW PETITOT
## To determine each year of each site, just filter each year in the dataframe.
lowpet <- filter(all_data_clean, Watershed == "Lower Petitot River")
lowpet$Date <- as.Date(lowpet$Date,"%Y-%m-%d")
lowpet$Day <- as.integer(format(lowpet$Date, '%d'))
lowpet$Year <- as.numeric(format(lowpet$Date,'%Y'))
lowpet$Month <- as.character(format(lowpet$Date,'%b'))
Yearsofdata <- distinct(lowpet, Year)
numberofparams <- distinct(lowpet, Variable)

## LOW KISKATINAW

lowkisk <- filter(all_data_clean, Watershed == "Lower Kiskatinaw River")
lowkisk$Date <- as.Date(lowkisk$Date,"%Y-%m-%d")
lowkisk$Day <- as.integer(format(lowkisk$Date, '%d'))
lowkisk$Year <- as.numeric(format(lowkisk$Date,'%Y'))
lowkisk$Month <- as.character(format(lowkisk$Date,'%b'))
Yearsofdata <- distinct(lowkisk, Year)
numberofparams <- distinct(lowkisk, Variable)

## MID KISKATINAW

midkisk <- filter(all_data_clean, Watershed == "Middle Kiskatinaw River")
midkisk$Date <- as.Date(midkisk$Date,"%Y-%m-%d")
midkisk$Day <- as.integer(format(midkisk$Date, '%d'))
midkisk$Year <- as.numeric(format(midkisk$Date,'%Y'))
midkisk$Month <- as.character(format(midkisk$Date,'%b'))
Yearsofdata <- distinct(midkisk, Year)
numberofparams <- distinct(midkisk, Variable)

site0400545 <- filter(midkisk, Monitoring_Site == "0400545")
site0400545params <- distinct(site0400545, Variable)

siteE256837 <- filter(midkisk, Monitoring_Site == "E256837")
siteE256837params <- distinct(siteE256837, Variable)

## LOW PEACE

lowpeace <- filter(all_data_clean, Watershed == "Lower Peace River")
lowpeace$Date <- as.Date(lowpeace$Date,"%Y-%m-%d")
lowpeace$Day <- as.integer(format(lowpeace$Date, '%d'))
lowpeace$Year <- as.numeric(format(lowpeace$Date,'%Y'))
lowpeace$Month <- as.character(format(lowpeace$Date,'%b'))
Yearsofdata <- distinct(lowpeace, Year)
numberofparams <- distinct(lowpeace, Variable)
lowpeace <- filter(lowpeace, Value != "161.5" | is.na(Value))

## LOWER PINE

lowpine <- filter(all_data_clean, Watershed == "Lower Pine River")
lowpine$Date <- as.Date(lowpine$Date,"%Y-%m-%d")
lowpine$Day <- as.integer(format(lowpine$Date, '%d'))
lowpine$Year <- as.numeric(format(lowpine$Date,'%Y'))
lowpine$Month <- as.character(format(lowpine$Date,'%b'))
Yearsofdata <- distinct(lowpine, Year)
numberofparams <- distinct(lowpine, Variable)

## LOWER BEATTON

lowbeat <- filter(all_data_clean, Watershed == "Lower Beatton River")
lowbeat$Date <- as.Date(lowbeat$Date,"%Y-%m-%d")
lowbeat$Day <- as.integer(format(lowbeat$Date, '%d'))
lowbeat$Year <- as.numeric(format(lowbeat$Date,'%Y'))
lowbeat$Month <- as.character(format(lowbeat$Date,'%b'))
Yearsofdata <- distinct(lowbeat, Year)
numberofparams <- distinct(lowbeat, Variable)
##Take out 1970s zinc data
lowbeat <- filter(lowbeat, Monitoring_Site != "0400145" | is.na(Monitoring_Site))

## MURRAY RIVER

murray <- filter(all_data_clean, Watershed == "Murray River")
murray$Date <- as.Date(murray$Date,"%Y-%m-%d")
murray$Day <- as.integer(format(murray$Date, '%d'))
murray$Year <- as.numeric(format(murray$Date,'%Y'))
murray$Month <- as.character(format(murray$Date,'%b'))
Yearsofdata <- distinct(murray, Year)
numberofparams <- distinct(murray, Variable)
DOmurr <- filter(murray, Variable == "Dissolved Oxygen-Field"|Variable == "Oxygen Dissolved") %>% 
  summarise(Median=median(Value))
