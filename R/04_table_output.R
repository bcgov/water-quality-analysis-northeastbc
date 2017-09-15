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

#####################################################################
# Module 4. CREATE WATER QUALITY SUMMARY STATS TABLES FOR CLEAN DATA
#####################################################################

## This module can also be done in an R Markdown file to create a word output.
## The Upper Peace and Lower Peace Watersheds were separated into turbid and clear flow periods to view
## the data better. All other watersheds included all the years data grouped together. 

## TURBID FLOWS DATA SUMMARY TABLE

## UPPER PEACE DATAFRAME
up_peace_freshet <- filter(up_peace, Month == "Apr"| Month == "May"| Month == "Jun"| Month == "Jul") 
up_peace_table <- up_peace_freshet %>%
  group_by(Variable, Units) %>%
  summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
table <- kable(up_peace_table, digits = 2, knitr.kable.NA = '', caption = " Upper Peace River Watershed: Summary of water chemistry data and simple statistics collected from April to July, 1972 to 2005.")
print(table) 

## LOWER PEACE DATAFRAME
low_peace_freshet <- filter(lowpeace, Month == "Apr"| Month == "May"| Month == "Jun"| Month == "Jul"|Month == "Aug") 
low_peace_table <- low_peace_freshet %>%
  group_by(Variable, Units, ResultLetter) %>%
  summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
table <- kable(low_peace_table, digits = 3, knitr.kable.NA = '', caption = "Lower Peace River Watershed: Summary of water chemistry data and simple statistics collected from April to July, 1984 to 2017.")
print(table) 

## cLEAR FLOWS DATA SUMMARY TABLE

## UPPER PEACE
up_peace_non_freshet <- filter(up_peace, Month == "Jan"| Month == "Feb"| Month == "Mar"| Month == "Aug"|Month == "Sep"|Month == "Oct"| Month == "Nov"| Month == "Dec")
up_peace_non_freshet_table <- up_peace_non_freshet %>%
  group_by(Variable, Units) %>%
  summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
table <- kable(up_peace_non_freshet_table, digits = 3, knitr.kable.NA = '', caption = " Upper Peace River Watershed: Summary of water chemistry data and simple statistics collected during January to March and August to December, 1971 to 2009.")
print(table) 

## LOWER PEACE
low_peace_non_freshet <- filter(lowpeace, Month == "Jan"| Month == "Feb"| Month == "Mar"|Month == "Sep"|Month == "Oct"| Month == "Nov"| Month == "Dec")
low_peace_non_freshet_table <- low_peace_non_freshet %>%
  group_by(Variable, Units) %>%
  summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
table <- kable(low_peace_non_freshet_table, digits = 3, knitr.kable.NA = '', caption = "Lower Peace River Watershed: Summary of water chemistry data and simple statistics collected during January to March and August to December, 1984 to 2017.")
print(table) 

## SUMMARY TABLE FOR ALL DATES FOR ALL WATERSHEDS WITH RESULT LETTER (<)
watersheds <- c("Murray River","Pouce Coupe River","Upper Peace River", "Upper Pine River",
                "Blueberry River","Lower Beatton River","Lower Pine River","Lower Peace River",
                "Lynx Creek","Moberly River", "Middle Kiskatinaw River","East Kiskatinaw River",
                "Lower Kiskatinaw River","Milligan Creek","Lower Petitot River","Middle Petitot River",
                "Klua Creek")

for (w in watersheds) {
  sum_table <-filter(all_data_clean, Watershed == w)  
  stats_sum <- sum_table %>%
    group_by(Variable, Units, ResultLetter) %>%
    summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
  table <- kable(stats_sum, digits = 5, knitr.kable.NA = '', caption = paste0(w," Watershed: Water Quality Summary"))
  print(table) 
}

## SUMMARY TABLE FOR ALL DATES FOR ALL WATERSHEDS WITHOUT RESULT LETTER (groups all parameters together and 
## gives a parameter average per watershed)
for (w in watersheds) {
  sum_table <-filter(all_data_clean, Watershed == w)  
  stats_sum <- sum_table %>%
    group_by(Variable, Units, Watershed) %>%
    summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
  table <- kable(stats_sum, digits = 5, knitr.kable.NA = '', caption = paste0(w," Watershed: Water Quality Summary"))
  print(table) 
}

############################################################################################
# Module 5. CREATE WATER QUALITY SUMMARY STATS TABLES FOR DATA WITH WATER QUALITY GUIDELINES
############################################################################################

## These tables include the "Guideline" column 

## SUMMARY TABLE FOR ALL DATES FOR ALL WATERSHEDS WITHOUT RESULT LETTER (groups all parameters together)

## UPPER PEACE
up_peace_freshet <- filter(up_peace, Month == "Apr"| Month == "May"| Month == "Jun"| Month == "Jul") 
up_peace_table <- up_peace_freshet %>%
  group_by(Variable, Units, ResultLetter, Guideline) %>%
  summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
table <- kable(up_peace_table, digits = 2, knitr.kable.NA = '', caption = " Upper Peace River Watershed: Summary of water chemistry data and simple statistics collected from April to July, 1972 to 2005.")
print(table) 

up_peace_non_freshet <- filter(up_peace, Month == "Jan"| Month == "Feb"| Month == "Mar"| Month == "Aug"|Month == "Sep"|Month == "Oct"| Month == "Nov"| Month == "Dec")
up_peace_non_freshet_table <- up_peace_non_freshet %>%
  group_by(Variable, Units, ResultLetter, Guideline) %>%
  summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
table <- kable(up_peace_non_freshet_table, digits = 3, knitr.kable.NA = '', caption = " Upper Peace River Watershed: Summary of water chemistry data and simple statistics collected during January to March and August to December, 1971 to 2009.")
print(table) 

## LOWER PEACE
low_peace_freshet <- filter(lowpeace, Month == "Apr"| Month == "May"| Month == "Jun"| Month == "Jul"|Month == "Aug") 
low_peace_table <- low_peace_freshet %>%
  group_by(Variable, Units, ResultLetter, Guideline) %>%
  summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
table <- kable(low_peace_table, digits = 3, knitr.kable.NA = '', caption = "Lower Peace River Watershed: Summary of water chemistry data and simple statistics collected from April to July, 1984 to 2017.")
print(table)

low_peace_non_freshet <- filter(lowpeace, Month == "Jan"| Month == "Feb"| Month == "Mar"|Month == "Sep"|Month == "Oct"| Month == "Nov"| Month == "Dec")
low_peace_non_freshet_table <- low_peace_non_freshet %>%
  group_by(Variable, Units, ResultLetter, Guideline) %>%
  summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
table <- kable(low_peace_non_freshet_table, digits = 3, knitr.kable.NA = '', caption = "Lower Peace River Watershed: Summary of water chemistry data and simple statistics collected during January to March and August to December, 1984 to 2017.")
print(table)  

## REST OF WATERSHEDS
for (w in watersheds) {
  sum_table_limits <-filter(all_data_limits, Watershed == w)  
  stats_sum <- sum_table_limits %>%
    group_by(Variable, Units, Guideline, Watershed) %>%
    summarise(Min=min(Value), Max=max(Value), Median=median(Value), n=length(Variable))
  table <- kable(stats_sum, digits = 3, knitr.kable.NA = '', caption = paste0(w," Watershed: Water Quality Summary"))
  print(table) 
}

