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

############################################################################### # Module 3. INITIAL DATA VISUALIZATION
############################################################################### 
## ORGANIZE DATASET BY UNITS TO PLOT
## 
## Set a vector of the parameters we are interested in
## Do this manually by looking at the `parameters` and `all_data_limits` dataframes
## 
mgL <- c("Ammonia Dissolved", "Ammonia Total", "Boron Dissolved", "Calcium Total", "Carbon Dissolved Organic", "Chloride Dissolved", "Fluoride Dissolved", "Iron Total", "Nitrate Dissolved", "Oxygen Dissolved") 

ugL <- c("Barium Dissolved", "Barium Total", "Cadmium Total", "Chromium Dissolved", "Chromium Total", "Cobalt Dissolved", "Copper Dissolved" , "Mercury Total", "Nickel Dissolved", "Selenium Dissolved", "Silver Dissolved","Thallium Dissolved", "Uranium Dissolved")

mgL_withWQG <- c("Nitrogen Dissolved", "Phosphorus Total","Manganese Total", "Molybdenum Total","Phosphorus Dissolved", "Sulphate Total", "Boron Total", "Aluminium Dissolved", "Nitrogen Total", "Nitrate Total")

ugL_withWQG <- c("Arsenic Total", "Cadmium Total", "Copper Total","Lead Total","Selenium Total", "Cobalt Total","Thallium Total","Uranium Total", "Nickel Total","Silver Total", "Zinc Total")

## PLOT DATA 1 (in a loop, make one plot per parameter)
## First set working directory to save plots to. This section only plots with clean data. Water quality guidelines not taken into account.

#setwd('XXX/FolderX')

## mg/L plots
site_mgL <- filter(all_data_clean, Variable %in% mgL)

for (v in mgL) {
  #change_units <- ifelse(E303845_ugL$Units == "ug/L", E303845_ugL$Value/1000, E303845_ugL$Value). 
  ## This is making the values to the power of 100. Only have to change if want unit other than what guideline is expressed is.
  mgL_plots <- filter(site_mgL, Variable == v)
  plot <- ggplot(mgL_plots, aes(x = Date, y = Value)) + 
    expand_limits(y = c(0, 0.5)) +
    geom_point(colour = 'blue') +
    ##geom_hline(data = mgL_plots, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
    ggtitle(v, "mg/L") + scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("Value")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
}

## ug/L parameters plots
site_ugL <- filter(all_data_clean, Variable %in% ugL)

for (v in ugL) {
  ugL_plots <- filter(site_ugL, Variable == v)
  plot <- ggplot(ugL_plots, aes(x = Date, y = Value)) + 
    expand_limits(y = c(0, 10)) +
    geom_point(colour = 'blue') +
    ##geom_hline(data = ugL_plots, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
    ggtitle(v, "ug/L") + scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("Value")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
}

## SINGLE PLOTS for single unit parameters
## pH
site_pH <-filter(all_data_clean, Variable=="pH") 
pHplot <- ggplot(site_pH, aes(x=Date, y=Value)) + 
  geom_point(colour="blue") + 
  expand_limits(y = c(0, 15)) + 
  scale_x_date(labels = date_format("%b %Y")) +
  xlab("Date") + 
  ylab("pH")
plot(pHplot)
ggsave(filename = "pHplot.png", plot = pHplot, units= "in") 

## Temperature
E257094_temp <-filter(all_data_clean, Variable=="Temperature") 
tempplot <- ggplot(E206585_temp, aes(x=Date, y=Value)) + geom_point(colour="blue") + 
  scale_x_date(labels = date_format("%b %Y"))+
  xlab("Date") + ylab("Temperature (Celcius)")
plot(tempplot)
ggsave(filename = "temp_plot.png", plot = tempplot, units= "in") 

## Conductivity
E257094_cond <-filter(all_data_clean, Variable=="Conductance")
condplot <- ggplot(E206585_cond, aes(x=Date, y=Value)) + geom_point(colour="blue") + 
  xlab("Date") + ylab("Conductivity (us/cm)")
plot(condplot)
ggsave(filename = "cond_plot.png", plot = pHplot, units= "in", dpi = 120) 

## E coli
E257094_Ecol <-filter(all_data_clean, Variable=="E. coli")
Ecolplot <- ggplot(E206585_Ecol, aes(x=Date, y=Value)) + geom_point(colour="blue") + 
  scale_x_date(labels = date_format("%b %Y"))+
  ylab("(E. coli per 100 mL)")
plot(Ecolplot)
ggsave(filename = "Ecoli_plot.png", plot = Ecolplot, units= "in") 

## Turbidity 
E257094_turb <-filter(all_data_clean, Variable=="Turbidity")
turbplot <- ggplot(E206585_turb, aes(x=Date, y=Value)) + geom_point(colour="blue") + 
  scale_x_date(labels = date_format("%b %Y")) + ylab("Turbidity (NTU)")
plot(turbplot) 
ggsave(filename = "turbs_plot.png", plot = turbplot, units= "in") 

## Hardness Dissolved
hardness <- filter(all_data_clean, Variable=="Hardness Dissolved")
hard_plot <- ggplot(hardness, aes(x=Date, y=Value))+ geom_point(colour="blue") + 
  scale_x_date(labels = date_format("%b %Y")) + 
  ylab("Dissolved Hardness (mg/L)")
plot(hard_plot)

## PLOT DATA 2 - WITH WQGS
## Run plots again to add WQG line onto plot for those parameters that have WQGs calculated. 
## Have to make sure you've run 'all_data_limits' first (in 02_clean_calc_WQO)

#setwd('XXX/FolderX')

## mg/L plots
site_mgL <- filter(all_data_limits, Variable %in% mgL_withWQG)

for (v in mgL_withWQG) {
  #change_units <- ifelse(E303845_ugL$Units == "ug/L", E303845_ugL$Value/1000, E303845_ugL$Value). 
  ##This is making the values to the power of 100. Only have to change if want unit other than what 
  ##limit is expressed is.
  mgL_plots_WQGs <- filter(site_mgL, Variable == v)
  plot <- ggplot(mgL_plots_WQGs, aes(x = Date, y = Value)) + 
    expand_limits(y = c(0, 0.5)) +
    geom_point(colour = 'blue') +
    geom_hline(data = mgL_plots_WQGs, aes(yintercept = UpperLimit), colour = "red", 
               linetype = "dashed", show.legend = TRUE) +
    ggtitle(v, "mg/L") + scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("Value")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
}

## ug/L parameters plots
site_ugL <- filter(all_data_limits, Variable %in% ugL_withWQG)
for (v in ugL_withWQG) {
  ugL_plots_WQGs <- filter(site_ugL, Variable == v)
  plot <- ggplot(ugL_plots_WQGs, aes(x = Date, y = Value)) + 
    expand_limits(y = c(0, 10)) +
    geom_point(colour = 'blue') +
    geom_hline(data = ugL_plots_WQGs, aes(yintercept = UpperLimit), colour = "red", 
               linetype = "dashed", show.legend = TRUE) +
    ggtitle(v, "ug/L") + scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("Value")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
}

