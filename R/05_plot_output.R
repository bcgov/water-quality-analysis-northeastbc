## Specific plots for the final report, grouped by watershed and saved as .png files. 
## 
## Change Monitoring_site column name back to EMS_ID 
 colnames(all_data_clean)[which(names(all_data_clean) == "Monitoring_Site")] <- "EMS_ID"
## 
## 1) Murray River Watershed
  
murray <- filter(all_data_clean, EMS_ID == "0410060"| EMS_ID == "0410094"
                 | EMS_ID == "0410097"| EMS_ID == "E206227"| EMS_ID == "E206228"
                 | EMS_ID == "E206229"| EMS_ID == "E206232"| EMS_ID == "E206319"|
                   EMS_ID == "E206521"| EMS_ID == "E206526"| EMS_ID == "E206972")

Se_plot <- filter(murray, EMS_ID == "0410094"| EMS_ID == "E206227"| EMS_ID == "E206232"|
                      EMS_ID == "E206228"|EMS_ID == "E206526"|EMS_ID == "E206229"|
                      EMS_ID == "E206521", Variable == "Selenium Total")
  
  plot <- ggplot(Se_plot, aes(x = Date, y = Value, color = EMS_ID)) + 
  geom_point(size=1.5) +
  geom_hline(data = Se_plot, aes(yintercept = 2), colour = "red", linetype = "dashed", 
             show.legend = TRUE) +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  xlab("Date") +
  ylab("Total Selenium (µg/L)")
  plot(plot)
  
  #ggsave(filename = "Se_murr.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")
  
  
TNO3_plot <- filter(murray, EMS_ID == "0410094"| EMS_ID == "E206227", 
                      Variable == "Nitrogen NO3 Total", Date != "1983-10-04")
  
plot <- ggplot(TNO3_plot, aes(x = Date, y = Value, color = EMS_ID)) + 
    geom_point(size=1.5) +
    geom_hline(data = TNO3_plot, aes(yintercept = 3), colour = "red", linetype = "dashed", 
               show.legend = TRUE) +
    scale_x_date(labels = date_format("%b %Y")) +
    theme(legend.title=element_blank()) +
    xlab("Date") +
    ylab("Total Nitrate (mg/L)")
  plot(plot)
  
#ggsave(filename = "TNO3_murr.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


DNO3_plot <- filter(murray, EMS_ID == "E206319"| EMS_ID == "E206521"| EMS_ID == "E206972"|
                      EMS_ID == "E206526", Variable == "Nitrate (NO3) Dissolved", Date != "1985-09-18")
  
plot <- ggplot(DNO3_plot, aes(x = Date, y = Value, color = EMS_ID)) + 
    geom_point(size=1.5) +
        scale_x_date(labels = date_format("%b %Y")) +
    theme(legend.title=element_blank()) +
    xlab("Date") +
    ylab("Dissolved Nitrate (mg/L)")
  plot(plot)
  
#ggsave(filename = "DNO3_murr.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")

## 2) Pouce Coupe River Watershed

pc_N <- filter(pc, Variable == "Nitrate(NO3) + Nitrite(NO2) Dissolved")
plot <- ggplot(pc_N, aes(x = Date, y = Value, colour = Monitoring_Site, scales = "free_y")) + 
  #expand_limits(y = c(0, 1)) +
  geom_point(size=1.5) +
  #geom_hline(data = As_plot, aes(yintercept = UpperLimit), colour = "red", 
  # linetype = "dashed", show.legend = TRUE) +
  #ggtitle("Dissolved Nitrate + Nitrite","(mg/L)") + 
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  #scale_colour_discrete(name='Monitoring\nSite',
  #                      breaks=c("0410042","E206705","E206959","0410039"),
  #                    labels=c("0410042","E206705","E206959","0410039")) +
  xlab("Date") +
  ylab("Dissolved Nitrate + Nitrite (mg/L)")
plot(plot)

#ggsave(filename = "Pouce_Coupe_Nitrate+Nitrite.png", plot = plot, path = 'X', width = 7, height = 5, units= "in")


pc_T <- filter(pc, Variable == "Turbidity")
plot <- ggplot(pc_T, aes(x = Date, y = Value, scales = "free_y")) + 
  #expand_limits(y = c(0, 1)) +
  geom_point(size=1.5) +
  #geom_hline(data = As_plot, aes(yintercept = UpperLimit), colour = "red", 
  # linetype = "dashed", show.legend = TRUE) +
  ggtitle("Turbidity, NTU") + scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  #scale_colour_discrete(name='Monitoring\nSite',
  #                      breaks=c("0410042","E206705","E206959","0410039"),
  #                    labels=c("0410042","E206705","E206959","0410039")) +
  xlab("Date") +
  ylab("Value")
plot(plot)

#ggsave(filename = "Pouce_Coupe_Turbs.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


pc_ammon <- filter(pc, Variable == "Nitrogen Ammonia Dissolved")
plot <- ggplot(pc_ammon, aes(x = Date, y = Value, scales = "free_y")) + 
  #expand_limits(y = c(0, 1)) +
  geom_point(size=1.5,color = 'blue') +
  #geom_hline(data = As_plot, aes(yintercept = UpperLimit), colour = "red", 
  # linetype = "dashed", show.legend = TRUE) +
  ggtitle("Ammonia Nitrogen", "(mg/L)") + scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  #scale_colour_discrete(name='Monitoring\nSite',
  #                      breaks=c("0410042","E206705","E206959","0410039"),
  #                    labels=c("0410042","E206705","E206959","0410039")) +
  xlab("Date") +
  ylab("Value")
plot(plot)

#ggsave(filename = "Pouce_Coupe_ammonia.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")

#pc_Nitrate <- filter(pc, Variable == "Nitrate (NO3) Dissolved")
#plot <- ggplot(pc_Nitrate, aes(x = Date, y = Value, colour = Monitoring_Site)) + 
#expand_limits(y = c(0, 1)) +
# geom_point(size=1.5) +
#geom_hline(data = As_plot, aes(yintercept = UpperLimit), colour = "red", 
# linetype = "dashed", show.legend = TRUE) +
#ggtitle("Nitrate - pouce coupe", "mg/L") + scale_x_date(labels = date_format("%b %Y")) +
#theme(legend.title=element_blank()) +
#scale_colour_discrete(name='Monitoring\nSite',
#                      breaks=c("0410042","E206705","E206959","0410039"),
#                    labels=c("0410042","E206705","E206959","0410039")) +
# xlab("Date") +
#ylab("Value")
#plot(plot)


site0410039 <- filter(all_data_limits, Monitoring_Site == "0410039")

As_plot <- filter(site0410039, Variable == "Arsenic Total")

plot <- ggplot(As_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 1)) +
  geom_point(size=1.5, color = 'blue') +
  geom_hline(data = As_plot, aes(yintercept = Guideline), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  ggtitle("Total Arsenic", "ug/L") + scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  
  #scale_colour_discrete(name="Monitoring\nLocation",
  #breaks=c("0410028"),
  #labels=c("Upper Pine River")) +
  xlab("Date") +
  ylab("Value")
plot(plot)
#ggsave(filename = "Pouce_Coupe_Arsenic.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


## 3) Upper Peace River Watershed

## CU AND ZN PLOT
Zn_Cu_plot <- filter(all_data_limits, EMS_ID == "0400492"|EMS_ID == "0400134", Variable == "Copper Total"|Variable == "Zinc Total")
plot <- ggplot(Zn_Cu_plot, aes(x = Date, y = Value)) + 
  #expand_limits(y = c(0, 1)) +
  geom_point(size=1.5, color = 'blue') +
  geom_hline(data = Zn_Cu_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  ylab("Parameter Value (µg/L)")+
  facet_grid(Variable ~ EMS_ID, scales = "free") 

plot(plot)

#ggsave(filename = "Upper.Peace.River_CuZn.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


## IRON TOTAL PLOT

up_peace <- filter(all_data_clean, Monitoring_Site == "0400134"|Monitoring_Site == "0400492")

up_peace_Feplot <- filter(up_peace, Variable == "Iron Total")

plot <- ggplot(up_peace_Feplot, aes(x = Date, y = Value, colour = Monitoring_Site)) + 
  geom_point(size=1.5) +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  geom_hline(data = up_peace_Feplot, aes(yintercept = 1), colour = "red", linetype = "dashed") +
  xlab("Date") +
  ylab("Total Iron (mg/L)")
plot(plot)
#ggsave(filename = "uppeace_Fe.png", plot = plot, path = 'X', width = 7, height = 5, units= "in")


## TURB VS CU PLOT
up_peace_turb_cu <- filter(all_data_clean, Watershed == "Upper Peace River", Variable == "Copper Total"|Variable == "Turbidity")
up_peace_turb_cu <- spread(up_peace_turb_cu, key=Variable, value=Value)
colnames(up_peace_turb_cu)[which(names(up_peace_turb_cu) == "Copper Total")] <- "Copper_Total"

plot <- boxplot(Copper_Total~Turbidity, data=up_peace_turb_cu, main="Turbs vs Cu",
                xlab="Date", ylab="Value")
plot(plot)


## 6) Blueberry River Watershed

Al_plot <- filter(all_data_limits, Variable == "Aluminium Dissolved", EMS_ID == "E250094")

plot <- ggplot(Al_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5,colour = 'blue') +
  geom_hline(data = Al_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  #scale_colour_discrete(name="Parameter"),
  #                     breaks=c("Turbidity", "Total Iron")
  #                    labels=c("Turbidity", "Total Iron") +
  xlab("Date") +
  ylab("Dissolved Aluminum (mg/L)")

plot(plot)

#ggsave(filename = "blueberry_Al.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")

blue_Cu_plot <- filter(all_data_limits, Variable == "Copper Total", EMS_ID == "E250094")

plot <- ggplot(blue_Cu_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5,colour = 'blue') +
  geom_hline(data = blue_Cu_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  #scale_colour_discrete(name="Parameter"),
  #                     breaks=c("Turbidity", "Total Iron")
  #                    labels=c("Turbidity", "Total Iron") +
  xlab("Date") +
  ylab("Total Copper (µg/L)")

plot(plot)

#ggsave(filename = "blueberry_Cu.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")

blue_Fe_plot <- filter(all_data_clean, Variable == "Iron Total", Monitoring_Site == "E250094")

plot <- ggplot(blue_Fe_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5,colour = 'blue') +
  geom_hline(data = blue_Fe_plot, aes(yintercept = 1), colour = "red", linetype = "dashed") +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  #scale_colour_discrete(name="Parameter"),
  #                     breaks=c("Turbidity", "Total Iron")
  #                    labels=c("Turbidity", "Total Iron") +
  xlab("Date") +
  ylab("Total Iron (mg/L)")

plot(plot)

#ggsave(filename = "blueberry_Fe.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


blue_Cd_plot <- filter(all_data_limits, Variable == "Cadmium Dissolved", EMS_ID == "E250094")

plot <- ggplot(blue_Cd_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5,colour = 'blue') +
  geom_hline(data = blue_Cd_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  #scale_colour_discrete(name="Parameter"),
  #                     breaks=c("Turbidity", "Total Iron")
  #                    labels=c("Turbidity", "Total Iron") +
  xlab("Date") +
  ylab("Dissolved Cadmium (µg/L)")

plot(plot)

#ggsave(filename = "blueberry_Cd.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


## 8) Lower Beatton River Watershed

DAl_plot <- filter(all_data_limits, EMS_ID == "E207448"| EMS_ID == "E207449", Variable == "Aluminium Dissolved")

plot <- ggplot(DAl_plot, aes(x = Date, y = Value, colour = EMS_ID)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5) +
  geom_hline(data = DAl_plot, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  xlab("Date") +
  ylab("Dissolved Aluminum (mg/L)")
plot(plot)
ggsave(filename = "Al_lowbeat.png", plot = plot, path = 'D:/Work Docs/Report/Plots', width = 5, height = 5, units= "in")


TZn_plot <- filter(all_data_limits, EMS_ID == "E207448"| EMS_ID == "E207449", Variable == "Zinc Total")

plot <- ggplot(TZn_plot, aes(x = Date, y = Value, colour = EMS_ID)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5) +
  geom_hline(data = TZn_plot, aes(yintercept = UpperLimit, color = factor(EMS_ID)), linetype = "dashed", show.legend = TRUE) +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  xlab("Date") +
  ylab("Total Zinc (µg/L)")
plot(plot)

## 10) Lower Peace River Watershed

low_peace <- filter(all_data_clean, Monitoring_Site == "E206585")
low_peace_quad <- filter(low_peace, Variable == "Turbidity"|Variable =="Selenium Total"|Variable=="Thallium Total"|Variable=="Aluminum Total")

quadplot <- ggplot(low_peace_quad, aes(x = Date, y = Value)) + 
  geom_point() + 
  facet_wrap(Variable ~ Monitoring_Site, scales = "free_y")
print(quadplot)

## Comparing Up peace turbs to low peace turbs
peace_turbs <- filter(all_data_clean, Monitoring_Site == "0400492"|Monitoring_Site == "0400134", Variable == "Turbidity")
turb_plot <- ggplot(peace_turbs, aes(x = Date, y = Value, colour = Monitoring_Site)) + geom_point(alpha = 0.5)
print(turb_plot)

As_plot <- filter(all_data_limits, Variable == "Arsenic Total", EMS_ID == "E206585")

plot <- ggplot(As_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5,colour = 'blue') +
  geom_hline(data = As_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(date_breaks= "5 year", labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  
  xlab("Date") +
  ylab("Total Arsenic (µg/L)")
plot(plot)

#ggsave(filename = "As_lowpeace.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


TCu_plot <- filter(lowpeace, Variable == "Copper Total")
TCu_plot$UpperLimit <- NA
TCu_plot$UpperLimit[TCu_plot$Variable == "Copper Total"] <- 3.99

plot <- ggplot(TCu_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5, colour = "blue") +
  geom_hline(data = TCu_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(date_breaks= "5 year", labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  xlab("Date") +
  ylab("Total Copper (µg/L)")
plot(plot)

#ggsave(filename = "Cu_lowpeace.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


Pb_plot <- filter(low_peace, Variable == "Lead Total")
Pb_plot$UpperLimit <- NA
Pb_plot$UpperLimit[Pb_plot$Variable == "Lead Total"] <- 6.47

plot <- ggplot(Pb_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5,colour = 'blue') +
  geom_hline(data = Pb_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(date_breaks= "5 year", labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  xlab("Date") +
  ylab("Total Lead (µg/L)")
plot(plot)

#ggsave(filename = "Pb_lowpeace.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


Ag_plot <- filter(low_peace, Variable == "Silver Total")
Ag_plot$UpperLimit <- NA
Ag_plot$UpperLimit[Ag_plot$Variable == "Silver Total"] <- 0.05
plot <- ggplot(Ag_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5,colour = 'blue') +
  geom_hline(data = Ag_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(date_breaks= "5 year", labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  xlab("Date") +
  ylab("Total Silver (µg/L)")
plot(plot)

#ggsave(filename = "Ag_lowpeace.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


Al_plot <- filter(low_peace, Variable == "Aluminum Total")
plot <- ggplot(Al_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5, colour = 'blue') +
  scale_x_date(date_breaks= "5 year", labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  xlab("Date") +
  ylab("Total Aluminum (µg/L)")
plot(plot)

#ggsave(filename = "Al_lowpeace.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


## 12) Lynx Creek Watershed

lynx_Al <- filter(all_data_limits, EMS_ID == "E253393"|EMS_ID == "E253394")

lynx_Al_plot <- filter(lynx_Al, Variable == "Aluminium Dissolved")

plot <- ggplot(lynx_Al_plot, aes(x = Date, y = Value, colour = EMS_ID)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5) +
  geom_hline(data = lynx_Al_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  xlab("Date") +
  ylab("Dissolved Aluminum (mg/L)")
plot(plot)

#ggsave(filename = "Al_lynx.png", plot = plot, path = 'X', width = 7, height = 5, units= "in")


## 18) Middle Kiskatinaw River Watershed

Mid_kisk_Fe <- filter(all_data_clean, Monitoring_Site == "0400545", Variable == "Iron Total")
plot <- ggplot(Mid_kisk_Fe, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 1)) +
  geom_point(size=1.5,colour = 'blue') +
  scale_x_date(date_breaks= "2 year",labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  geom_hline(data = Mid_kisk_Fe, aes(yintercept = 1), colour = "red", linetype = "dashed", show.legend = TRUE) +
   xlab("Date") +
  ylab("Total Iron (mg/L)")
plot(plot)

#ggsave(filename = "mid_kisk_Fe.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


## 21) Lower Kiskatinaw River Watershed

low_kisk <- filter(all_data_clean, Monitoring_Site == "E228061"|Monitoring_Site == "E228062")

kisk_Fe_plot <- filter(low_kisk, Variable == "Iron Total")

plot <- ggplot(kisk_Fe_plot, aes(x = Date, y = Value, colour = Monitoring_Site)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5) +
  #geom_hline(data = Al_plot, aes(yintercept = UpperLimit), colour = "red", 
  #        linetype = "dashed", show.legend = TRUE) +
  #ggtitle("Total Iron", "mg/L") + 
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  #scale_colour_discrete(name="Parameter"),
  #                     breaks=c("Turbidity", "Total Iron")
  #                    labels=c("Turbidity", "Total Iron") +
  geom_hline(data = kisk_Fe_plot, aes(yintercept = 1), colour = "red", linetype = "dashed") +
  xlab("Date") +
  ylab("Total Iron (mg/L)")
plot(plot)

#ggsave(filename = "lowkisk_Fe.png", plot = plot, path = 'X', width = 7, height = 5, units= "in")


## 23) Lower Halfway River Watershed

low_half <- filter(all_data_clean, EMS_ID == "E249801")

Fe_plot <- filter(low_half, Variable == "Iron Total")

plot <- ggplot(Fe_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5,colour = 'blue') +
  #geom_hline(data = Al_plot, aes(yintercept = UpperLimit), colour = "red", 
  #        linetype = "dashed", show.legend = TRUE) +
  ggtitle("Total Iron", "mg/L") + 
  scale_x_date(labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  #scale_colour_discrete(name="Parameter"),
  #                     breaks=c("Turbidity", "Total Iron")
  #                    labels=c("Turbidity", "Total Iron") +
  xlab("Date") +
  ylab("Value")
plot(plot)


## 24) Milligan Creek Watershed

milligan <- filter(all_data_clean, Monitoring_Site == "E249804")
Al_plot <- filter(all_data_limits, EMS_ID == "E249804", Variable == "Aluminium Dissolved")
plot <- ggplot(Al_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 0.5)) +
  geom_point(size=1.5,colour = 'blue') +
  geom_hline(data = Al_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(date_breaks= "1 month", labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  xlab("Date") +
  ylab("Dissolved Aluminium (mg/L)")
plot(plot)

# ggsave(filename = "mill_Al.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


Zn_plot <- filter(all_data_limits, EMS_ID == "E249804", Variable == "Zinc Total")
plot <- ggplot(Zn_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 15)) +
  geom_point(size=1.5,colour = 'blue') +
  geom_hline(data = Zn_plot, aes(yintercept = UpperLimit), colour = "red", 
             linetype = "dashed", show.legend = TRUE) +
  scale_x_date(date_breaks= "1 month", labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  
  xlab("Date") +
  ylab("Total Zinc (µg/L)")
plot(plot)

#ggsave(filename = "mill_Zn.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")


Fe_plot <- filter(all_data_clean, Monitoring_Site == "E249804", Variable == "Iron Total")
plot <- ggplot(Fe_plot, aes(x = Date, y = Value)) + 
  expand_limits(y = c(0, 15)) +
  geom_point(size=1.5,colour = 'blue') +
  scale_x_date(date_breaks= "1 month", labels = date_format("%b %Y")) +
  theme(legend.title=element_blank()) +
  geom_hline(data = Fe_plot, aes(yintercept = 1), colour = "red", linetype = "dashed") +
  xlab("Date") +
  ylab("Total Iron (µg/L)")
plot(plot)

#ggsave(filename = "mill_Fe.png", plot = plot, path = 'X', width = 5, height = 5, units= "in")




## Facet plots for blueberry sites and parameters
blue2 <- all_data %>%
  filter(EMS_ID %in% c("E250094", "E257094"),
         Variable %in% c("Aluminum Dissolved", "Arsenic Total", "Barium Total", "Cadmium Dissolved", "Lead Total", 
                         "Molybdenum Total", "Phosphorus Total", "Selenium Total", "Temperature","Turbidity", "Iron Total"))
ggplot2::ggplot(data = blue2, aes(x = DateTime, y = Value)) +
  geom_point() +
  facet_grid(Variable ~ EMS_ID, scales = "free_y")

## Facet plots for lower beatton
lowbeat2 <- all_data %>%
  filter(EMS_ID %in% c("E207448", "E207449"),
         Variable %in% c("Arsenic Total", "Barium Total", "Cadmium Dissolved", "Lead Total", 
                         "Molybdenum Total", "Phosphorus Total", "Selenium Total", "Temperature","Turbidity", "Iron Total"))
ggplot2::ggplot(data = lowbeat2, aes(x = DateTime, y = Value)) +
  geom_point() +
  facet_grid(Variable ~ EMS_ID, scales = "free_y")
