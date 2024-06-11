##### Nurse analysis script #####
##### Packages #####
options(scipen = 999)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggthemr)
library(openxlsx)
library(magrittr)
library(stringr)
library(curl)
library(ggrepel)
library(readxl)
library(RColorBrewer)
library(sf)
library(qicharts2)
library(showtext)
library(scales)
library(patchwork)
library(chron)
library(readODS)
library(lubridate)
library(sf)
library(geojsonsf)
library(gginnards)
library(rio)
library(janitor)
library(data.table)
library(formattable)
library(flextable)
library(reshape2)
######### Section 1 - RUN BHF STYLE FUNCTIONS########

##ADD FONTS##

#Beats  Will need to update local file location for .otf font files

font_add("bhf_beats_bold", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Beats/OTF/BHFBeats-Bold.otf")
font_add("bhf_beats_light", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Beats/OTF/BHFBeats-Light.otf")
font_add("bhf_beats_reg", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Beats/OTF/BHFBeats-Regular.otf")
font_add("bhf_ginger_bold", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Ginger/OTF/F37Ginger-Bold.otf")
font_add("bhf_ginger_light", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Ginger/OTF/F37Ginger-Light.otf")
font_add("bhf_ginger_reg", "C:/Users/almondth/OneDrive - British Heart Foundation/Documents/R/Fonts/Ginger/OTF/F37Ginger-Regular.otf")
showtext_auto()

t_font <- list(family = "bhf_ginger_reg", size = 14)

#If you are using showtext in RMarkdown documents you don’t have to use showtext_auto(). 
#That will set up the wrong dpi and the text will look too small. 
#You need to add fig.showtext=TRUE to the chunk settings.

###SET COLOUR PALETTES##

bhf_colours <- c(
  `Bright Red` = "#FF0030",
  `Dark Red` = "#8C0032",
  `Medium Red` = "#D20019",
  `Rubine Red` = "#E71348",
  `Light Blue` = "#2D91FF",
  `Indigo` = "#500AB4",
  `Pinkish` = "#FF3C64",
  `Orange` = "#FF873C",
  `Yellow` = "#FFBE32",
  `Light green` = "#19D79B",
  `Dark green` = "#00A06E",
  `Dark grey` = "#474E5A",
  `White` = "#FFFFFF"
)

bhf_cols <- function(...) {
  cols <- c(...)
  if(is.null(cols))
    return(bhf_colours)
  bhf_colours[cols]
}

#Define palettes

bhf_palettes <- list(
  `reds` = bhf_cols("Bright Red","Dark Red","Rubine Red", "Medium Red"),
  `not reds` = bhf_cols("Bright Red","Light Blue","Indigo"),
  `gradient_1` = bhf_cols("Dark Red","Medium Red"),
  `gradient_2` = bhf_cols("Medium Red","Bright Red"),
  `gradient_3` = bhf_cols("Bright Red","Rubine Red"),
  `gradient_4` = bhf_cols("Bright Red","White"),
  `secondaries` = bhf_cols("Light Blue", "Indigo","Pinkish",
                           "Orange","Yellow","Light green",
                           "Dark green","Dark grey"),
  `expanded secondaries` = bhf_cols("Bright Red", "Light Blue", "Indigo","Pinkish",
                                    "Orange","Yellow","Light green",
                                    "Dark green","Dark grey"),
  `red and light blue` = bhf_cols("Bright Red", "Light Blue"),
  `red, blue, indigo` = bhf_cols("Bright Red", "Light Blue", "Indigo")
)

bhf_pal<- function(palette = "reds", reverse = FALSE, ...) {
  pal <- bhf_palettes[[palette]]
  if(reverse) pal <- rev(pal)
  colorRampPalette(pal,...)
}

#Create scale_colour and scale_fill functions

scale_color_bhf <- function(palette = "reds", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


scale_fill_bhf <- function(palette = "reds", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


scale_fill_bhf_cont <- function(palette = "reds", discrete = FALSE, reverse = TRUE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

##BUILD FORMATTING FUNCTION##


#BHF everything 

bhf_style <- function (bhf_brand,textsize=10) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 #legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"))#,
  #axis.line = ggplot2::element_blank(), 
  #panel.grid.minor = ggplot2::element_blank(), 
  # panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
  #panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
  #strip.background = ggplot2::element_rect(fill = "white"), 
  #strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_map_style <- function (bhf_brand,textsize=10) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 #legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 # panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white")) 
  #strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

#### RISK FACTORS
#smoking
smoking_wales <- smoking_wales %>% 
  pivot_longer(!Status, names_to = "Deprivation Quintile", values_to = "Percent")
smoking_wales$Status<-factor(smoking_wales$Status,levels=c("Smoker","Ex-smoker","Never smoked"))
smoking_plot <- ggplot(smoking_wales, aes(`Deprivation Quintile`,Percent,fill=`Deprivation Quintile`))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  scale_fill_bhf(palette ='red and light blue') +
  bhf_style() +
  labs(title="Smoking habits of adults living in Wales by deprivation quintile in 2022-23", y="Percent (%)",caption="Data Source: StatsWales",subtitle = "1=most deprived, 5=least deprived")+
  facet_wrap(~Status)+
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        plot.subtitle = element_text(size = 15),
        strip.text.x = element_text(size = 16))+
  geom_text(aes(label = `Percent`), vjust = -0.3,size=5) +
  theme(legend.position = "none") 
smoking_plot

#alcohol

alcohol_wales <- alcohol_wales %>% 
  pivot_longer(!`Average weekly alcohol consumption`, names_to = "Deprivation Quintile", values_to = "Percent")
alcohol_wales <- alcohol_wales %>%
  filter(`Average weekly alcohol consumption` != "Hazardous (over 14 units, up to 50 (m) / 35 (f))")

alcohol_wales$`Average weekly alcohol consumption`<-factor(alcohol_wales$`Average weekly alcohol consumption`,levels=c("None","Up to 14 units (moderate drinkers)","Above 14 units (over guidelines)","Hazardous (over 14 units, up to 50 (m) / 35 (f))","Harmful (over 50 (m) / 35 (f) units)"))
alcohol_plot <- ggplot(alcohol_wales, aes(`Deprivation Quintile`,Percent,fill=`Deprivation Quintile`))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  scale_fill_bhf(palette ='red and light blue') +
  bhf_style() +
  labs(title="Weekly alcohol consumption of adults living in Wales by deprivation quintile in 2022-23", y="Percent (%)",caption="Data Source: StatsWales", subtitle="1=most deprived, 5=least deprived")+
  facet_wrap(~`Average weekly alcohol consumption`)+
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        plot.subtitle = element_text(size = 15),
        strip.text.x = element_text(size = 16))+
  geom_text(aes(label = `Percent`), vjust = -0.3,size=5)+
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(0,75))
alcohol_plot

alcohol_wales2<-alcohol_wales%>%
  filter(`Average weekly alcohol consumption`=="None"|`Average weekly alcohol consumption`=="Up to 14 units (moderate drinkers)"|`Average weekly alcohol consumption`==	
           "Above 14 units (over guidelines)")
alcohol_plot2 <- ggplot(alcohol_wales2, aes(`Deprivation Quintile`,Percent,fill=`Deprivation Quintile`))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  scale_fill_bhf(palette ='red and light blue') +
  bhf_style() +
  labs(title="Weekly alcohol consumption of adults living in Wales by deprivation quintile in 2022-23", y="Percent (%)",caption="Data Source: StatsWales", subtitle="1=most deprived, 5=least deprived")+
  facet_wrap(~`Average weekly alcohol consumption`)+
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        plot.subtitle = element_text(size = 15),
        strip.text.x = element_text(size = 16))+
  geom_text(aes(label = `Percent`), vjust = -0.3,size=5)+
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(0,75))
alcohol_plot2
#diet
diet_wales <- diet_wales %>% 
  pivot_longer(!`Portions in previous day`, names_to = "Deprivation Quintile", values_to = "Percent")
diet_plot <- ggplot(diet_wales, aes(`Deprivation Quintile`,Percent,fill=`Deprivation Quintile`))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  scale_fill_bhf(palette ='red and light blue') +
  bhf_style() +
  labs(title="Fruit and vegetable consumption of adults living in Wales in the previous day, 
       by deprivation quintile in 2022-23", y="Percent (%)",caption="Data Source: StatsWales", subtitle="1=most deprived, 5=least deprived")+
  facet_wrap(~`Portions in previous day`)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14)) +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        plot.subtitle = element_text(size = 17),
        strip.text.x = element_text(size = 18))+
  geom_text(aes(label = `Percent`), vjust = -0.3,size=5)+
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(0,75))
diet_plot

#physical exercise
physical_wales <- physical_wales %>% 
  pivot_longer(!`Activity level in previous week`, names_to = "Deprivation Quintile", values_to = "Percent")
physical_wales$`Activity level in previous week`<-factor(physical_wales$`Activity level in previous week`,levels=c("Less than 30 minutes","30-149 minutes","At least 150 minutes"))
physical_plot <- ggplot(physical_wales, aes(`Deprivation Quintile`,Percent,fill=`Deprivation Quintile`))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  scale_fill_bhf(palette ='red and light blue') +
  bhf_style() +
  labs(title="Levels of physical activity of adults living in Wales in the previous week, 
       by deprivation quintile in 2022-23", y="Percent (%)",caption="Data Source: StatsWales", subtitle="1=most deprived, 5=least deprived")+
  facet_wrap(~`Activity level in previous week`)+
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        plot.subtitle = element_text(size = 15),
        strip.text.x = element_text(size = 16))+
  geom_text(aes(label = `Percent`), vjust = -0.3,size=5)+
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(0,75))

physical_plot

#bmi
bmi_wales <- bmi_wales %>% 
  pivot_longer(!BMI, names_to = "Deprivation Quintile", values_to = "Percent")
bmi_wales<-bmi_wales%>%
  filter(BMI=="Overweight (BMI=25 - under 30)"|BMI=="Obese (BMI=30+)")
#bmi_wales$BMI<-factor(bmi_wales$BMI, levels=c("under 18.5","18.5 - under 25","25 - under 30","30+"))
bmi_wales$BMI <-factor(bmi_wales$BMI,levels=c("Overweight (BMI=25 - under 30)","Obese (BMI=30+)"))
bmi_plot <- ggplot(bmi_wales, aes(`Deprivation Quintile`,Percent,fill=`Deprivation Quintile`))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  scale_fill_bhf(palette ='red and light blue') +
  bhf_style() +
  labs(title="BMI levels of adults living in Wales by deprivation quintile in 2022-23", y="Percentage (%)",caption="Data Source: StatsWales", subtitle="1=most deprived,5=least deprived")+
  facet_wrap(~BMI)+
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        plot.subtitle = element_text(size = 15),
        strip.text.x = element_text(size = 16))+
  geom_text(aes(label = `Percent`), vjust = -0.3,size=5)+
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(0,75))
bmi_plot

#general cvd prevalence
prev_wales <- prev_wales %>% 
  pivot_longer(!cond, names_to = "Deprivation Quintile", values_to = "Percent")
prev_plot <- ggplot(prev_wales, aes(`Deprivation Quintile`,Percent,fill=`Deprivation Quintile`))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  scale_fill_bhf(palette ='not reds') +
  bhf_style() +
  labs(title="Percentage of those in Wales with a heart and circulatory complaint in 2022-23", y="Percent (%)",caption="Data Source: National Survey for Wales")+
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        plot.subtitle = element_text(size = 15),
        strip.text.x = element_text(size = 16))+
  theme(legend.position = "none") +
  geom_text(aes(label = scales::percent(Percent, accuracy = 0.1)), vjust = -0.3, size = 5) +
  scale_y_continuous(label = scales::percent, limits = c(0,1))
prev_plot

#healthy life expectancy at birth
life_plot <- ggplot(life_wales, aes(x=`Year`, y=`Healthy life expectancy`,color=`Deprivation Quintile`,group=`Deprivation Quintile`))+
  geom_line(size = 2.6)+
  geom_point(size=3.5)+
  bhf_style()+
  scale_color_bhf(palette ='red and light blue', name="Deprivation Quintile") +
  facet_wrap(~Gender)+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(limits = c(0,75))+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 16),
        axis.title.y= element_text(size = 14),
        plot.title= element_text(size = 18),
        strip.text.x = element_text(size = 16),
        legend.text=element_text(size=15),
        plot.subtitle = element_text(size = 15))+
  labs(title="Healthy life expectancy at birth by deprivaiton quintile in Wales in 2018-2020", caption="Data source: Public Health Wales")
life_plot
  

#Primary care in wales

gp_wales<-gp_wales %>%
  pivot_longer(!`Deprivation Quintile`,names_to ="Category",values_to = "Number of providers" )
gp_wales$`Deprivation Quintile`<-as.factor(gp_wales$`Deprivation Quintile`)
gp_wales_plot<-ggplot(gp_wales,aes(`Deprivation Quintile`, `Number of providers`, fill=`Deprivation Quintile`))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  facet_wrap(~Category)+
  scale_fill_bhf(palette = 'red and light blue') +
  bhf_style() +
  geom_text(aes(label = `Number of providers`), vjust = -0.3,size=5) +
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 16),
        axis.title.y= element_text(size = 16),
        plot.title= element_text(size = 18),
        strip.text.x = element_text(size = 16),
        legend.text=element_text(size=15),
        plot.subtitle = element_text(size = 15))+
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(0,1000))+
  labs(title="Full-time equivalents of general practice workforce by cluster deprivation quintile in April 2023", subtitle="1=most deprived, 5=least deprived", caption="Data Source: StatsWales")
gp_wales_plot


#prescriptions
pres_wales$`Deprivation Quintile`<-factor(pres_wales$`Deprivation Quintile`,levels=c("1","2","3","4","5"))
pres_wales$`2021-22`<-as.numeric(pres_wales$`2021-22`)
pres_wales<-pres_wales%>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Cost")

pres_plot <- ggplot(pres_wales, aes(x = Year, y = Cost, color = `Deprivation Quintile`, group = Cluster)) +
  geom_point(size = 3) +
  geom_line(size = 2.3) +
  scale_color_bhf(palette = 'red and light blue') +
  facet_wrap(~`Deprivation Quintile`)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Cost of cardiovascular disease prescriptions in Wales by GP cluster and deprivation quintile",
       y = "Prescription cost",
       caption = "Data Source: StatsWales") +
  theme(plot.caption.position = "plot",
        axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 15))

pres_plot




#referrals ref_wales
ref_wales<-ref_wales%>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Referals")
ref_plot<- ggplot(ref_wales, aes(`Percentage of patients living in most deprived 20% of WIMD`, Referals,color=`Percentage of patients living in most deprived 20% of WIMD`))+
  geom_point(size=4)+
  facet_wrap(~Year)+
  #scale_color_bhf(palette ='red and light blue')+
  bhf_style() +
  labs(title = "Number of Cardiology refereals in December of 2020-23 by percentage of patients living in the most deprived 20% of WIMD",y="Number of Referals per 1000 population",caption="Data Source: StatsWales" )+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))+
  theme(legend.position = "none") 
ref_plot
#prevalence of specific conditions (prev3)
prev3_wales<-prev3_wales%>%
  pivot_longer(!`Deprivation Quintile`, names_to="conditions",values_to="Prevalence")
prev3_wales$`Deprivation Quintile`<-as.factor(prev3_wales$`Deprivation Quintile`)
prev3_plot<-ggplot(prev3_wales,aes(`Deprivation Quintile`, Prevalence, fill=`Deprivation Quintile`))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  facet_wrap(~conditions)+
  bhf_style() +
  scale_fill_bhf(palette ='red and light blue')+
labs(title="Prevalence of Cardiovascular conditions and risk factors in Wales by GP cluster deprivation quintile in 2021-22",x="Deprivation Quintile",y="Prevalence",caption="Data Source: QAIF Prevalence Data Wales", subtitle="1=most deprived, 5=least deprived")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))+
  theme(legend.position = "none") +
  geom_text(aes(label = scales::percent(Prevalence, accuracy = 0.1)), vjust = -0.3, size = 5) +
  scale_y_continuous(label = scales::percent, limits = c(0,0.3))
prev3_plot

##time series riskfactors
smoking2_wales$`Deprivation Quintile`<-as.factor(smoking2_wales$`Deprivation Quintile`)
smoking2_wales$Year<-factor(smoking2_wales$Year,levels=c("2020-21","2020-21 Q4","2021-22","2022-23"))
smoking2_wales<-smoking2_wales%>%
  filter(Year!="2020-21 Q4")
smoking2_plot<-ggplot(smoking2_wales, aes(Year,Prevalence, color=`Deprivation Quintile`,alpha=`Deprivation Quintile`, group=`Deprivation Quintile`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  scale_color_bhf(palette = "red and light blue")+
  scale_alpha_manual(values=c(1,0.4,0.4,0.4, 1), guide = "none") +
  labs(title="Smoking prevalence in Wales from 2020-2023 by depriation quintile", y="Prevalence (%)", caption="Data source:StatsWales")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))+
  scale_y_continuous(limits = c(0,50))+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
smoking2_plot

alcohol2_wales$`Deprivation Quintile`<-as.factor(alcohol2_wales$`Deprivation Quintile`)
alcohol2_wales$Year<-factor(alcohol2_wales$Year,levels=c("2020-21 Q4","2021-22","2022-23"))
alcohol2_plot<-ggplot(alcohol2_wales, aes(Year,Prevalence, color=`Deprivation Quintile`,alpha=`Deprivation Quintile`, group=`Deprivation Quintile`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  scale_color_bhf(palette = "red and light blue")+
  scale_alpha_manual(values=c(1,0.4,0.4,0.4, 1), guide = "none") +
  labs(title="Percentage of those who drink more than guidelines (above 14 units) 
  in Wales by deprivation quintile from 2020-23", y="Percent (%)", caption="Data source:StatsWales")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))+
  scale_y_continuous(limits = c(0,50))+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
alcohol2_plot

diet2_wales$`Deprivation Quintile`<-as.factor(diet2_wales$`Deprivation Quintile`)
diet2_wales$Year<-factor(diet2_wales$Year,levels=c("2020-21 Q4","2021-22","2022-23"))
diet2_plot<-ggplot(diet2_wales, aes(Year,Prevalence, color=`Deprivation Quintile`,alpha=`Deprivation Quintile`, group=`Deprivation Quintile`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  scale_color_bhf(palette = "red and light blue")+
  scale_alpha_manual(values=c(1,0.4,0.4,0.4, 1), guide = "none") +
  labs(title="Percentage of those who ate less than 5 fruits and vegetables in the previous day in Wales
  by deprivation quintile from 2020-23", y="Percent (%)", caption="Data source:StatsWales")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))+
  scale_y_continuous(limits = c(0,80))+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
diet2_plot

diet2_wales$`Deprivation Quintile`<-as.factor(diet2_wales$`Deprivation Quintile`)
diet2_wales$Year<-factor(diet2_wales$Year,levels=c("2020-21 Q4","2021-22","2022-23"))
diet2_plot<-ggplot(diet2_wales, aes(Year,Prevalence, color=`Deprivation Quintile`,alpha=`Deprivation Quintile`, group=`Deprivation Quintile`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  scale_color_bhf(palette = "red and light blue")+
  scale_alpha_manual(values=c(1,0.4,0.4,0.4, 1), guide = "none") +
  labs(title="Percentage of those who ate less than 5 fruits and vegetables in the previous day in Wales by deprivation quintile from 2020-23", y="Percent (%)", caption="Data source:StatsWales")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))+
  scale_y_continuous(limits = c(0,80))+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
diet2_plot

physical2_wales<-bmi2_wales
physical2_wales$`Deprivation Quintile`<-as.factor(physical2_wales$`Deprivation Quintile`)
physical2_wales$Year<-factor(physical2_wales$Year,levels=c("2020-21 Q4","2021-22","2022-23"))
physical2_plot<-ggplot(physical2_wales, aes(Year,Prevalence, color=`Deprivation Quintile`,alpha=`Deprivation Quintile`, group=`Deprivation Quintile`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  scale_color_bhf(palette = "red and light blue")+
  scale_alpha_manual(values=c(1,0.4,0.4,0.4, 1), guide = "none") +
  labs(title="Percentage of those meeting the 150 minute per week physical activity target 
  by deprivation quintile from 2020-23", y="Percent (%)", caption="Data source:StatsWales")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))+
  scale_y_continuous(limits = c(0,70))+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
physical2_plot

bmi2_wales$`Deprivation Quintile`<-as.factor(bmi2_wales$`Deprivation Quintile`)
bmi2_wales$Year<-factor(bmi2_wales$Year,levels=c("2020-21 Q4","2021-22","2022-23"))
bmi2_plot<-ggplot(bmi2_wales, aes(Year,Prevalence, color=`Deprivation Quintile`,alpha=`Deprivation Quintile`, group=`Deprivation Quintile`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  scale_color_bhf(palette = "red and light blue")+
  scale_alpha_manual(values=c(1,0.4,0.4,0.4, 1), guide = "none") +
  labs(title="Percentage of those classified as overweight or obeses (BMI of 25+) 
in Wales, by deprivation quintile from 2020-23", y="Percent (%)", caption="Data source:StatsWales")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))+
  scale_y_continuous(limits = c(0,75))+
  scale_x_discrete(guide = guide_axis(angle = 45)) 
bmi2_plot


##admissions
admin_wales1<-admin_wales
  #filter(Category=="Admissions"|Category=="Female"|Category=="Male")
admin_wales1$`LHB rank`<-factor(admin_wales1$`LHB rank`,levels=c("1","2","3","4","5","6"))
admin_wales1$Year<-as.factor(admin_wales1$Year)
admin_wales_plot<-ggplot(admin_wales1, aes(Year, rate, alpha=`LHB rank`,color=`LHB rank`,group=`LHB rank`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  facet_wrap(~Category)+
  scale_color_bhf(palette = "red and light blue")+
  scale_alpha_manual(values=c(1,0.4,0.4,0.4,0.4, 1), guide = "none") +
  labs(title="Cardiology admissions rate per 1,000 by Wales local health board and gender from 2018-23", y="Admission rate", caption="Data sources: NHS Cymru and StatsWales
       *Powys Teaching Health Board excluded as Cardiology admissions were not reported.", subtitle="Local Health Board rank where 1=most deprived and 6=least deprived")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))
admin_wales_plot

admin_wales2<-admin_wales%>%
  filter(Category=="Emergency")
admin_wales2$`LHB rank`<-factor(admin_wales2$`LHB rank`,levels=c("1","2","3","4","5","6"))
admin_wales2$Year<-as.factor(admin_wales2$Year)
admin_wales_plot2<-ggplot(admin_wales2, aes(Year, rate, alpha=`LHB rank`,color=`LHB rank`,group=`LHB rank`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  scale_color_bhf(palette = "red and light blue")+
  scale_alpha_manual(values=c(1,0.4,0.4,0.4,0.4, 1), guide = "none") +
  labs(title="Cardiology emergency admissions rate per 1,000 by Wales local health board from 2018-23", y="Admission rate", caption="Data sources: NHS Cymru and StatsWales
       *Powys Teaching Health Board excluded as Cardiology admissions were not reported.", subtitle="Local Health Board rank where 1=most deprived and 6=least deprived")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))
admin_wales_plot2

##mortality 2 over time
mortality2_wales$`LA Rank`<-factor(mortality2_wales$`LA Rank`,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22"))
mortality2_wales$Year<-factor(mortality2_wales$Year,levels=c("2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"))
asmr_plot <-ggplot(mortality2_wales, aes(Year,ASMR, color=`LA Rank`,alpha=`LA Rank`,group=`LA Rank`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  scale_alpha_manual(values=c(1,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3, 1), guide = "none") +
  facet_wrap(~Gender)+
  scale_color_bhf(palette = "red and light blue")+
  labs(title="Age standardised mortality rate (ASMR) per 100,000 population for heart and circulatory diseases in Wales 
  by local authority from 2013-2022", y="ASMR per 100,000", caption="Data Source:ONS and NOMIS", subtitle="LA rank where 1=most deprived and 22=least deprived")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))
asmr_plot

asmru75_plot <-ggplot(mortality2_wales, aes(Year,`ASMR (<75s)`, color=`LA Rank`,alpha=`LA Rank`,group=`LA Rank`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  scale_alpha_manual(values=c(1,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3, 1), guide = "none") +
  facet_wrap(~Gender)+
  scale_color_bhf(palette = "red and light blue")+
  labs(title="Premature age standardised mortality rate (ASMR) per 100,000 population for heart and circulatory diseases in Wales 
  by local authority from 2013-2022", y="ASMR per 100,000", caption="Data Source:ONS and NOMIS", subtitle="LA rank where 1=most deprived and 22=least deprived")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))
asmru75_plot


pres2_wales<-pres2_wales%>%
  pivot_longer(!`Deprivation Quintile (cluster)`, names_to="Year", values_to="rate")
pres2_wales$Year<-as.factor(pres2_wales$Year)
pres2_wales$`Deprivation Quintile (cluster)`<-as.factor(pres2_wales$`Deprivation Quintile (cluster)`)
pres2_plot<-ggplot(pres2_wales, aes(Year, rate, color=`Deprivation Quintile (cluster)`, group=`Deprivation Quintile (cluster)`, alpha=`Deprivation Quintile (cluster)`))+
  geom_point(size = 3)+
  geom_line(size = 2.6)+
  bhf_style() +
  scale_alpha_manual(values=c(1,0.4,0.4,0.4,1))+
  scale_color_bhf(palette = "red and light blue")+
  scale_y_continuous(labels = comma)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  labs(title = "Cost of cardiovascular disease prescription in Wales per 100,000 
       by GP cluster and deprivation quintile from 2016-2022",
       y = "Prescription cost per 100,000",
       caption = "Data Source: StatsWales")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))

pres2_plot


##mortality 3 over time (averages)
mort_avg_wales$Year<-factor(mort_avg_wales$Year,levels=c("2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"))
asmr_plot <-ggplot(mort_avg_wales, aes(Year,ASMR, color=`Deprivation`,group=`Deprivation`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  facet_wrap(~Gender)+
  scale_color_bhf(palette = "red and light blue")+
  labs(title="Average age standardised mortality rate (ASMR) per 100,000 population for heart and circulatory diseases 
  for the 5 most and least deprived local authorities in Wales from 2013-2022", y="ASMR per 100,000", caption="Data Source:ONS and NOMIS", subtitle="Deprivation where 1=most deprived and 22=least deprived")+
  theme(plot.caption.position = "plot")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14)) +
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))
asmr_plot

asmru75_plot <-ggplot(mort_avg_wales, aes(Year,`ASMR (<75s)`, color=`Deprivation`,group=`Deprivation`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  facet_wrap(~Gender)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14)) +
  scale_color_bhf(palette = "red and light blue")+
  labs(title="Average premature age standardised mortality rate (ASMR) per 100,000 population for heart and circulatory diseases 
 for the 5 most and least deprived local authorities in Wales from 2013-2022", y="ASMR per 100,000", caption="Data Source:ONS and NOMIS", subtitle="Deprivation where 1=most deprived and 22=least deprived")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))
asmru75_plot
  
#mortality 3yr average
mort_3yr_wales$Year<-factor(mort_3yr_wales$Year,levels=c("2013-2015","2014-2016","2015-2017","2016-2018","2017-2019","2018-2020","2019-2021","2020-2022"))
mort_3yr_wales$Deprivation<-factor(mort_3yr_wales$Deprivation,levels=c("Most deprived","Least deprived"))
asmr_plot <-ggplot(mort_3yr_wales, aes(Year,ASMR, color=`Deprivation`,group=`Deprivation`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  facet_wrap(~Gender)+
  scale_color_bhf(palette = "red and light blue")+
  labs(title="Average age standardised mortality rate (ASMR) per 100k for CVD for 
  for the 5 most and least deprived local authorities in Wales from 2013-2022", y="ASMR per 100,000", caption="Data Source:ONS and NOMIS",  x="3-year period (average)")+
  theme(plot.caption.position = "plot")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14)) +
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))
asmr_plot

asmru75_plot <-ggplot(mort_3yr_wales, aes(Year,`ASMR (<75s)`, color=`Deprivation`,group=`Deprivation`))+
  geom_point(size = 3) +
  geom_line(size = 2.6)+
  bhf_style() +
  facet_wrap(~Gender)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=14)) +
  scale_color_bhf(palette = "red and light blue")+
  labs(title="Average premature age-standardised mortality rate (ASMR) per 100k 
       for Wales's 5 most and least deprived local authorities for CVD from 2013-2022", caption="Data Source:ONS and NOMIS", x="3-year period (average)")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 16),
        plot.subtitle= element_text(size = 15))
asmru75_plot