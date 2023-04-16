#Load libraries for plotting
library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
library(grid)
library(cowplot)

#Load and clean Data 
DatenEmma <- read_excel("Desktop/Medien_Vgl_Dicken-Gewichte_Pressure.xlsx", 
                       sheet = "max Pressure", col_types = c("skip", 
                                                             "numeric", "skip", "numeric", "skip", 
                                                             "numeric", "skip"))



# View(DatenEmma)
# are these the correct colors?
colour_palette <- c('#66c2a5' , '#fc8d62' , '#8da0cb' , '#e78ac3')

data_lollipop <- DatenEmma %>% 
  select(Pressure_max_mmHg, Rupture, Medium) %>% 
  drop_na(Pressure_max_mmHg) %>%
  # mutate(ID = c(1:24))
  mutate(Medium=factor(Medium, levels=c('333','454510','5050'))) %>% 
  mutate(Rupture=factor(Rupture, levels= (c("0","1"))))
data_lollippop_mean <- data_lollipop %>% group_by(Medium, Rupture) %>% 
  summarize(mean_preassure = mean(Pressure_max_mmHg)) %>% 
  mutate(Medium=factor(Medium, levels=c('1','2','3'))) %>% 
  mutate(Medium=as.numeric(Medium))


# data_lollipop <-  droplevels(data_lollipop[!data_lollipop$Medium == '5050',])

#create horizontal lollipop Plot
p_lolli <- data_lollipop %>%
  ggplot( aes(x=Medium, y=Pressure_max_mmHg)) +
  #geom_segment(aes(xend=ID, yend=400) , size  = 0.6) +
  geom_jitter(aes(shape = Rupture , color = Medium) ,  size=3 , stroke = 1, width = 0.3
              
              ) +
  stat_summary(data = data_lollippop_mean, fun = mean, geom = "errorbarh", 
               aes(y = Medium, xmin = mean_preassure, xmax = mean_preassure), 
               color = "red", size = 1.5)+
  # geom_hline(data = data_lollippop_mean, )
  scale_shape_manual(values = c(20, 4) , labels  = c("No" , "Yes")) +
  scale_color_manual(values = colour_palette , labels=c("3|3|3" , "45|45|10" ))+
  scale_y_continuous(limits = c(750,1500) , breaks=c(600 , 800 , 1000,1200, 1400) ,
                     expand = c(0,0))+
  scale_x_discrete(breaks = NULL)+
  coord_flip()+
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        panel.grid = element_blank(),
        panel.border = element_blank()
        
  )+
  ylab("Pressure [mmHg]")+
  xlab("")+
  ggtitle(label = "Maximum Withstood Pressure")

# ggsave(path = "C/Users/ChrisHemingway/Desktop", filename = "Max_WithstoodPressure_MediumComp.png", plot = p_lolli, device = "png" , dpi = 300 ,
#        units = "cm" , width = 16.8 , height = 12)


#####generate common figure legend with uniform labelling:
legend <- get_legend(
  p_lolli + 
    theme(legend.box.margin = margin(
      2,6,2,6 , unit = "cm")
    ) +
    labs(color = "Test series" , shape = "Ruptured in test"))

ggsave(path = "C/Users/ChrisHemingway/Desktop", filename = "Max_WithstoodPressure_MediumComp.png", plot = p_lolli, device = "png" , dpi = 300 ,
       units = "cm" , width = 16.8 , height = 12)

p_lolli
