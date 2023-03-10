library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
library(grid)
library(RColorBrewer)
library(colorspace)
# pal1 <- choose_palette()
# pal2 <- choose_palette()

zugversuche_daten <- read_excel("~/Desktop/Doktorarbeit/r_programming/EMMA-Paper/ZugversucheNeu.xlsx",
                                sheet = "Glucose_einfluss",
                                col_types = c("numeric",
                                              "numeric",
                                              "numeric")) %>% 
  mutate(large_group = if_else(Probe %in% c(2,5,6,7,8,9,10,11,12), true = "condition1", false = "condition2"))
zugversuche_daten$Probe <-as.factor(zugversuche_daten$Probe)
zugversuche_daten$Probe <- factor(zugversuche_daten$Probe, levels=c("2" , "5" , "6" , "7" , "8" , "9" , "10" ,"11" , "12" , "13", "14", "15", "16","17", "18",  "19" ,"20" ,"21"))



mypal2 <- colorRampPalette(brewer.pal(6, "Greens"))
mypal3 <- colorRampPalette(brewer.pal(6, "Purples"))

p_zugversuch <- zugversuche_daten%>%
  ggplot( aes(x=`Dehnung [%]`, y=`Standardkraft [N]`, group = Probe , colour = Probe , linetype = large_group
  ))+
  geom_line()+
  scale_colour_manual(values = c(mypal2(8), mypal3(10)))+
  theme_ipsum()+
  theme(
    legend.title=element_text(size=7), 
    legend.text=element_text(size=4)
  )+
  ggtitle(label = "Einlfuss von Glukose")
p_zugversuch

# ggsave(filename = "Dehnung_vs_SK_Glukose", plot = p_zugversuch, device = "tiff" , dpi = 300 ,
#        units = "cm" , width = 16.8 , height = 11)

