#Load libraries for plotting
library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
library(grid)
library(cowplot)
library(plyr)
library(tikzDevice)

# custom color palette
colour_brewer_set2 <- c('#66c2a5',
                        '#fc8d62',
                        '#8da0cb',
                        '#e78ac3')

#Load and clean Data 
DatenEmma_thickness <- read_excel("Desktop/Medien_Vgl_Dicken-Gewichte_Pressure.xlsx", 
                             sheet = "Thickness_foR", col_types = c("skip", 
                                                                    "numeric", "skip", "skip", "skip", 
                                                                    "skip", "skip", "skip", "numeric"),
                             range = 'A2:I34',
                             col_names = c('probe','thickness')) %>%
  drop_na() %>% 
  mutate(probe = paste0('Test ',probe))
view(DatenEmma_thickness)

DatenEmma_maxp <- read_excel("Desktop/Medien_Vgl_Dicken-Gewichte_Pressure.xlsx", 
                             sheet = "max Pressure", col_types = c("skip", 
                                                                   "numeric", "text", "numeric", "skip", 
                                                                   "numeric", "skip"),
                             col_names = c('medium','probe','pressure','rupture'),
                             range = 'A2:G31') %>% 
  drop_na() %>% 
  mutate(medium = factor(medium, levels = c('454510','333','5050')))
view(DatenEmma_maxp)

DatenEmma <- inner_join(DatenEmma_thickness, DatenEmma_maxp) %>% 
  filter(rupture == 1)
view(DatenEmma)

plot(DatenEmma$thickness,DatenEmma$pressure)

### generate a linear model:
linear_model <- lm(DatenEmma[DatenEmma$medium == '454510', ]$thickness ~ DatenEmma[DatenEmma$medium == '454510', ]$pressure)
R2.exp <- expression(paste(" ",R^2 ,"= 0.2467"))


### plot the data with linear regression based on N-10 medium subset:
p_regression_thickness_maxp <- DatenEmma[DatenEmma$medium == '454510', ] %>% 
  ggplot(
    aes(x = thickness , y = pressure , color = medium)
  )+
  geom_point( size = 3 , alpha = 1)+
  scale_colour_manual(values = colour_brewer_set2[1:3],
                      labels = c("N-10", "N-50","N-30","C-100"))+
  geom_smooth(data =DatenEmma[DatenEmma$medium == '454510', ],
    # aes(
    #       x = DatenEmma$thickness , y = DatenEmma$pressure
    #    )  , 
    method=lm , color="grey", fill="#69b3a2", se=TRUE)+
  geom_label(aes(x = 3.15 , y = 1350 , label = "atop(r^2 == 0.2467, 
                 \np == 0.082)"),
             color = 'black' , parse = T)+
  ylab('pressure [mmHg]')+
  xlab('thickness [mm]')+
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.ticks.y = element_blank(),
        # axis.line.y = element_blank(),
        axis.line.x = element_line(),
        panel.grid = element_blank(),
        panel.border = element_blank())


p_regression_thickness_maxp
print(summary(linear_model))
ggsave(path = "C/Users/ChrisHemingway/Desktop", filename = "regression_thickness_pressure.png", plot = p_regression_thickness_maxp, device = "png" , dpi = 300 ,
       units = "cm" , width = 16.8 , height = 12)
