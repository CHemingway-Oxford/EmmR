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
klappen_daten_emmadeluxe <- read_excel("Desktop/klappen_daten_emmadeluxe.xlsx") %>% 
  mutate(pressure_gradient_indexed = pressure_gradient / EOF_max) %>% 
  mutate(EOF_ind = EOA / EOF_max) %>% 
  mutate(holder = factor(holder, levels = c('0','1','EI'))) %>% 
  mutate(Valve_n = factor(Valve_n, levels = unique(Valve_n))) %>% 
  mutate(rupture = factor(rupture, levels = unique(rupture))) %>% 
  filter(Valve_n != 6)
  # indexing of dP by maximum EOA:



# plot(klappen_daten_emmadeluxe$flow_rate,klappen_daten_emmadeluxe$pressure_gradient)


### plot the data with linear regression based on N-10 medium subset:
p_legend <- klappen_daten_emmadeluxe %>% 
  ggplot(
    aes(x = flow_rate , y = pressure_gradient_indexed , group = Valve_n , color = `PTFE_suture?` , shape = rupture)
  )+
  geom_point(alpha = 0.9, size = 4 , color = 'black' , stroke = 1.2)+
  geom_line(alpha = 1 , aes(linetype = holder), size = 1)+
  scale_shape_manual(values = c(NA,4),
                     labels = c('','ruptured'))+
  scale_colour_manual(values = colour_brewer_set2[1:3],
                      labels = c('No','Yes','Control'))+
  scale_linetype(labels = c('compliant','stiff','Control'))+
  # scale_colour_manual(values = colour_brewer_set2[1:3],
  #                     labels = c("N-10", "N-50","N-30","C-100"))+
  ylab(label = bquote('pressure gradient, normalised [mmHg/'~cm^2~']'))+
  xlab('systolic flow rate [mL / s]')+
  theme_classic() +
  ggtitle('B')+
  labs(color = 'PTFE sutured' , shape = '', linetype = 'holder used')+
  theme(legend.position = "right",
        plot.title = element_text(size=26),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        axis.ticks.y = element_blank(),
        # axis.line.y = element_blank(),
        axis.line.x = element_line(),
        panel.grid = element_blank(),
        panel.border = element_blank())

p_indPvsSFlowRate <- klappen_daten_emmadeluxe %>% 
  ggplot(
    aes(x = flow_rate , y = pressure_gradient_indexed , group = Valve_n , color = `PTFE_suture?` , shape = rupture)
  )+
  geom_point(alpha = 0.9, size = 4 , color = 'black' , stroke = 1.2)+
  geom_line(alpha = 1 , aes(linetype = holder), size = 1)+
  scale_shape_manual(values = c(NA,4),
                     labels = c('','ruptured'))+
  scale_colour_manual(values = colour_brewer_set2[1:3],
                      labels = c('No','Yes','Control'))+
  ylab(label = bquote('pressure gradient, normalised [mmHg/'~cm^2~']'))+
  xlab('systolic flow rate [mL / s]')+
  ggtitle('B')+
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=26),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  )
 
  
p_indEOAvsSFlowRate <- klappen_daten_emmadeluxe %>% 
  ggplot(
    aes(x = flow_rate , y = EOF_ind , group = Valve_n , color = `PTFE_suture?` , shape = rupture)
  )+
  geom_point(alpha = 0.9, size = 4 , color = 'black' , stroke = 1.2)+
  geom_line(alpha = 1 , aes(linetype = holder), size = 1)+
  scale_shape_manual(values = c(NA,4))+
  scale_colour_manual(values = colour_brewer_set2[1:3],
                      labels = c('No','Yes','Control'))+
  ylab('EOA normalised\n[a.u.]')+
  xlab('systolic flow rate [mL / s]')+
  ggtitle('A')+
  theme_classic()+
  theme(
    legend.position="none",
    plot.title = element_text(size=26),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  )

p_leakageVolumevsSFlowRate <- klappen_daten_emmadeluxe %>% 
  ggplot(
    aes(x = flow_rate , y = leakage_volume , group = Valve_n , color = `PTFE_suture?` , shape = rupture)
  )+
  geom_point(alpha = 0.9, size = 4 , color = 'black' , stroke = 1.2)+
  geom_line(alpha = 1 , aes(linetype = holder), size = 1)+
  scale_shape_manual(values = c(NA,4),
                     labels = c('','ruptured'))+
  scale_colour_manual(values = colour_brewer_set2[1:3],
                      labels = c('No','Yes','Control'))+
  ylab('leakage [mL]')+
  xlab('systolic flow rate [mL / s]')+
  ylim(0,60)+
  ggtitle('C')+
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=26),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  )

legend <- get_legend(
  # create some space to the left of the legend
  p_legend
)

pg_vsSFlowRate <- plot_grid(p_indPvsSFlowRate, p_indEOAvsSFlowRate, p_leakageVolumevsSFlowRate , nrow = 3)
pg_vsSFlowRate <- plot_grid(pg_vsSFlowRate, legend, nrow  = 2, rel_heights = c(9,2))

ggsave(path = "/Users/ChrisHemingway/Desktop/poster_emma", filename = "p_indPvsSFlowRate", plot = p_indPvsSFlowRate, device = "png" , dpi = 300 ,
       units = "cm" , width = 16.8 , height = 11)
ggsave(path = "/Users/ChrisHemingway/Desktop/poster_emma", filename = "p_indEOAvsSFlowRate", plot = p_indEOAvsSFlowRate, device = "png" , dpi = 300 ,
       units = "cm" , width = 16.8 , height = 11)
ggsave(path = "/Users/ChrisHemingway/Desktop/poster_emma", filename = "p_leakageVolumevsSFlowRate", plot = p_leakageVolumevsSFlowRate, device = "png" , dpi = 300 ,
       units = "cm" , width = 16.8 , height = 11)
ggsave(path = "/Users/ChrisHemingway/Desktop/poster_emma", filename = "p_legend", plot = p_legend, device = "png" , dpi = 300 ,
       units = "cm" , width = 16.8 , height = 16)

pg_vsSFlowRate