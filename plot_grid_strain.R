library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library("ggrepel")       
library(hrbrthemes)
library(grid)
library(RColorBrewer)
library(colorspace)
library(cowplot)
# pal1 <- choose_palette()
# pal2 <- choose_palette()


#load custom colour scheme, and make custom colour palette
# colours <- read_excel("Desktop/HexCodes_zugversuche.xlsx",
#                       col_types = c('text','skip','text'),
#                        col_names = c('probe','groupl')) %>% 
#   mutate(probe = paste0('Probe ',probe))
# 

# colour_palette <- colours$`...2`[c(1,12,15)]
colour_brewer_set2 <- c('#66c2a5',
                        '#fc8d62',
                        '#8da0cb',
                        '#e78ac3')


# retrieve the thicknesses of CelluloseMediumVgl_2 WDH.xlsx: 
thickness_CelluloseMediumVgl_2_1  <- read_excel("Desktop/CelluloseMediumVglWdh2.1.xlsx", 
                                                sheet = "Ergebnisse", range = "A3:L29", 
                                                col_names = c('probe','dicke','groupl'),
                                                col_types = c("text", 
                                                                                 "skip", "skip", "skip", "skip", "skip", 
                                                                                 "skip", "skip", "numeric","skip", "skip","text")) %>% 
  drop_na()

##compile data from CelluloseMediumVgl_2 WDH.xlsx into one workable file.
#use tab names of experimental excel sheet to iterate.
zugversuche_daten_sheets <- excel_sheets("Desktop/CelluloseMediumVglWdh2.1.xlsx") 


# #read all experiment names from sheet 2 of experiment output file, 
# # maximum limit of experiments per file set to 998
# # omit all unused entries.
exp_type <- read_excel("Desktop/CelluloseMediumVglWdh2.1.xlsx",
                       sheet = 2,
                       range = 'A2:A1000',
                       col_names = 'probe') %>%
  na.omit()

# groupl_type <- read_excel("Desktop/CelluloseMediumVglWdh2.1.xlsx",
#                           sheet = 2,
#                           range = 'L2:L1000',
#                           col_names = 'probe') %>% 
#   na.omit()



#initiate data frame to be filled in for loop
zugversuche_datenFull <- data.frame(row.names = c("Dehnung", "Standardkraft", "probe","dicke","groupl"))

#iterate along the tab names to extract relevant experimental data,
# start at i=4, as first data is stored from sheet 4.
for (i in 4:length(zugversuche_daten_sheets)){
  # for (i in 4){
  #make an intermediate variable x that stores all data points within one experiment
  x <- read_excel("Desktop/CelluloseMediumVglWdh2.1.xlsx",
                  sheet = i,
                  skip = 4
                  ,col_types = c("numeric" , "numeric")
                  ,col_names = c("Dehnung", "Standardkraft")
  ) 
  
  #find the maximum of x, and remove all values that follow after < 80% of the maximum
  x <- x %>% filter(
    # Standardkraft<=(0.8 * max(x$Standardkraft)) &
    # x <- x %>% filter(Standardkraft >= 1.389771            
    Dehnung <= (Dehnung[which.max(Standardkraft)] + 0.5)
  )
  #add $probe with experiment name and $group with experimental group for colour code
  x$probe <- exp_type$probe[i-3]
  x <- x%>% 
    left_join(thickness_CelluloseMediumVgl_2_1) %>% 
    na.omit()
  
  #concatenate into full experimental dataframe
  zugversuche_datenFull <- rbind(zugversuche_datenFull,x)
}
rm(x) #delete intermediate variable


#reorder columns such that probe is the first column
zugversuche_datenFull2 <- zugversuche_datenFull[,c(3,1,2,4,5)]

##turn probe into factor 
# zugversuche_datenFull2 <- zugversuche_datenFull2 %>% 
#   left_join(thickness_CelluloseMediumVgl_2_1)
zugversuche_datenFull2$probe <- as.factor(zugversuche_datenFull$probe)
zugversuche_datenFull2$probe <- factor(zugversuche_datenFull$probe,
                                       levels = unique(zugversuche_datenFull$probe))

# zugversuche_datenFull2 <- zugversuche_datenFull2 %>% 
#   left_join(thickness_CelluloseMediumVgl_2_1)

zugversuche_datenFull2$Standardkraft_norm <- zugversuche_datenFull2$Standardkraft / zugversuche_datenFull2$dicke

#load earlier dataset "Zugversuche 1: with c100 medium"
zugversuche1_daten <- read_excel("/Users/ChrisHemingway/Desktop/Zugversuche_Neu2.xlsx",
                                                                sheet = "C100MediumVglWdh1", col_types = c("numeric", "numeric", "numeric", 
                                                                                                           "skip", "skip"))%>%
  mutate(groupl = "group 4")
colnames(zugversuche1_daten) = c('Dehnung', 'Standardkraft', 'probe' , 'groupl')

zugversuche2_daten <- data.frame(row.names = c("Dehnung", "Standardkraft", "probe","groupl"))
for (i in 1:length(unique(zugversuche1_daten$probe))){
  x <- subset(zugversuche1_daten, zugversuche1_daten$probe == unique(zugversuche1_daten$probe)[i]) %>% 
    filter(Dehnung <= (Dehnung[which.max(Standardkraft)] + 0.5))
  zugversuche2_daten <- rbind(zugversuche2_daten,x)
}
rm(x)


zugversuche2_daten <- zugversuche2_daten[,c(3,1,2,4)]

# zugversuche1_daten$probe2 <- zugversuche1_daten$probe
zugversuche2_daten$probe <-  as.character(zugversuche2_daten$probe)

#rename values of zugversuche1_daten$probe
#counter_variable <- c(unique(zugversuche1_daten$probe2))
#for (i in 13:21) {
 # zugversuche1_daten <- zugversuche1_daten   %>% mutate(
  #  probe = ifelse(
   #   probe2 == i,
    #  paste("1_Probe_",i),
     # zugversuche1_daten$probe)
  #)
#}
# zugversuche1_daten <- zugversuche1_daten[,1:4]
zugversuche1_daten_dicke <- read_excel("/Users/ChrisHemingway/Desktop/Zugversuche_Neu2.xlsx",
                                                             sheet = "C100MediumVglWdh1", col_types = c("skip", "skip", "skip", 
                                                                                                        "numeric", "numeric")) %>% 
  drop_na()
zugversuche1_daten_dicke$probe <- as.character(zugversuche1_daten_dicke$probe)

zugversuche2_daten <- zugversuche2_daten %>% 
  left_join(zugversuche1_daten_dicke)

zugversuche2_daten$Standardkraft_norm <- zugversuche2_daten$Standardkraft / zugversuche2_daten$dicke


zugversuche_datenFull3 <- rbind(zugversuche_datenFull2,zugversuche2_daten) %>% 
  drop_na()


# zugversuche_datenFull3 <-  zugversuche_datenFull2

# mypal2 <- colorRampPalette(brewer.pal(6, "Greens"))
# mypal3 <- colorRampPalette(brewer.pal(6, "Purples"))

plot_legend <- zugversuche_datenFull3 %>%
  ggplot( aes(x=`Dehnung`, y=Standardkraft, group = probe
              , colour = groupl
  ))+
  geom_line()+
  ylab('force')+
  xlab('strain')+
  scale_colour_manual(values = colour_brewer_set2, labels=c("N-10", "N-30","N-50","C-100"))+
  theme_minimal_grid()+
  theme()+
  # geom_label_repel(aes(label = group),
  #                  nudge_x = 1,
  #                  na.rm = TRUE) +
  # ggtitle(label = "Mediumw
  labs(colour = "")

for (i in 1:length(unique(zugversuche_datenFull3$groupl))){
  daten_temp <- zugversuche_datenFull3%>%
    filter(groupl==paste0('group ',i))
  var_p <- paste0('p_zugversuch_',i)
  plot <- daten_temp%>%
    ggplot( aes(x=`Dehnung`, y=`Standardkraft`, group = probe
                , colour = groupl
    ))+
    geom_line()+
    scale_colour_manual(values = colour_brewer_set2[i],
                      labels = c("N-10", "N-30","N-50","C-100"))+
    xlim(0,30)+
    ylim(0,3)+
    ylab('force')+
    xlab('strain')+
    theme_minimal_grid()+
    theme(
      legend.position="none"
    )+
    # geom_label_repel(aes(label = group),
    #                  nudge_x = 1,
    #                  na.rm = TRUE) +
    # ggtitle(label = "Mediumwechsel")+
    labs(colour = "")
  
  var_plot <- assign(var_p, plot)
  if (i == 1) {
    plotGrid <- list(var_plot)
  }
  else {
    plotGrid <- append(plotGrid, list(var_plot), after = length(plotGrid))
  }
  
}

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  plot_legend + theme(legend.box.margin = margin(0, 0, 0, 0))
)

labs_groups <- c('45|45|10','1/3|1/3|1/3','50|50','C-100')
# for (i in 2:length(unique(zugversuche_datenFull3$groupl))){
#   labs_groups <- append(labs_groups, paste0('Group ', i))
# }


pg2 <- plot_grid(plotlist = 
                   plotGrid
                 # ,
                 # labels = labs_groups,
                 # label_size = 12
)
pg2 <- plot_grid(pg2, legend, ncol = 2, rel_widths = c(9,2))

pg2
ggsave(path = "/Users/ChrisHemingway/Desktop", filename = "cellulose_medium_vergleich_asTiles.png", plot = pg2, device = "png" , dpi = 300 ,
       units = "cm" , width = 16.8 , height = 11)
# 
ggsave(path = "/Users/ChrisHemingway/Desktop",filename = "cellulose_medium_vergleich_ineinem.png", plot = plot_legend, device = "png" , dpi = 300 ,
       units = "cm" , width = 16.8 , height = 11)