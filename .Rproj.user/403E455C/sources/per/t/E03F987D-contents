#required packages
library(readxl)
library("tidyverse")
library(viridis)
library(RColorBrewer)
library(ggExtra)
install.packages("psych")
library(psych)

#load and clean data
daten_OD_mg <- read_excel("/Users/ChrisHemingway/Desktop/Doktorarbeit/r_programming/EMMA-Paper/daten_OD_mg_05092022.xlsx", 
                        sheet = "Tabelle1", col_types = c("text", 
                                                          "text",
                                                          "numeric"
                                                          ),
                        na = "NA")

describeBy(daten_OD_mg$weight,daten_OD_mg$group)
if(kruskal.test(daten_OD_mg$weight,daten_OD_mg$group)[3] < 0.05){
  p_value <- pairwise.wilcox.test(daten_OD_mg$weight,daten_OD_mg$group, p.adjust = "hommel")
} else {
  p_value <- "NS"
}


# sample size
sample_size = daten_OD_mg %>% group_by(group) %>% summarize(num=n())

#box plot
p1 <- daten_OD_mg %>% 
  left_join(sample_size) %>%
  mutate(myaxis = paste0(group, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y=weight, fill = group)) + 
  geom_boxplot(
    outlier.colour = NA#,
    # custom boxes
    #color="blue",
    #fill="blue",
    #alpha=0.2,
    
    # Notch?
    #notch=FALSE,
    #notchwidth = 0.8,
    
    # custom outliers
    #outlier.colour="red",
    #outlier.fill="red",
    #outlier.size=3
  ) + 
  #expand_limits(y = c(0,5:3)) +
  scale_fill_brewer(palette="PuRd") +
  geom_point(color="black", size=1, alpha=0.9) +
  ggtitle("Effects of OD") +
  theme_light()+
  theme(legend.position="none", 
        plot.title = element_text(size=20),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 11)
  ) +
  xlab("optical density of inoculum")+
  ylab(label = c("Patch weight [mg]"))

ggsave(filename = "OD_vs_weight", plot = p1, device = "tiff" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 9)
p1