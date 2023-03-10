#required packages
library(readxl)
library("tidyverse")
library(viridis)
library(RColorBrewer)

#load and clean data
DatenEmma <- read_excel("/Users/ChrisHemingway/Desktop/Doktorarbeit/r_programming/EMMA-Paper/DatenEmma.xlsx", 
                            sheet = "Tabelle1", col_types = c("text", 
                                                              "text",
                                                              "text",
                                                              "text",
                                                              "text", 
                                                              "text",
                                                              "text",
                                                              "numeric",
                                                              "numeric", 
                                                              "numeric",
                                                              "numeric",
                                                              "numeric", 
                                                              "numeric"),
                            na = "NA")
data_preprep <- DatenEmma %>% 
  filter( 
    test_series_nr == "2"|
    test_series_nr == "3" &
      preprep != "Agar") %>%
  select(patch, test_series_nr, thickness, preprep) %>%
  na.omit()

#mutate column preprep -> factor with levels "", "12"
data_preprep$preprep <- as.factor(data_preprep$preprep)
data_preprep$preprep <- factor(
  data_preprep$preprep, levels = c("without prepreparation",
                                   "with prepreparation"))

# sample size
sample_size = data_preprep %>% group_by(preprep) %>% summarize(num=n())

#box plot
p1 <- data_preprep %>% 
  left_join(sample_size) %>%
  mutate(myaxis = paste0(preprep, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y=thickness, fill = preprep)) + 
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
  expand_limits(y = c(0:3)) +
  scale_fill_brewer(palette="PuRd") +
  geom_jitter(color="black", size=1, alpha=0.9, width = 0.05) +
  ggtitle("Effects of Pre-preparation on Patch Yield") +
  ylab(label = c("Thickness")) +
  theme_light()+
  theme(legend.position="none", 
        plot.title = element_text(size=20),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 11)
        ) +
  xlab("Pre-Preparation?")+
  scale_x_discrete(labels=c("without prepreparation" = "Without Pre-Prep",
                            "with prepreparation" ="With Pre-Prep"))
p1
ggsave(filename = "Fig1D-PREP", plot = p1, device = "tiff" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 9)

