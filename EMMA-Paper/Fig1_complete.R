#required packages
library(readxl)
library("tidyverse")
library(viridis)
library(RColorBrewer)
library(ggExtra)
library(cowplot)

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

data_incubator <- DatenEmma %>% 
  select(patch, test_series_nr , incubator, thickness) %>% 
  filter(test_series_nr == "1") %>% 
  mutate(incubator = ifelse(incubator == "FALSE", "no", "yes"))

data_mediumcond <- DatenEmma %>% 
  filter(test_series_nr == "1") %>% 
  select(patch, test_series_nr , refill_exchange, thickness)

#mutate column refill_exchange -> factor with levels "rrr", "rer", "eee"
data_mediumcond$refill_exchange <- as.factor(data_mediumcond$refill_exchange)
data_mediumcond$refill_exchange <- factor(
  data_mediumcond$refill_exchange, levels = c("rrr","rer","eee"))

data_inctime <- DatenEmma%>% 
  filter( 
      test_series_nr == "3"| 
      test_series_nr == "4") %>% 
  select(patch, test_series_nr , incubation_time, thickness)

#mutate column incubation_time -> factor with levels "6", "12"
data_inctime$incubation_time <- as.factor(data_inctime$incubation_time)
data_inctime$incubation_time <- factor(
  data_inctime$incubation_time, levels = c("6","12"))

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
sample_size_1a = data_incubator %>% group_by(incubator) %>% summarize(num=n())
sample_size_1b = data_mediumcond %>% group_by(refill_exchange) %>% summarize(num=n())
sample_size_1c = data_inctime %>% group_by(incubation_time) %>% summarize(num=n())
sample_size_1d = data_preprep %>% group_by(preprep) %>% summarize(num=n())


#draw box plots

p1a <- data_incubator %>% 
  left_join(sample_size_1a) %>%
  mutate(myaxis1a = paste0(incubator, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis1a, y=thickness, fill = incubator)) + 
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
  geom_point(color="black", size=1, alpha=0.9) +
  ggtitle("Effects of an Incubator on\n Patch Yield") +
  theme_light(
  )+
  theme(legend.position="none", 
        plot.title = element_text(size=12),
        axis.title = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8)
  ) +
  xlab("Incubator?")+
  ylab(label = c("Thickness"))

p1b <- data_mediumcond %>% 
  left_join(sample_size_1b) %>%
  mutate(myaxis1b = paste0(refill_exchange, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis1b, y=thickness, fill = refill_exchange)) + 
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
  geom_point(color="black", size=1, alpha=0.9) +
  ggtitle("Effects of Medium \n Refill vs Exchange") +
  theme_light()+
  theme(legend.position="none", 
        plot.title = element_text(size=12),
        axis.title = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8)
  ) +
  xlab("Modality of medium refill vs exchange")+
  ylab(label = c("Thickness"))

p1c <- data_inctime %>% 
  left_join(sample_size_1c) %>%
  mutate(myaxis1c = paste0(incubation_time, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis1c, y=thickness, fill = incubation_time)) + 
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
  ggtitle("Effects of Incubation Time \n on Patch Yield") +
  theme_light()+
  theme(legend.position="none", 
        plot.title = element_text(size=12),
        axis.title = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8)
  ) +
  xlab("Incubation time (days)")+
  ylab(label = c("Thickness"))

p1d <- data_preprep %>% 
  left_join(sample_size_1d) %>%
  mutate(myaxis1d = paste0(preprep, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis1d, y=thickness, fill = preprep)) + 
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
  ggtitle("Effects of Pre-Preparation \n on Patch Yield") +
  ylab(label = c("Thickness")) +
  theme_light()+
  theme(legend.position="none", 
        plot.title = element_text(size=11),
        axis.title = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8)
  ) +
  xlab("Pre-Preparation?")+
  scale_x_discrete(labels=c("Yes",
                            "No"))

#arrange plots into grid:

plot_grid <- plot_grid(p1a, p1b, p1c, p1d, 
                       labels = c('A', 'B','C','D'), label_size = 14)

ggsave(filename = "figure1_complete", plot = plot_grid , device = "tiff" , dpi = 1000 ,
       units = "cm" , width = 16.8 , height = 16.8)





