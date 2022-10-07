library(survival)
library(GTDL)
library(ggplot2)
library(ggpubr)
library(survminer)
library(MASS)
library(tidyverse)
library(bbmle)
library(stats4)
data(SPcovid)
str(SPcovid)



SPcovid <- read.csv('SPcovid')

SPcovid$icu <- factor(SPcovid$icu,
                      labels = c("Admitted to the ICU", "Not admitted to the ICU"),
                      levels = c(0,1))


#1
ggplot(data = SPcovid, aes(x = censure)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat='count',
           position = position_dodge()) +
  facet_grid(icu ~ sex + age_group) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.2) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "", plot.title = ggtext::element_markdown(size = 30),
        plot.subtitle = ggplot2::element_text(size = 30), axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        plot.caption = ggtext::element_markdown(size = 15),) + labs(title="", x = "", y = "")



#2
ggplot(data = SPcovid, aes(x = censure)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat='count',
           position = position_dodge()) +
  facet_grid(icu ~ sex + schooling) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.2, size= 2.6) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "",  plot.title = ggtext::element_markdown(size = 30),
        plot.subtitle = ggplot2::element_text(size = 30), axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        plot.caption = ggtext::element_markdown(size = 15)) + labs(title="", x = "", y = "") +
  scale_fill_manual(values = c('#07A35D', '#2782A3'))

#3

ggplot(data = SPcovid , aes(x = censure)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat='count',
           position = position_dodge()) +
  facet_grid(icu ~ sex + ethnicities) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.2, size= 2.5) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "", plot.title = ggtext::element_markdown(size = 30),
        plot.subtitle = ggplot2::element_text(size = 30), axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        plot.caption = ggtext::element_markdown(size = 15)) + labs(title="", x = "", y = "") + 
  scale_fill_manual(values = c('#2EA6F0', '#F0C322' ))
