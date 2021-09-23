# Homework_Final
Here, I used R to create better plots for an article - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8114258/pdf/10.1177_2333794X211012980.pdf

---
title: "Homework"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Load important libraries
```{r}
library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)
```

Import Dataset into project.
```{r}
(egypt <- read_csv(here("data", "Egypt.csv")))
```

Data Transformation.
1. Transforming original data to get only columns needed for plots.
I will exclude all the columns I do not need.
```{r}
vars_out <- c("Diet", "School", "Family Members", "Tics?", "Z score JUL", "Percentile JUL", "Z score MAR", "Percentile_MAR", "Name", "Group", "Weight_Change", "...35", "...36", "Sleep Questionnaire", "Sport")
egypt <- data_frame(select(egypt, -vars_out))

egypt
```

2. Clean up

```{r}
New_Egypt <- egypt %>% 
  drop_na() %>% 
  mutate(
    Routine = case_when(
      Routine == "no" ~ "None",
      Routine == "No" ~ "None",
      Routine == "intense" ~ "Intense",
      Routine == "medium" ~ "Medium",
      Routine == "moderate" ~ "Medium",
      Routine == "mild" ~ "Mild"
    ) %>% 
      as.factor %>% 
      fct_relevel("Intense", "Medium",
                  "Mild", "None")
  )

New_Egypt
```


Data Processing & Visualisation 
1. Simple Data Visualisation - Bar chart of proportionality
```{r}
ggplot(data = New_Egypt) + 
  stat_count(mapping = aes(x = Routine), width = 0.5, fill="tomato2") + 
    labs(title="Bar Chart", 
      subtitle="Exercise Routine for Participants")
```
To find out if Exercise Routine helped with weight gain, I compare change in BMI change with routine.
First, I create BMI_DIFF to calculate change in BMI from March to July, and Quality of Life (Qol) change.
```{r}
New_Egypt <- mutate(New_Egypt, 
                   BMI_DIFF = BMI_JUL - BMI_MAR,
                   Qol_Change = QoLMAR - QoLJUL
                   )
New_Egypt


```

Summary - General BMI increase, no positive correlation in routine and weight gain or loss.
Although proportionally - about 40% (2 of 5) of people with intense routine had BMI loss, while over 80% of athletes that did not undergo any routine had an increase BMI> 


```{r}
library(ggpubr)

compare_means(BMI_DIFF ~ Routine,  data = New_Egypt)
my_comp <- list( c("Mild", "None"), c("Mild", "Medium"), c("Intense", "Medium"), c("Intense", "Mild"), c("Intense", "None") )

ggboxplot(New_Egypt, x = "Routine", y = "BMI_DIFF",
          fill = "plum", palette = "jco")+ 
  
  labs(title="Box plot", 
    subtitle="Body Mass Index Change based on Routine",
    caption="Source: Egypt2",
    x="Routine",
    y="Body Mass Index Change") +
  
  stat_compare_means(comparisons = my_comp) # Add pairwise comparisons p-value

```

Here, I check sleep hours of players based on routine.

```{r}
Egypt2 <- New_Egypt %>% 
  group_by(Routine) %>% 
  summarise(
    mean_sleep = mean(Daily_Sleep_Hours),
    sd_sleep   = sd(Daily_Sleep_Hours), 
    mean_ScreenTime = mean(ScreenTime),
    sd_ScreenTime   = sd(ScreenTime), 
    
  ) 

Egypt2 %>% 
  ggplot(aes(Routine, mean_sleep)) +
    geom_col(aes(fill = Routine), color = "black", width = 0.85) +
    geom_errorbar(aes(ymin = mean_sleep - sd_sleep,
                      ymax = mean_sleep + sd_sleep),
                  color = "#22292F",
                  width = .1)

```


```{r}
library(ggcorrplot)
library(corrplot)

Soccer <- New_Egypt[,c(9:10, 12:13, 22)]
Soccer2 <- cor(Soccer, method = "pearson")
Soccer2

data(Soccer2)
corr <- round(cor(Soccer2), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Behavioral Changes and Weight Gain", 
           ggtheme=theme_bw)
```
compare BMI_DIFF and Depression and Anxiety Score - Any Correlation? 
```{r}
P_Check <- select(New_Egypt, 9,11, 19:22)

ggplot(data = P_Check) +
  geom_smooth(
    mapping = aes(x = BMI_DIFF , y = DASS_D),
    show.legend = FALSE
  )
```

```{r}
ggplot(data = P_Check) +
  geom_smooth(
    mapping = aes(x = BMI_DIFF, y = DASS_A),
    show.legend = FALSE
  )
```



```{r}
egypt2 <- New_Egypt %>% 
  group_by(Routine) %>% 
  summarise(
    mean_sleep = mean(Daily_Sleep_Hours),
    sd_sleep   = sd(Daily_Sleep_Hours),
    mean_screentime = mean(ScreenTime),
    sd_screentime   = sd(ScreenTime)
  )
    
egypt2
```

```{r}
Egypt3 <- select(New_Egypt, 9:13)
theme_set(theme_classic())

# Histogram on a Continuous (Numeric) Variable
g <- ggplot(Egypt3, aes(Daily_Sleep_Hours)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=Routine), 
                   binwidth = .3, 
                   col="black", 
                   size=.5) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Daily Sleep across Routine")  
```

Check Qol Change wiht WeightGain and BMI Change

```{r}
P_Check2 <- select(New_Egypt, 9,22:23)

ggplot(data = P_Check2) +
  geom_smooth(
    mapping = aes(x = BMI_DIFF , y = Qol_Change),
    show.legend = FALSE
  ) +
    labs(title="Correlation Map", 
       subtitle="Influence of Quality of Life Change on BMI") 
```
Higher Qol Change correlates with Increase in BMI.
