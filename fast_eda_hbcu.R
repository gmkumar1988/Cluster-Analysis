options(scipen = 999)

install.packages("DataExplorer")
library(DataExplorer)

install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)

hbcu_eda <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

hbcu_eda

hbcu_eda %>% glimpse()

hbcu_eda %>% 
  create_report(
    output_file = "hbcu fast eda analysis",
    output_dir = "F://R Programming//R Working Files/",
    report_title = "EDA Report - HBCU EDA Analysis"
    
  )


hbcu_eda %>% introduce()

hbcu_eda %>% plot_intro()

hbcu_eda %>% plot_missing()

###to identify the missing values:
hbcu_eda %>% profile_missing()

hbcu_eda %>% plot_density()

hbcu_eda %>% plot_histogram()

hbcu_eda %>% plot_bar()

hbcu_eda %>% plot_correlation(maxcat = 15)

