options(scipen = 999)

# install.packages(c("tidyverse", "ggtext", "extrafont"))

library(tidyverse)
library(ggtext)
library(extrafont)

# font_import()  #run once
loadfonts(device = "win") #run once per session

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')


hbcu_all %>%
  filter(Year >= 1990) %>% #filter the year
  select(Year, Males, Females)%>% #select columns of interest
  mutate(diff = Females - Males) %>% #calculate difference
  pivot_longer(cols = c(Males, Females)) %>% #get into long format
  rename(Gender = name, #rename columns
         Enrollments = value)-> dat_gender

head(dat_gender)


Males <- dat_gender %>%
  filter(Gender == "Males")

Females <- dat_gender %>%
  filter(Gender == "Females")

head(Females)

p <- ggplot(dat_gender)+
  
  geom_segment(data = Males,
               aes(x = Enrollments, y = Year,
                   yend = Females$Year, xend = Females$Enrollments), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  
  geom_point(aes(x = Enrollments, y = Year, color = Gender), size = 4, show.legend = TRUE)+
  
  ggtitle("Enrollment Trends at Historically Black Colleges and Universities")

p


dat_gender %>%
  group_by(Gender) %>%
  summarise(mean = mean(Enrollments),
            SE = sd(Enrollments)) %>%
  mutate(meanpos = mean + 1 *SE,
         meanneg = mean - 1 *SE)-> stats

stats_males <- stats %>%
  filter(Gender == "Males")

stats_females <- stats %>%
  filter(Gender == "Females")

head(stats)

diff <- dat_gender %>% 
  filter(Gender == "Males") %>% #you can chose Males of Females, doesn't matter
  mutate(x_pos = Enrollments + (diff/2)) #x position of label (Enrollment value of Males + diff/2)

head(diff)

p + 
  geom_text(data = diff,
            aes(label = paste("D: ",diff), x = x_pos, y = Year), #note thatI changed the greek letter Delta to "D:" because of encoding reasons
            fill = "white",
            color = "#4a4e4d",
            size = 2.5,
            family = "Segoe UI Semibold") -> p_labelled
p_labelled


p_labelled +
  
  #add facet for more control
  facet_grid(Year ~ ., scales = "free", switch = "y") +
  
  #theming
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI Semibold", color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,.5,1, "cm"))-> p_ext_facetted

p_ext_facetted


ggplot(dat_gender)+
  
  #add mean and standard deviation for both groups
  geom_rect(xmin = stats_males$meanneg, xmax = stats_males$meanpos,
            ymin = 2016, ymax = 1989, fill = "#762a83", alpha = .05)+
  geom_vline(xintercept = stats_males$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83")+
  
  geom_rect(xmin = stats_females$meanneg, xmax = stats_females$meanpos,
            ymin = 2016, ymax = 1989, fill = "#009688", alpha = .05)+  
  geom_vline(xintercept = stats_females$mean, color = "#009688", linetype = "solid",  size = .5, alpha = .8) +
  
  #add point range
  geom_segment(data = Males, aes(x = Enrollments, y = Year, yend = Females$Year, , xend = Females$Enrollments),
               color = "#aeb6bf", size = 4.5, alpha = .5) +
  
  #add points
  geom_point(aes(x = Enrollments, y = Year, color = Gender), size = 4, show.legend = FALSE) +
  
  #color points
  scale_color_manual(values = c("#009688","#762a83"))+
  
  #add point-range labels
  geom_text(data = diff, aes(label = paste("D: ",diff), x = x_pos, y = Year), fill = "white", color = "#4a4e4d", size = 2.5, family = "Segoe UI") +
  
  #add annotations for mean and standard deviations
  geom_text(x = stats_females$mean - 1500, y = 1990, label = "MEAN", angle = 90, size = 2.5, color = "#009688", family = "Segoe UI")+
  geom_text(x = stats_females$meanpos -1500, y = 1990, label = "STDEV", angle = 90, size = 2.5, color = "#009688", family = "Segoe UI")+
  
  #add facets for more control
  facet_grid(Year ~ ., scales = "free", switch = "y") +
  
  #add title
  ggtitle("Enrollment Trends at Historically Black Colleges and Universities")+
  
  #theming
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI", color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,.5,1, "cm")) -> p_styled
p_styled



p_styled +   
  
  #add subtitle and caption
  labs(subtitle = "<span style = 'color: #762a83;'>**Male**</span> and <span style = 'color: #009688;'>**Female**</span> Enrollment from 1990 to 2015<br>",
       caption = "Plot by **Tobias Stalder** | tobias-stalder.netlify.app<br>Data from **data.world** | data.world/nces | originally by NCES<br>**#TidyTuesday**")+
  
  #add theming for title, subtitle, caption
  theme(plot.caption = element_markdown(hjust = 0, lineheight = 1.5),
        plot.subtitle = element_markdown(size = 14, hjust = -.06),
        plot.title = element_text(size = 16, hjust = -.14)) #Note that hjust of title and subtitle depend on the export dimensions
