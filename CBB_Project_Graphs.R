# Libraries

library(tidyverse)
library(mosaic)
library(ggthemes)
library(extrafont)

# Imports

levels = c('SEC','Pac-12','Big Ten','Big East','Big 12','ACC')

cbb <- read_csv("C:/Users/Max/Desktop/CBB Project/CBB_Conference.csv") %>%
  mutate(Percent_Change = I((Wins - Expected_Wins)/Expected_Wins),
         Conference = factor(Conference, levels = levels))

# Unused Dataset

# cbb_cross <- read_csv("C:/Users/Max/Desktop/CBB Project/CBB_Cross.csv") %>%
#   group_by(Conference, Opponent_Conference) %>%
#   mutate(Percent_Change = I((Wins - Expected_Wins)/Expected_Wins),
#          label = paste(round(Percent_Change,4)*100,'%',' (',Games,')',sep = '')
#          )

cbb_all_season <- read_csv("C:/Users/Max/Desktop/CBB Project/CBB_Conference_All_Season.csv") %>%
  mutate(Percent_Change = I((Wins - Expected_Wins)/Expected_Wins),
         Conference = factor(Conference, levels = levels),
         label = paste(round(Percent_Change,4)*100,'%',sep = ''))

cbb_cross_all_season <- read_csv("C:/Users/Max/Desktop/CBB Project/CBB_Cross_All_Season.csv") %>%
  group_by(Conference, Opponent_Conference) %>%
  mutate(Percent_Change = I((Wins - Expected_Wins)/Expected_Wins),
         Conference = factor(Conference, levels = levels),
         label = paste(round(Percent_Change,4)*100,'%',' (',Games,')',sep = '')
  )  


# Percent Change Annual Bar Graph

ggplot(cbb, aes(x = Percent_Change,
                y = as.factor(Conference),
                fill = factor(sign(Percent_Change)))) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values = c('red','green'))+
  facet_grid(cols = vars(Season)) +
  guides(fill = "none") +
  theme_minimal(base_family = 'Calibri') +
  labs(title = "Actual vs. Expected Nonconference Win Percentage By Conference (% Difference)",
       subtitle = "2021 to 2023 CBB Season",
       caption = "Expected Win% Based on Conference Win% (Data from Sports-Reference.com)") +
  scale_x_continuous(name = "% Difference in Actual and Expected Win%", # Axis title
                     breaks = c(-.3,-.2,-.1,0,.1,.2,.3), # Tic marks
                     labels = c('-30%','-20%','-10%',0,'10%','20%','30%'),
                     limits = c(-.4,.4)) +
  scale_y_discrete(name = "Conference") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'lightgrey'),
        panel.background = element_rect(fill= 'lightgrey'),
        panel.grid.major.y = element_blank()) +
  geom_text(data = subset(cbb, Season == '2021'),
            aes(x = 0.0620, y = 'Pac-12', label = "12.42%"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2022'),
            aes(x = 0.1711, y = 'Big 12', label = "34.23%"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2023'),
            aes(x = .1195, y = 'Big 12', label = "23.89%"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2021'),
            aes(x = -.1089, y = 'SEC', label = "-21.78%"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2022'),
            aes(x = -.1473, y = 'Pac-12', label = "-29.43%"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2023'),
            aes(x = -.0733, y = 'Pac-12', label = "-14.67%"),
            color = "black",
            size = 3,
            family = 'Calibri')


# All Season Bar Chart

ggplot(cbb_all_season, aes(x = Percent_Change,
                   y = reorder(as.factor(Conference), Percent_Change), 
                   fill = factor(sign(Percent_Change)))) +
  geom_col(position = 'dodge') +
  geom_text(aes(label= label), size = 3.5, hjust = ifelse(cbb_all_season$Percent_Change > 0,'left','right')) +
  scale_fill_manual(values = c('red','green'))+
  guides(fill = "none") +
  theme_minimal(base_family = 'Calibri') +
  labs(title = "Actual vs. Expected Nonconference Win Percentage By Conference (% Difference)",
       subtitle = "2021 to 2023 CBB Season",
       caption = "Expected Win% Based on Conference Win% (Data from Sports-Reference.com)") +
  scale_x_continuous(name = "% Difference in Actual and Expected Win%", # Axis title
                     breaks = c(-.3,-.2,-.1,0,.1,.2,.3), # Tic marks
                     labels = c('-30%','-20%','-10%',0,'10%','20%','30%'),
                     limits = c(-.3,.3)) +
  scale_y_discrete(name = "Conference") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'lightgrey'),
        panel.background = element_rect(fill= 'lightgrey'),
        panel.grid.major.y = element_blank())


# Cross Heatmap

cbb_cross_all_season %>%
  ggplot(aes(x = Opponent_Conference, 
             y = Conference, 
             fill = as.numeric(Percent_Change))) + 
  geom_tile() +
  geom_text(aes(label= label), size = 3.5) +
  scale_fill_gradient2(midpoint = 0,
                       low = "darkred",
                       high = "darkgreen") +
  theme_minimal(base_family = 'Calibri') +
  guides(fill = "none") +
  labs(title = "Actual vs. Expected Conference vs Conference Win Percentage (% Difference)",
       subtitle = "2021 to 2023 CBB Season (# Games in Parenthesis)",
       x = 'Opponent Conference',
       y = 'Conference',
       caption = "Expected Win% Based on Conference Win% (Data from Sports-Reference.com)") +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = 'lightgrey'),
        panel.background = element_rect(fill = 'white'))

# Point Differential Barchart
  
ggplot(cbb, aes(x = Point_Differential_Alpha,
                y = as.factor(Conference),
                fill = factor(sign(Point_Differential_Alpha)))) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values = c('red','green'))+
  facet_grid(cols = vars(Season)) +
  guides(fill = "none") +
  theme_minimal(base_family = 'Calibri') +
  labs(title = "Actual - Expected Nonconference Point Differential By Conference",
       subtitle = "2021 to 2023 CBB Season",
       caption = "Expected Point Differential Based on Conference Point Differential (Data from Sports-Reference.com)") +
  scale_x_continuous(name = "Actual - Expected Point Differential", # Axis title
                     breaks = c(-6,-4,-2,0,2,4,6), # Tic marks
                     limits = c(-8,8)) +
  scale_y_discrete(name = "Conference") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'lightgrey'),
        panel.background = element_rect(fill= 'lightgrey'),
        panel.grid.major.y = element_blank()) +
  geom_text(data = subset(cbb, Season == '2021'),
            aes(x = 2.21, y = 'Pac-12', label = "4.42"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2022'),
            aes(x = 3.28, y = 'Big 12', label = "6.57"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2023'),
            aes(x = 1.87, y = 'Big 12', label = "3.74"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2021'),
            aes(x = -1.09, y = 'ACC', label = "-2.17"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2022'),
            aes(x = -1.90, y = 'Pac-12', label = "-3.81"),
            color = "black",
            size = 3,
            family = 'Calibri') +
  geom_text(data = subset(cbb, Season == '2023'),
            aes(x = -2.6, y = 'ACC', label = "-5.21"),
            color = "black",
            size = 3,
            family = 'Calibri')


# Totals Bar Chart (Point Differential)

ggplot(cbb_all_season, aes(x = Point_Differential_Alpha,
                   y = reorder(as.factor(Conference), Point_Differential_Alpha), 
                   fill = factor(sign(Point_Differential_Alpha)))) +
  geom_col(position = 'dodge') +
  geom_text(aes(label= round(Point_Differential_Alpha,2)), size = 3.5, hjust = ifelse(cbb_all_season$Point_Differential_Alpha > 0,'left','right')) +
  scale_fill_manual(values = c('red','green'))+
  guides(fill = "none") +
  theme_minimal(base_family = 'Calibri') +
  labs(title = "Actual - Expected Nonconference Point Differential By Conference",
       subtitle = "2021 to 2023 CBB Season",
       caption = "Expected Point Differential Based on Conference Point Differential (Data from Sports-Reference.com)") +
  scale_x_continuous(name = "Actual - Expected Point Differential", # Axis title
                     breaks = c(-6,-4,-2,0,2,4,6), # Tic marks
                     limits = c(-8,8)) +
  scale_y_discrete(name = "Conference") +
  theme(panel.grid.minor = element_blank()) +
  theme(plot.background = element_rect(fill = 'lightgrey'),
        panel.background = element_rect(fill= 'lightgrey'))

# Cross Heatmap

cbb_cross_all_season %>%
  ggplot(aes(x = Opponent_Conference, 
             y = Conference, 
             fill = as.numeric(Point_Differential_Alpha))) + 
  geom_tile() +
  geom_text(aes(label= paste(round(Point_Differential_Alpha, 2),' (',Games, ')',sep = ''), size = 3.5)) +
  scale_fill_gradient2(midpoint = 0,
                       low = "darkred",
                       high = "darkgreen") +
  theme_minimal(base_family = 'Calibri') +
  guides(fill = "none",
         size = 'none') +
  labs(title = "Actual - Expected Conference vs Conference Point Differential",
       subtitle = "2021 to 2023 CBB Season (# Games in Parenthesis)",
       x = 'Opponent Conference',
       y = 'Conference',
       caption = "Expected Point Differential Based on Conference Point Differential (Data from Sports-Reference.com)") +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = 'lightgrey'),
        panel.background = element_rect(fill = 'white'))

