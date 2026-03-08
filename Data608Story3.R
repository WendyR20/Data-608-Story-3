library(tidyverse)
library(tigris)
library(sf)
library(usmap)

url <- "https://raw.githubusercontent.com/WendyR20/Data-608-Story-3/refs/heads/main/CRUDERATE_Underlying_Cause_of_Death_2010_2020_X93_X95.csv"
#firearm_crude <- read_csv(url)

#problems(firearm_crude)

firearm_crude <- read_csv(url, n_max = 2032) 

head(firearm_crude)
tail(firearm_crude)

#crude age rename 
names(firearm_crude)

firearm_crude <- firearm_crude %>%
  rename(
    "State_code" = "State Code",
    "Year_code" = "Year Code",
    "Age_groups" = "Ten-Year Age Groups",
    "Crude_rate" = "Crude Rate" 
  )

firearm_crude <- firearm_crude %>% 
  select(-Notes, -`Year_code`, -`Ten-Year Age Groups Code`)


head(firearm_crude)

#likert scale

law_likert <- data.frame(
  State = c(
    "California","New Jersey","Connecticut","Hawaii","Maryland","Massachusetts","New York","Illinois","Rhode Island","Delaware",
    "Washington","Oregon","Colorado","Nevada","Virginia","Minnesota","New Mexico","Michigan","Pennsylvania","Vermont",
    "Maine","Wisconsin","Florida","North Carolina", "Nebraska","Indiana","South Carolina","Georgia","Arizona","Ohio",
    "Montana","Alaska","Texas","Alabama","Mississippi","Louisiana","Arkansas","Missouri","Oklahoma","Kansas",
    "Kentucky","Tennessee","West Virginia","South Dakota","North Dakota","Idaho","Wyoming","Utah","Iowa","New Hampshire"
  ),
  law_score = c(
    5,5,5,5,5,5,5,5,5,5,
    5,5,5,4,4,4,4,4,4,4,
    3,3,3,3,3,2,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1
  )
)


#join ten-year age data

merged_crude <- firearm_crude %>%
  left_join(law_likert, by="State")

head(merged_crude)
tail(merged_crude)
anyNA(merged_crude$law_score)

which(is.na(merged_crude$law_score))

merged_crude <- merged_crude %>%
  filter(State != "District of Columbia")

anyNA(merged_crude$law_score)


head(merged_crude)
tail(merged_crude)
#exporting data


law_likert<-law_likert %>%
  mutate(state = state.abb[match(State, state.name)])

head(law_likert)



library(statebins)

statebins(law_likert,
          value_col = "law_score",
          name = "Gun Law Strength") +
  # gradientn allows for a custom list of colors
  scale_fill_gradientn(
    name = "Gun Law Strength",
    colors = c("firebrick", "coral3", "goldenrod", "darkseagreen", "dodgerblue"),
    labels = c("Weakest Gun Laws\n(Minimal Restrictions)", 
               "Low", 
               "Moderate", 
               "High", 
               "Strongest Gun Laws\n(Strict Regulations)"),
    guide = guide_colorbar(
      barwidth = 15, 
      barheight = 0.5, 
      title.position = "top", 
      title.hjust = 0.5
    )
  ) +
  theme(
    # Remove background
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    
    # Remove axes
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    
    legend.position = "bottom",
    legend.text = element_text(size = 8) # Keeps the long text readable
  ) +
  labs(
    title = "Gun Law Strength by State",
    subtitle = "Mapping regulatory intensity across the US"
  )




merged_crude2 <- merged_crude %>%
  mutate(state = state.abb[match(State, state.name)])

head(merged_crude2)


# Create a 'Law Category' column
merged_crude2 <- merged_crude2 %>%
  mutate(law_cat = case_when(
    law_score >= 4 ~ "Strong Laws (4-5)",
    law_score == 3 ~ "Moderate Laws (3)",
    law_score <= 2 ~ "Weak Laws (1-2)"
  )) %>%
  mutate(law_cat = factor(law_cat, levels = c("Weak Laws (1-2)", 
                                              "Moderate Laws (3)", "Strong Laws (4-5)"))) %>%
  group_by(Year, law_cat, Age_groups) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop")

#plot
ggplot(merged_crude2, aes(x = Year, y = Deaths, fill = Age_groups)) +
  geom_area(alpha = 0.8, color = "white", linewidth = 0.1) + # Stacked area
  facet_wrap(~law_cat, ncol = 1, scales = "free_y") + 
  scale_fill_manual(values = c("lightpink2", "red", "purple", "khaki3", "tan", "darkseagreen3", "grey","steelblue","burlywood4" )) +
  scale_x_continuous(breaks = seq(min(merged_crude2$Year), max(merged_crude2$Year), by = 2))+
  theme_minimal() +
  labs(
    title = "Those Between 15 and 34 Years Old Bear the Brunt of Firearm Assault Deaths",
    subtitle = "Total Firearm Assualt Death Totals Across States by Gun Law Strength \n From 2010-2020",
    y = "Total Number of Deaths",
    x = "Year",
    fill = "Age Group"
  )


#avg deaths by age
# Create a 'Law Category' column
merged_crude3 <- merged_crude %>%
  mutate(law_cat = case_when(
    law_score >= 4 ~ "Strong Laws (4-5)",
    law_score == 3 ~ "Moderate Laws (3)",
    law_score <= 2 ~ "Weak Laws (1-2)"
  )) %>%
  mutate(law_cat = factor(law_cat, levels = c("Weak Laws (1-2)", 
                                              "Moderate Laws (3)", "Strong Laws (4-5)"))) %>%
  group_by(Year, law_cat, Age_groups) %>%
  summarise(Avg_Deaths = mean(Deaths, na.rm = TRUE), .groups = "drop")


#plot
ggplot(merged_crude3, aes(x = Year, y = Avg_Deaths, fill = Age_groups)) +
  geom_area(alpha = 0.8, color = "white", linewidth = 0.1) + # Stacked area
  facet_wrap(~law_cat, ncol = 1, scales = "free_y") + 
  scale_fill_manual(values = c("lightpink2", "blue", "yellow", "khaki3", "tan", "darkseagreen3", "grey","steelblue","burlywood4" )) +
  scale_x_continuous(breaks = seq(min(merged_crude3$Year), max(merged_crude3$Year), by = 2))+
  theme_minimal() +
  labs(
    title = "Average Number of Firearm Assault Deaths \n For 15-34 Year Olds Are Trending Upwards",
    subtitle = "By State Gun Law Strength From 2010-2020",
    y = "Total Number of Deaths",
    x = "Year",
    fill = "Age Group"
  )

#Exploring the crude rate


merged_crude_clean <- merged_crude %>%
  mutate(Crude_Rate_Clean = as.numeric(as.character(Crude_rate)))


names(merged_crude_clean)
# Calculate the Average RATE instead of Deaths
crude_rate <- merged_crude_clean %>%
  mutate(law_cat = case_when(
    law_score >= 4 ~ "Strong Laws (4-5)",
    law_score == 3 ~ "Moderate Laws (3)",
    law_score <= 2 ~ "Weak Laws (1-2)"
  )) %>%
  group_by(Year, law_cat) %>%
  summarise(Mean_Rate = mean(Crude_Rate_Clean, na.rm = TRUE), .groups = "drop")

# Plot a simple line chart to see the truth
ggplot(crude_rate, aes(x = Year, y = Mean_Rate, color = law_cat)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("lightsteelblue", "lightgray", "red")) +
  scale_x_continuous(breaks = seq(min(crude_rate$Year), max(crude_rate$Year), by = 2))+
  theme_minimal() +
  labs(title = "Stark Per-Capita Gap: Raw Death Totals Mask the Reality of Gun Law Effectiveness",
       subtitle = "Mortality rates from 2010-2020 when controlled for population size",
       y = "Average Death Rate (Deaths per 100k )",
       color = "Gun Law Category")

#highlighting surges

ggplot(crude_rate, aes(x = Year, y = Mean_Rate, color = law_cat)) +
  annotate("rect", xmin = 2015.5, xmax = 2016.5, ymin = -Inf, ymax = Inf, 
           fill = "gray90", alpha = 0.5) +
  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = -Inf, ymax = Inf, 
           fill = "gray85", alpha = 0.5) +
  
  geom_line(size = 1.2) +
  geom_point() +
  
  geom_vline(xintercept = 2016, linetype = "dotted", color = "gray40") +
  annotate("text", x = 2016, y = max(crude_rate$Mean_Rate) * 0.85, 
           label = "2016 Spike", hjust = 1.1, color = "gray30") +
  
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2020, y = max(crude_rate$Mean_Rate, na.rm = TRUE), 
           label = "2020 Covid Surge", hjust = 1.1) +
  
  scale_color_manual(values = c("lightsteelblue", "lightgray", "red")) +
  scale_x_continuous(breaks = seq(min(crude_rate$Year), max(crude_rate$Year), by = 2)) +
  theme_minimal() +
  labs(
    title = "Accelerating Risk: 2016 and 2020's Firearm Mortality Escalation",
    subtitle = "Mortality rates spiked in 2016 and 2020 \n Which Both Marked Periods of Upheaval",
    y = "Average Death Rate (Deaths per 100k)",
    color = "Gun Law Category"
  )

class(crude_rate$Year)
