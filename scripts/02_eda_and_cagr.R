# 02_eda_and_cagr.R
# This script performs Exploratory Data Analysis (EDA) and calculates
# Compound Annual Growth Rates (CAGR) for national-level data.

# --- Descriptive Plotting 1 - Annual Cumulative Capacity ---

# Plotting annual installed capacity for both sectors
sw_cumulative_plot <- ggplot(sw_cumulative, aes(x=Year,y=Annual_Cumulative_Capacity_MW,colour = Sector,group = Sector))+
  geom_line(size = 1.5)+ # Defining the line size
  geom_point(size = 3)+  # Defining the point size
  labs(
    title = "Annual Cumulative Capacity: Wind vs Solar",
    subtitle = "Year : From 2011 to 2024",
    x = "",
    y = "Annual Cumulative Capacity (MW)"
  )+
  theme_minimal()+
  scale_x_continuous(breaks = unique(sw_cumulative$Year))+ # Using 'continuous' to ensure all years are indexed in the plot
  scale_y_continuous(labels = scales::comma)+ # Using comma for capacity values for readability
  theme(
    text = element_text(family = my_font, face = "bold", colour = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold",family = my_font, size = 20,margin = margin(b=20,t=15,unit = "pt")),
    plot.subtitle = element_text(hjust = 0.5,family= my_font, size = 16,margin = margin(b=15,unit="pt"),lineheight = 1),
    legend.position = "bottom",
    legend.margin = margin(b=10,unit="pt"),
    legend.text = element_text(size = 11),
    axis.title.x = element_text(size = 14, angle = 0, hjust = 0.5, vjust = 1, lineheight = 0.9,
                                family = my_font, color = "black",face = "bold",margin = margin(t=10,unit = "pt")),
    axis.title.y = element_text(family = my_font,size = 14, face="bold",margin = margin(r=10,unit = "pt")),
    axis.text.y = element_text(family = my_font, color = "black",size = 11, face = "bold", vjust = -1,margin = margin(r=20,unit = "pt")),
    axis.text.x = element_text(family = my_font, color = "black",size = 11, face = "bold",margin = margin(r=20,unit = "pt")),
    plot.margin = margin(t=20,b=20,r=50,l=30,unit="pt")
  )

ggsave(filename = file.path(plots_dir,"sw_cumulative_plot.png"),
       plot = sw_cumulative_plot,
       device = "png",
       width = 12,
       height = 10,
       units = "in",
       dpi = 300)

# --- Descriptive Plotting 2 - Annual Installed Capacity ---

# Plotting annual installed capacity for both sectors
sw_installed_plot <- ggplot(sw_installed, aes(x=Year,y=Annual_Installed_Capacity_MW,colour = Sector,group = Sector))+
  geom_line(size = 1.5)+ # Defining the line size
  geom_point(size = 3)+  # Defining the point size
  labs(
    title = "Annual Installed Capacity: Wind vs Solar",
    subtitle = "Year : From 2011 to 2024",
    x = "",
    y = "Annual Installed Capcity (MW)"
  )+
  theme_minimal()+
  scale_x_continuous(breaks = unique(sw_installed$Year))+ # Using 'continuous' to ensure all years are indexed in the plot
  scale_y_continuous(labels = scales::comma)+ # Using comma for capacity values for readability
  theme(
    text = element_text(family = my_font, face = "bold", colour = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold",family = my_font, size = 20,margin = margin(b=20,t=15,unit = "pt")),
    plot.subtitle = element_text(hjust = 0.5,family= my_font, size = 16,margin = margin(b=15,unit="pt"),lineheight = 1),
    legend.position = "bottom",
    legend.margin = margin(b=10,unit="pt"),
    legend.text = element_text(size = 11),
    axis.title.x = element_text(size = 14, angle = 0, hjust = 0.5, vjust = 1, lineheight = 0.9,
                                family = my_font, color = "black",face = "bold",margin = margin(t=10,unit = "pt")),
    axis.title.y = element_text(family = my_font,size = 14, face="bold",margin = margin(r=10,unit = "pt")),
    axis.text.y = element_text(family = my_font, color = "black",size = 11, face = "bold", vjust = -1,margin = margin(r=20,unit = "pt")),
    axis.text.x = element_text(family = my_font, color = "black",size = 11, face = "bold",margin = margin(r=20,unit = "pt")),
    plot.margin = margin(t=20,b=20,r=50,l=30,unit="pt")
  )

ggsave(filename = file.path(plots_dir,"sw_installed_plot.png"),
       plot = sw_installed_plot,
       device = "png",
       width = 12,
       height = 10,
       units = "in",
       dpi = 300)

# --- Analysis of Annual Cumulative Capacity Using CAGR ---

# Calculating the CAGR for the entire timeline from 2011 to 2024
start_year <- 2011
end_year <- 2024
number_of_years <- end_year - start_year

sw_cagr_11_24 <- sw_cumulative %>%
  filter(Year %in% c(start_year,end_year)) %>%
  pivot_wider(
    names_from = "Year",
    values_from = "Annual_Cumulative_Capacity_MW"
  ) %>%
  rename(start_capacity = `2011`, end_capacity = `2024`) %>%
  mutate(
    CAGR_11_24 = ((end_capacity/start_capacity)^(1/number_of_years)) -1 # Defining the formula for CAGR
  )

# Calculating the CAGR for the timeline 2011 to 2016 for comparison
start_year2 <- 2011
end_year2 <- 2016
number_of_years <- end_year2 - start_year2

sw_cagr_11_16 <- sw_cumulative %>%
  filter(Year %in% c(start_year2,end_year2)) %>%
  pivot_wider(
    names_from = "Year",
    values_from = "Annual_Cumulative_Capacity_MW"
  ) %>%
  rename(start_capacity = `2011`, end_capacity = `2016`) %>%
  mutate(
    CAGR_11_16 = ((end_capacity/start_capacity)^(1/number_of_years)) -1 # Defining the formula for CAGR
  )

# Calculating the CAGR for the timeline 2016 to 2024 for comparison
start_year3 <- 2016
end_year3 <- 2024
number_of_years <- end_year3 - start_year3

sw_cagr_16_24 <- sw_cumulative %>%
  filter(Year %in% c(start_year3,end_year3)) %>%
  pivot_wider(
    names_from = "Year",
    values_from = "Annual_Cumulative_Capacity_MW"
  ) %>%
  rename(start_capacity = `2016`, end_capacity = `2024`) %>%
  mutate(
    CAGR_16_24 = ((end_capacity/start_capacity)^(1/number_of_years)) -1 # Defining the formula for CAGR
  )

# --- Counterfactual Annual Cumulative Capacity Using CAGR ---

# For the period 2011 to 2016, wind grew at a CAGR of 13.2%
# This counterfactual calculates the growth of wind sector had it followed the same rate from 2011 till 2024.

# Defining the base year for counterfactual calculation
base_year_cf <- 2011
# Defining the CAGR of the period 2011 to 2016 as the targeted CAGR
targeted_CAGR <- 0.132
# Getting the base year's capacity value from the parent dataset
wind_base_capacity <- sw_cumulative %>%
  filter(Sector == "Wind", Year == base_year_cf) %>%
  pull(Annual_Cumulative_Capacity_MW)

# Calculating the counterfactual annual cumulative capacity for years 2011 to 2024
wind_counterfactual <-  sw_cumulative %>%
  filter(Sector == "Wind") %>%
  select(Year, Annual_Cumulative_Capacity_MW) %>%
  mutate(
    years_from_base = Year - base_year_cf,
    # Calculating counterfactual by redefining the CAGR formula
    Counterfactual_Wind_MW = (targeted_CAGR + 1)^(years_from_base) * wind_base_capacity
  ) %>%
  rename(Actual_Wind_MW = Annual_Cumulative_Capacity_MW) %>%
  select(Year, Actual_Wind_MW, Counterfactual_Wind_MW)

# print(wind_counterfactual) # Uncomment to print

# Arranging the counterfactual data into a dataframe
# wc_data <- tidy(wind_counterfactual) # Tidy for data frames is deprecated, use print(wind_counterfactual)
# print(wc_data) # This will print summary statistics, not the dataframe itself
print(wind_counterfactual) # To print the actual data frame

# Getting the solar annual installed capacity from the parent dataset
solar_data <- sw_cumulative %>%
  filter(Sector == "Solar") %>%
  # Renaming the column for plotting & comparison
  rename(Actual_Solar_MW = Annual_Cumulative_Capacity_MW) %>%
  select(Year, Actual_Solar_MW)
print(solar_data)

# Creating the final dataset for plotting using 'full_join'
wind_counterfactual_plot_data <- full_join(
  wind_counterfactual, solar_data,
  by = "Year") %>%
  # Pivoting the columns for plotting purposes
  pivot_longer(
    cols = c("Actual_Solar_MW","Actual_Wind_MW","Counterfactual_Wind_MW"),
    names_to = "Type",
    values_to = "Capacity_MW"
  )
print(wind_counterfactual_plot_data)

# --- Descriptive Plotting 3 - Counterfactual Annual Cumulative Capacity ---

# Plotting counterfactual wind
wind_counterfactual_plot <- ggplot(wind_counterfactual_plot_data, aes(x=Year,y=Capacity_MW,colour = Type,group = Type))+
  geom_line(size = 1.5)+ # Defining the line size
  geom_point(size = 3)+  # Defining the point size
  labs(
    title = "Counterfactual Cumulative Capacity - Wind",
    subtitle = "Years : 2011 to 2024",
    x = "",
    y = "Cumulative Capcity (MW)"
  )+
  theme_minimal()+
  scale_x_continuous(breaks = unique(wind_counterfactual_plot_data$Year))+ # Using 'continuous' to ensure all years are indexed in the plot
  scale_y_continuous(labels = scales::comma)+ # Using comma for capacity values for readability
  theme(
    text = element_text(family = my_font, face = "bold", colour = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold",family = my_font, size = 20,margin = margin(b=20,t=15,unit = "pt")),
    plot.subtitle = element_text(hjust = 0.5,family= my_font, size = 16,margin = margin(b=15,unit="pt"),lineheight = 1),
    legend.position = "bottom",
    legend.margin = margin(b=10,unit="pt"),
    legend.text = element_text(size = 11),
    axis.title.x = element_text(size = 14, angle = 0, hjust = 0.5, vjust = 1, lineheight = 0.9,
                                family = my_font, color = "black",face = "bold",margin = margin(t=10,unit = "pt")),
    axis.title.y = element_text(family = my_font,size = 14, face="bold",margin = margin(r=10,unit = "pt")),
    axis.text.y = element_text(family = my_font, color = "black",size = 11, face = "bold", vjust = -1,margin = margin(r=20,unit = "pt")),
    axis.text.x = element_text(family = my_font, color = "black",size = 11, face = "bold",margin = margin(r=20,unit = "pt")),
    plot.margin = margin(t=20,b=20,r=50,l=30,unit="pt")
  )

ggsave(filename = file.path(plots_dir,"wind_counterfactual_plot.png"),
       plot = wind_counterfactual_plot,
       device = "png",
       width = 12,
       height = 10,
       units = "in",
       dpi = 300)
