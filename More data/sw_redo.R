library(tidyverse)
library(readxl)
library(fixest)
library(sandwich)
library(lmtest)
library(extrafont)
library(broom)
library(writexl)
library(ggplot2)
library(dplyr)
library(tseries)
library(car)
my_font <- "Times New Roman"


#------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Initial data loading and cleaning - National Level Data
#------------------------------------------------------------------------------------------------------------------------------------------------

#Loading and cleaning 'Annual Cumulative Capacity' dataset
sw_cumulative_raw <- read.csv("Annual_Cumulative_Capacity.csv") 

sw_cumulative <- sw_cumulative_raw %>%
  #Replacing NAs with 0 and removing whitespaces
  mutate(across(where(is.numeric), ~replace_na(.,0)),
         across(where(is.character), trimws))

#Changing the datatype of Sector to factor for plotting convenience
sw_cumulative$Sector <- as.factor(sw_cumulative$Sector)
sw_cumulative$Year <- as.numeric(sw_cumulative$Year)

#Loading and cleaning 'Annual Installed Capacity' dataset

sw_installed_raw <- read.csv("Annual_Installed_Capacity.csv")

sw_installed <- sw_installed_raw %>%
  #Replacing NAs with 0 and removing whitespaces
  mutate(across(where(is.numeric), ~replace_na(.,0)),
         across(where(is.character), trimws))

#Changing the datatype of Sector to factor for plotting convenience
sw_installed$Sector <- as.factor(sw_installed$Sector)
sw_installed$Year <- as.numeric(sw_installed$Year)


#------------------------------------------------------------------------------------------------------------------------------------------------
# 2.1 Descriptive Plotting - Annual Cumulative Capacity
#------------------------------------------------------------------------------------------------------------------------------------------------


#Plotting annual installed capacity for both sectors
sw_cumulative_plot <- ggplot(sw_cumulative, aes(x=Year,y=Annual_Cumulative_Capacity_MW,colour = Sector,group = Sector))+
  #Defining the line size
  geom_line(size = 1.5)+
  #Defining the point size
  geom_point(size = 3)+
  labs(
    title = "Annual Cumulative Capacity: Wind vs Solar",
    subtitle = "Year : From 2011 to 2024",
    x = "",
    y = "Annual Cumulative Capacity (MW)"
  )+
  theme_minimal()+  #Using 'continuous' to ensure all years are indexed in the plot
  scale_x_continuous(breaks = unique(sw_cumulative$Year))+
  #Using comma for capacity values for readability
  scale_y_continuous(labels = scales::comma)+
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

print(sw_cumulative_plot)




#------------------------------------------------------------------------------------------------------------------------------------------------
# 2.2 Descriptive Plotting - Annual Installed Capacity
#------------------------------------------------------------------------------------------------------------------------------------------------

dev.off()
graphics.off()

#Plotting annual installed capacity for both sectors
sw_installed_plot <- ggplot(sw_installed, aes(x=Year,y=Annual_Installed_Capacity_MW,colour = Sector,group = Sector))+
  #Defining the line size
  geom_line(size = 1.5)+
  #Defining the point size
  geom_point(size = 3)+
  labs(
    title = "Annual Installed Capacity: Wind vs Solar",
    subtitle = "Year : From 2011 to 2024",
    x = "",
    y = "Annual Installed Capcity (MW)"
  )+
  theme_minimal()+
  #Using 'continuous' to ensure all years are indexed in the plot
  scale_x_continuous(breaks = unique(sw_installed$Year))+
  #Using comma for capacity values for readability
  scale_y_continuous(labels = scales::comma)+
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

print(sw_installed_plot)


#------------------------------------------------------------------------------------------------------------------------------------------------
# 3. Annual Cumulative Capacity - An Analysis using CAGR
#------------------------------------------------------------------------------------------------------------------------------------------------

#Calculating the CAGR for the entire timeline from 2011 to 2024
start_year <- 2011
end_year <- 2024
number_of_years <- end_year - start_year

sw_cagr_11_24 <- sw_cumulative %>%
  filter(Year %in% c(start_year,end_year)) %>%
  pivot_wider(
    #Pivoting the columns wider for calculation
    names_from = "Year",
    values_from = "Annual_Cumulative_Capacity_MW"
  ) %>%
  #Renaming the columns for easier comparison
  rename(start_capacity = `2011`, end_capacity = `2024`) %>%
  mutate(
    #Defining the formula for CAGR
    CAGR_11_24 = ((end_capacity/start_capacity)^(1/number_of_years)) -1
  ) 

#Calculating the CAGR for the timeline 2011 to 2016 for comparison
start_year2 <- 2011
end_year2 <- 2016
number_of_years <- end_year2 - start_year2

sw_cagr_11_16 <- sw_cumulative %>%
  filter(Year %in% c(start_year2,end_year2)) %>%
  pivot_wider(
    #Pivoting the columns wider for calculation
    names_from = "Year",
    values_from = "Annual_Cumulative_Capacity_MW"
  ) %>%
  #Renaming the columns for easier comparison
  rename(start_capacity = `2011`, end_capacity = `2016`) %>%
  mutate(
    #Defining the formula for CAGR
    CAGR_11_16 = ((end_capacity/start_capacity)^(1/number_of_years)) -1
  ) 

#Calculating the CAGR for the timeline 2016 to 2024 for comparison
start_year3 <- 2016
end_year3 <- 2024
number_of_years <- end_year3 - start_year3

sw_cagr_16_24 <- sw_cumulative %>%
  filter(Year %in% c(start_year3,end_year3)) %>%
  pivot_wider(
    #Pivoting the columns wider for calculation
    names_from = "Year",
    values_from = "Annual_Cumulative_Capacity_MW"
  ) %>%
  #Renaming the columns for easier comparison
  rename(start_capacity = `2016`, end_capacity = `2024`) %>%
  mutate(
    #Defining the formula for CAGR
    CAGR_16_24 = ((end_capacity/start_capacity)^(1/number_of_years)) -1
  ) 


#------------------------------------------------------------------------------------------------------------------------------------------------
# 4.1 Annual Cumulative Capacity - Counterfactual calculation using CAGR
#------------------------------------------------------------------------------------------------------------------------------------------------

#For the period 2011 to 2016, wind grew at a CAGR of 13.2%
#This counterfactual calculates the growth of wind sector had it followed the same rate from 2011 till 2024.

#Defining the base year for counterfactual calculation
base_year_cf <- 2011
#Defining the CAGR of the period 2011 to 2016 as the targeted CAGR
targeted_CAGR <- 0.132
#Getting the base year's capacity value from the parent dataset
wind_base_capacity <- sw_cumulative %>%
  filter(Sector == "Wind", Year == base_year_cf) %>%
  pull(Annual_Cumulative_Capacity_MW)

#Calculating the counterfactual annual cumulative capacity for years 2011 to 2024
wind_counterfactual <-  sw_cumulative %>%
  filter(Sector == "Wind") %>%
  select(Year, Annual_Cumulative_Capacity_MW) %>%
  mutate(
    years_from_base = Year - base_year_cf,
    #Calculating counterfactual by redefining the CAGR formula
    Counterfactual_Wind_MW = (targeted_CAGR + 1)^(years_from_base) * wind_base_capacity
  ) %>%
  rename(Actual_Wind_MW = Annual_Cumulative_Capacity_MW) %>%
  select(Year, Actual_Wind_MW,Counterfactual_Wind_MW)

#print(wind_counterfactual) #Uncomment to print

#Arranging the counterfactual data into a dataframe
wc_data <- tidy(wind_counterfactual)
print(wc_data)

#Getting the solar annual installed capacity from the parent dataset
solar_data <- sw_cumulative %>%
  filter(Sector == "Solar") %>%
  #Renaming the column for plotting & comparison
  rename(Actual_Solar_MW = Annual_Cumulative_Capacity_MW)%>%
  select(Year,Actual_Solar_MW)
print(solar_data)

#Creating the final dataset for plotting using 'full_join'
wind_counterfactual_plot_data <- full_join(
  wind_counterfactual, solar_data,
  by = "Year")%>%
  #Pivoting the columns for plotting purposes
  pivot_longer(
    cols = c("Actual_Solar_MW","Actual_Wind_MW","Counterfactual_Wind_MW"),
    names_to = "Type",
    values_to = "Capacity_MW"
  )
print(wind_counterfactual_plot_data)
#------------------------------------------------------------------------------------------------------------------------------------------------
# 4.2 Annual Cumulative Capacity - Counterfactual Plot for Wind
#------------------------------------------------------------------------------------------------------------------------------------------------

#Plotting counterfactual wind
wind_counterfactual_plot <- ggplot(wind_counterfactual_plot_data, aes(x=Year,y=Capacity_MW,colour = Type,group = Type))+
  #Defining the line size
  geom_line(size = 1.5)+
  #Defining the point size
  geom_point(size = 3)+
  labs(
    title = "Counterfactual Cumulative Capacity - Wind",
    subtitle = "Years : 2011 to 2024",
    x = "",
    y = "Cumulative Capcity (MW)"
  )+
  theme_minimal()+
  #Using 'continuous' to ensure all years are indexed in the plot
  scale_x_continuous(breaks = unique(wind_counterfactual_plot_data$Year))+
  #Using comma for capacity values for readability
  scale_y_continuous(labels = scales::comma)+
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

print(wind_counterfactual_plot)



#------------------------------------------------------------------------------------------------------------------------------------------------
# 5.1 Annual Cumulative Capacity - Event Study Model
#------------------------------------------------------------------------------------------------------------------------------------------------

#Analysing whether the policy introduction & implementation during the years 2016 & 2017 caused the slowdown
#The analysis is done using an event study plot

#Creating a dataset for event study
sw_event_study_data <- sw_installed %>%
  #Creating a 'treated' dummy variable - wind
  mutate(treated = ifelse(Sector == "Wind",1,0),
         #Creating a 'post-treatment' dummy variable
         #Defining 'post-treatment' period as year 2017 onwards
         post = ifelse(Year >= 2017,1,0))
#Creating a 'time_to_event' - Years relative to the policy year
sw_event_study_data <- sw_event_study_data %>%
  mutate( time_to_event = Year - 2017) 
  #filter( time_to_event >= -5 & time_to_event <= 5)
sw_event_study_model <- 
  feols(Annual_Installed_Capacity_MW ~ i(time_to_event, treated, ref = -1)|States_Uts,
                              data = sw_event_study_data
                              )

summary(sw_event_study_model)

#Creating clusters to eliminate assumption of independency
sw_event_study <- summary(sw_event_study_model)
sw_event_study_df <- tidy(sw_event_study)
write_xlsx(sw_event_study_df,path = "sw_event_study_df.xlsx")
print(sw_event_study)
print(sw_event_study_df)
print(sw_event_study_model)


#Defining the global parameters of font, plot margin, color and text size
par(family = my_font, mar = c(6, 6, 4, 12) + 0.1 ) # Added +0.1 to match default margins starting at 0.1

#Plotting of Event Study Model
sw_event_study_plot <- iplot(sw_event_study_model,
                             xlab = "Years Relative to Policy (2017)",
                             ylab = "Differential Effect on Capacity (Wind vs. Solar)",
                             main = "Event Study: Dynamic Impact of Policy",
                             col = "darkblue", # Defining color for coefficients
                             ci.col = "gray70", # Defining confidence interval color
                             lwd = 2,
                             #Customizing the zero line parameters - color and line type
                             zero.par = list(col = "red", lty = 2), 
                             ref.line = TRUE, #Setting this to TRUE to draw the ref line
                             #Customizing ref line parameters - color and line type 
                             ref.line.par = list(col = "steelblue", lty = 2),
                             #Using grid for readability
                             grid = TRUE)

# Adding a vertical line at the policy year (time_to_event = 0)
abline(v = 0, col = "darkgreen", lty = 3)
legend(x = par("usr")[2]+ 0.3, #Placing the legend to the right of the plot area
       y = par("usr")[4],      #Placing the legend to the top of the plot area
       legend = c("Coefficient Estimates", "Confidence Interval", "Zero Effect Line",
                  "Reference Period (t=-1)", "Policy Change Year (t=0)"),
       col = c("darkblue", "gray70", "red", "steelblue", "darkgreen"),
       lty = c(1, 1, 2, 2, 3), # Solid for coefficients/CI, dashed for zero/ref, dotted for policy year
       lwd = c(1.5, 1, 1, 1, 1), # Line width for each item 
       bty = "n", #No box around the legend
       cex = 0.9, #Defining legend text size
       xpd = TRUE)

print(sw_event_study_plot)



# First, make sure your sw_event_study_model is generated correctly.
# For example, if using the did package:
install.packages("did")
library(did)
data("mpdta")
# sw_event_study_model <- att_gt(yname="lemp",tname="year",idname="id",gname="first.treat",data=mpdta)

plot(sw_event_study_model) # Use plot() if it's a did object, it calls iplot internally
# Or your original iplot call without too many custom parameters:
# sw_event_study_plot <- iplot(sw_event_study_model)
#------------------------------------------------------------------------------------------------------------------------------------------------
# LCOE
#------------------------------------------------------------------------------------------------------------------------------------------------

lcoe <- read.csv("LCOE.csv")
lcoe <- lcoe %>%
  mutate(
    LCOE_Difference = LCOE_Solar - LCOE_Wind  
  )

lcoe_data <- lcoe %>%
  mutate(
    lcoe_centered = LCOE_Difference - mean_lcoe_diff
  )


#------------------------------------------------------------------------------------------------------------------------------------------------
# Tarriff
#------------------------------------------------------------------------------------------------------------------------------------------------


tariff <- read.csv("Tariff.csv")
tariff <- tariff %>%
  filter(Year != is.na(NA)) %>%
  mutate(
    Tariff_Difference = Solar_Avg_Tarriff - Wind_Avg_Tarriff
  )


#------------------------------------------------------------------------------------------------------------------------------------------------
# ADF Tests
#------------------------------------------------------------------------------------------------------------------------------------------------

adf.test(its_policy$Annual_Installed_Capacity_MW, alternative = "stationary")
adf.test(its_tariffs$Solar_Avg_Tarriff, alternative = "stationary")
adf.test(its_tariffs$Wind_Avg_Tarriff, alternative = "stationary")
adf.test(its_tariff_diff$LCOE_Difference, alternative = "stationary")
adf.test(its_tariff_diff$Tariff_Difference, alternative = "stationary")
adf.test(its_policy$Years_after_policy, alternative = "stationary")
adf.test(its_tariffs$Years_from_base, alternative = "stationary")


#------------------------------------------------------------------------------------------------------------------------------------------------
# ITS Policy - SIGNIFICANT , NO MULTICOLLI
#------------------------------------------------------------------------------------------------------------------------------------------------


intervention_year <- 2017

its_policy <- sw_installed %>%
  filter(Sector == "Wind") %>%
  mutate(
    Policy_shift = ifelse(Year >= intervention_year,1,0),
    Years_after_policy =  ifelse(Year >= intervention_year, Year - intervention_year,0))

its_policy_model <- lm(Annual_Installed_Capacity_MW ~ Years_after_policy + Policy_shift, data = its_policy)
summary(its_policy_model)

dwtest(its_policy_model)

NW_test_policy <- coeftest(its_policy_model, vcov = vcovHAC(its_policy_model))
print(NW_test_policy)

vif(its_policy_model)

#------------------------------------------------------------------------------------------------------------------------------------------------
# ITS with Independent Tariff - SIGNIFICANT**
#------------------------------------------------------------------------------------------------------------------------------------------------

# Calculate the means of the tariff variables before centering
mean_solar_tariff <- mean(tariff$Solar_Avg_Tarriff, na.rm = TRUE)
mean_wind_tariff <- mean(tariff$Wind_Avg_Tarriff, na.rm = TRUE)

intervention_year <- 2017
its_independent <- sw_installed %>%
  filter(Sector == "Wind") %>%
  mutate(
    Years_from_base = Year - min(Year),
    Years_after_policy = ifelse(Year >= intervention_year, Year - intervention_year, 0),
    Policy_shift = ifelse(Year >= intervention_year, 1, 0)
  ) %>%
  left_join(tariff %>% select(Year, Solar_Avg_Tarriff, Wind_Avg_Tarriff), by = "Year") %>%
  filter(Year != "2024") %>%
  # *** Add these lines to center the tariff variables ***
  mutate(
    Solar_Avg_Tariff_Centered = Solar_Avg_Tarriff - mean_solar_tariff,
    Wind_Avg_Tariff_Centered = Wind_Avg_Tarriff - mean_wind_tariff
  )
# Re-run the model using the centered variables
its_independent_model <- feols(Annual_Installed_Capacity_MW ~ Years_after_policy + Policy_shift + Solar_Avg_Tariff_Centered + 
                                 Wind_Avg_Tariff_Centered + I(Wind_Avg_Tariff_Centered * Years_after_policy), data = its_independent)

summary(its_independent_model)
its_ind_data <- tidy(its_independent_model)
print(its_ind_data)
vif(its_independent_model) # Check VIFs again
dwtest(its_independent_model)

NW_test_ind <- coeftest(its_independent_model, vcov = vcovHAC(its_independent_model))
print(NW_test_ind)

library(dwtest)



#------------------------------------------------------------------------------------------------------------------------------------------------
# ITS with LCOE Difference  - INSIGNIFICANT
#------------------------------------------------------------------------------------------------------------------------------------------------
mean_lcoe_diff <- mean(lcoe$LCOE_Difference, na.rm = TRUE)

intervention_year <- 2017
its_lcoe <- sw_installed %>%
  filter(Sector == "Wind") %>%
  mutate(
    Years_from_base = Year - min(Year),
    Years_after_policy = ifelse(Year >= intervention_year, Year - intervention_year, 0),
    Policy_shift = ifelse(Year >= intervention_year,1,0)) %>%
  left_join(lcoe %>% select(Year,LCOE_Difference), by = "Year") %>%
  filter(Year != "2024") %>%
  mutate(
    lcoe_centered = LCOE_Difference - mean_lcoe_diff
  )
str(its_lcoe)
its_lcoe_model <- lm(Annual_Installed_Capacity_MW ~ Years_after_policy + lcoe_centered + Years_after_policy:lcoe_centered, data = its_lcoe)
summary(its_lcoe_model)

dwtest(its_lcoe_model)

NW_test_LCOE <- coeftest(its_lcoe_model, vcov = vcovHAC(its_lcoe_model))
print(NW_test_LCOE)
vif(its_lcoe_model)


#------------------------------------------------------------------------------------------------------------------------------------------------
# ITS with Tariff Difference - SIGNIFICANT
#------------------------------------------------------------------------------------------------------------------------------------------------

intervention_year <- 2017

its_tariff_diff <- sw_installed %>%
  filter(Sector == "Wind") %>%
  mutate(
    Years_from_base = Year - min(Year),
    Years_after_policy =  ifelse(Year >= intervention_year, Year - intervention_year,0),

    Policy_shift = ifelse(Year >= intervention_year,1,0)) %>%
  left_join(lcoe %>% select(Year,LCOE_Difference), by = "Year") %>%
  left_join(tariff %>% select(Year,Tariff_Difference),by ="Year")%>%
  mutate(
    Tariff_Difference_Centered = Tariff_Difference - mean_tariff_diff)



its_tariff_diff_model3 <- lm(Annual_Installed_Capacity_MW ~  Policy_shift + Years_after_policy + Tariff_Difference_Centered +
                               Years_after_policy:Tariff_Difference_Centered,
                               data = its_tariff_diff)

summary(its_tariff_diff_model3)


NW_test_tariff_diff3 <- coeftest(its_tariff_diff_model3, vcov = vcovHAC(its_tariff_diff_model3))
print(NW_test_tariff_diff3)
vif(its_tariff_diff_model3)


a#------------------------------------------------------------------------------------------------------------------------------------------------
# 5. Initial data loading and cleaning - State Level Data
#------------------------------------------------------------------------------------------------------------------------------------------------

#Loading the state level annual installed capacity additions data - wind
winds <- read_xlsx("winds.xlsx")
names(winds)
windsp <- winds %>%
  pivot_longer(
    cols = c("2015", "2016","2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
    names_to = "Year",
    values_to = "Installed_Capacity"
  ) %>%
  mutate( Sector = "Wind")%>%
  select(States_Uts,Sector,Year,Installed_Capacity)%>%
  mutate(across(where(is.numeric), ~replace_na(.,0)))

#Loading the state level annual installed capacity additions data - solar
solar <- read_xlsx("solar.xlsx")
names(solar)
solarp <- solar %>%
  pivot_longer(
    cols = c("2015", "2016","2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
    names_to = "Year",
    values_to = "Installed_Capacity"
  ) %>%
  mutate( Sector = "Solar")%>%
  select(States_Uts,Sector,Year,Installed_Capacity)%>%
  mutate(across(where(is.numeric), ~replace_na(.,0)))

#Combining the solar & wind datasets
sw_states <- full_join(windsp,solarp)%>%
  filter(States_Uts %in% c("ANDHRA PRADESH","GUJARAT","KARNATAKA","KERALA","MADHYA PRADESH","MAHARASHTRA","RAJASTHAN","TAMIL NADU","TELANGANA"))%>%
  mutate(States_Uts = trimws(States_Uts),
         Year = as.numeric(Year))%>%
  arrange(States_Uts,Year)


sw_others_data <- sw_states %>%
  filter(!States_Uts %in% c("ANDHRA PRADESH","GUJARAT","KARNATAKA","KERALA","MADHYA PRADESH","MAHARASHTRA","RAJASTHAN","TAMIL NADU","TELANGANA"))%>%
  mutate(
    States_Uts = "Others",
    Installed_Capacity = sum(Installed_Capacity)
  )
  
  









#------------------------------------------------------------------------------------------------------------------------------------------------
# 5.Descriptive Plotting of Annual Installed Capacity Additions - State Level Data
#------------------------------------------------------------------------------------------------------------------------------------------------

#Plotting annual installed capacity for both sectors
sw_states_plot <- ggplot(sw_states, aes(x=Year,y=Installed_Capacity,colour = Sector,group = Sector))+
  #Defining the line size
  geom_line(size = 1)+
  #Defining the point size
  geom_point(size = 1.5)+
  labs(
    title = "Statewise Annual Installed Capacity Additions: Wind vs Solar",
    subtitle = "Year : From 2014 to 2024",
    x = "",
    y = "Energy Sector"
  )+
  theme_minimal()+
  #Using a line to represent the intervention - policy shift in 2017
  #Using an immediately previous time period for clarity
  geom_vline(xintercept = 2016.5,linetype = "dashed",color = "grey50", size = 0.8)+
  #Defining the labels for the intervention line
  annotate("text",x=2016.5,y=max(sw_states$Installed_Capacity)*0.9,
           label = "Policy Shift (2017)",color = "grey30",hjust = -0.1)+
  facet_wrap(~ States_Uts, scales = "free_y")+
  #Using 'continuous' to ensure all years are indexed in the plot
  scale_x_continuous(breaks = unique(sw_states$Year))+
  #Using comma for capacity values for readability
  scale_y_continuous(labels = scales::comma)+
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

print(sw_states_plot)

#------------------------------------------------------------------------------------------------------------------------------------------------
# 6. Creating Dummy Variable for sector
#------------------------------------------------------------------------------------------------------------------------------------------------

sw_states <- sw_states %>%
  mutate(Year = as.numeric(Year),
         #Creating dummy variables for sector
         treated_states = ifelse(Sector == "Wind",1,0))

#------------------------------------------------------------------------------------------------------------------------------------------------
# 7. Event Study Model for Heterogeneity Analysis
#------------------------------------------------------------------------------------------------------------------------------------------------

#Creating a 'time_to_event' - Years relative to the policy year
sw_states$time_to_event <- sw_states$Year - 2017
sw_states_event_study_model <- feols(Installed_Capacity ~ i(time_to_event, treated_states, ref = -1) | States_Uts,
                                     data = sw_states)

#Creating clusters to eliminate assumption of independency
sw_event_study <- summary(sw_states_event_study_model,cluster = ~States_Uts)
sw_event_study_df <- tidy(sw_event_study)
write_xlsx(sw_event_study_df,path = "sw_event_study_df.xlsx")


sw_states_hetero <- sw_states %>%
  mutate( 
    #Creating a dummy variable for top 2 wind performers for robust analysis
    is_GT_TN = ifelse(States_Uts %in% c("TAMIL NADU"),1,0),
    treated_states = ifelse(Sector == "Wind",1,0))

#Creating a 'time_to_event' - Years relative to the policy year
sw_states_hetero$time_to_event <- sw_states_hetero$Year - 2017
sw_hetero_event_study_model <- feols(Installed_Capacity ~ i(time_to_event, treated_states, ref = -1)+
                                       i(time_to_event,treated_states*is_GT_TN,ref = -1)| States_Uts + Year,
                                     data = sw_states_hetero,
                                     cluster = ~States_Uts)

#Creating clusters to eliminate assumption of independency
sw_hetero_event_study <- summary(sw_hetero_event_study_model,cluster = ~States_Uts)
sw_hetero_event_study_df <- tidy(sw_hetero_event_study)
print(sw_hetero_event_study)
#write_xlsx(sw_event_study_df,path = "sw_event_study_df.xlsx")


# --- Extract coefficients and calculate combined effects for plotting ---


# 1. Extract coefficients for the 'Other States' baseline
baseline_coefs <- sw_hetero_event_study_df %>%
  # Match: "time_to_event::X:treated_states"
  filter(grepl("^time_to_event::(-?\\d+):treated_states$", term)) %>%
  # Ensure these are NOT the interaction terms
  filter(!grepl("is_GT_TN", term)) %>% # Filter out terms containing "is_GT_TN"
  mutate(
    event_time = as.numeric(gsub("time_to_event::(-?\\d+):treated_states", "\\1", term)),
    group = "Other States (Wind vs Solar)"
  ) %>%
  select(event_time, estimate, std.error, group)


# 2. Extract coefficients for the Gujarat/TN *difference*
gt_tn_diff_coefs <- sw_hetero_event_study_df %>%
  # Match: "time_to_event::X:treated_states * is_GT_TN"
  filter(grepl("^time_to_event::(-?\\d+):treated_states \\* is_GT_TN$", term)) %>%
  mutate(
    event_time = as.numeric(gsub("time_to_event::(-?\\d+):treated_states \\* is_GT_TN", "\\1", term)),
    group = "GT/TN Difference (from Other States)"
  ) %>%
  select(event_time, estimate, std.error, group)


# --- 3. Calculate the *combined* effect for Gujarat/TN ---
gt_tn_combined_coefs <- baseline_coefs %>%
  rename(baseline_estimate = estimate, baseline_std.error = std.error) %>%
  left_join(gt_tn_diff_coefs %>%
              rename(diff_estimate = estimate, diff_std.error = std.error),
            by = "event_time") %>%
  mutate(
    estimate = baseline_estimate + diff_estimate,
    std.error = sqrt(baseline_std.error^2 + diff_std.error^2), # Using sqrt(sum of squares for SE)
    group = "Gujarat/TN Combined Effect (Wind vs Solar)"
  ) %>%
  select(event_time, estimate, std.error, group)

# --- 4. Combine all for plotting ---
plot_data <- bind_rows(baseline_coefs, gt_tn_combined_coefs) %>%
  # Add the reference period (t-1 = -1) with 0 effect, as it's the baseline
  add_row(event_time = -1, estimate = 0, std.error = 0, group = "Other States (Wind vs Solar)") %>%
  add_row(event_time = -1, estimate = 0, std.error = 0, group = "Gujarat/TN Combined Effect (Wind vs Solar)") %>%
  # Calculate confidence intervals (95% CI using 1.96 * SE)
  mutate(
    lower_ci = estimate - 1.96 * std.error,
    upper_ci = estimate + 1.96 * std.error
  ) %>%
  arrange(group, event_time)

# --- 5. Plot using ggplot2 ---
ggplot(plot_data, aes(x = event_time, y = estimate, color = group, group = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") + # Policy start (between t=-1 and t=0)
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 0.4)) +
  geom_line(position = position_dodge(width = 0.4)) +
  labs(
    title = "Event Study: Differential Policy Impact on Wind Capacity",
    subtitle = "Comparing Gujarat/Tamil Nadu with Other States (Ref Year = 2016)",
    x = "Years Relative to Policy Shift (2017)",
    y = "Estimated Impact (MW)",
    color = "Group"
  ) +
  scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")










