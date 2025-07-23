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
library(stringr)  
library(tidyr)
my_font <- "Times New Roman"

#---INDEX------------------------------------------------------------------------------------------------------------------------------------------------
#PART 1 - ANALYSIS USING NATIONAL LEVEL DATA
#INITIAL DATA LOADING & CLEANING
#DESCRIPTIVE PLOTTING 1 - ANNUAL CUMULATIVE CAPACITY
#DESCRIPTIVE PLOTTING 2 - ANNUAL INSTALLED CAPACITY
#ANALYSIS OF ANNUAL CUMULATIVE CAPACITY USING CAGR
#COUNTERFACTUAL ANNUAL CUMULATIVE CAPACITY USING CAGR
#DESCRIPTIVE PLOTTING 3 - COUNTERFACTUAL ANNUAL CUMULATIVE CAPACITY
#ITS DATA CREATION WITH NATIONAL LEVEL DATA
#ITS POLICY MODEL 
#ITS TARIFF MODEL
#ITS DIFFERENCED LCOE MODEL


#PART 2 - ANALYSIS USING STATE LEVEL DATA
#INITIAL DATA LOADING & CLEANING
#DESCRIPTIVE PLOTTING 1 - ANNUAL INSTALLED CAPACITY STATE WISE
#EVENT STUDY MODEL - ANNUAL INSTALLED CAPACITY WITH FIXED EFFECTS FOR STATES
#EVENT STUDY MODEL - ANNUAL INSTALLED CAPACITY WITH HETEROGENEITY EFFECTS 
#ITS DATA CREATION WITH STATE LEVEL DATA
#ITS POLICY MODEL WITH FIXED EFFECTS FOR STATES




#---PART 1 - ANALYSIS USING NATIONAL LEVEL DATA------------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---INITIAL DATA LOADING & CLEANING------------------------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---DESCRIPTIVE PLOTTING 1 - ANNUAL CUMULATIVE CAPACITY------------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---DESCRIPTIVE PLOTTING 2 - ANNUAL INSTALLED CAPACITY-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---ANALYSIS OF ANNUAL CUMULATIVE CAPACITY USING CAGR-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---COUNTERFACTUAL ANNUAL CUMULATIVE CAPACITY USING CAGR-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
print(wind_counterfactual)

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

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---DESCRIPTIVE PLOTTING 3 - COUNTERFACTUAL ANNUAL CUMULATIVE CAPACITY-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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


#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---ITS DATASET CREATION--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Importing the tariff dataset
tariff <- read.csv("Tariff.csv") %>%
  #Removing data discrepancies
  filter(Year != is.na(NA))

# Calculating the means of the tariff variables for centering
mean_solar_tariff <- mean(tariff$Solar_Avg_Tarriff, na.rm = TRUE)
mean_wind_tariff <- mean(tariff$Wind_Avg_Tarriff, na.rm = TRUE)

tariff <- tariff %>%
    mutate(
      #Calculating centered values of tariff variables
      Solar_Avg_Tariff_Centered = Solar_Avg_Tarriff - mean_solar_tariff,
      Wind_Avg_Tariff_Centered = Wind_Avg_Tarriff - mean_wind_tariff)
    
#Importing the LCOE dataset
lcoe <- read.csv("LCOE.csv") %>%
  #Removing data discrepancies
  filter(Year != is.na(NA)) %>%
  mutate(LCOE_Difference = LCOE_Solar - LCOE_Wind)

# Calculating the mean of the LCOE variable for centering
mean_lcoe_diff <- mean(lcoe$LCOE_Difference, na.rm = TRUE)

lcoe <- lcoe %>%
  mutate(
    #Calculating the centered values of LCOE variable
    LCOE_centered = LCOE_Difference - mean_lcoe_diff
  )


#Assigning a variable for intervention year
intervention_year <- 2017

#Creating a dataset for ITS Modelling
its_data <- sw_installed %>%
  filter(Sector == "Wind") %>%
  mutate(
    #Creating a variable to indicate years after policy shift
    Policy_shift = ifelse(Year >= intervention_year,1,0),
    #Creating a variable to indicate the number of years after policy shift
    Years_after_policy =  ifelse(Year >= intervention_year, Year - intervention_year,0)) %>%
  #Joining the tariff dataset
  left_join(tariff %>% select(Year, Solar_Avg_Tariff_Centered, Wind_Avg_Tariff_Centered), by = "Year") %>%
  #Joining the LCOE dataset
  left_join(lcoe %>% select(Year,LCOE_centered), by = "Year") 
  
  

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---ITS POLICY MODEL--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Constructing an ITS Model using feols()
its_policy_model <- feols(Annual_Installed_Capacity_MW ~ Years_after_policy + Policy_shift, data = its_data,
                          #Specifying heteroskedasticity-robust standard errors using 'hetero'
                          vcov = "hetero")
                         
summary(its_policy_model)
#Creating a database for storing the summary of the model
its_policy_model_df <- tidy(summary(its_policy_model))
#Storing the summary database as an excel file
#write_xlsx(its_policy_model_df,path = "its_policy_model_df.xlsx")


#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---ITS TARIFF MODEL-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Constructing an ITS Model using feols()
its_tariff_model <- feols(Annual_Installed_Capacity_MW ~ Years_after_policy + Policy_shift + Solar_Avg_Tariff_Centered + 
                                 Wind_Avg_Tariff_Centered + I(Wind_Avg_Tariff_Centered * Years_after_policy), data = its_data,
                          #Specifying heteroskedasticity-robust standard errors using 'hetero'
                          vcov = "hetero"
                          )

summary(its_tariff_model)
#Creating a database for storing the summary of the model
its_tariff_model_df <- tidy(summary(its_tariff_model))
#Storing the summary database as an excel file
#write_xlsx(its_tariff_model_df,path = "its_tariff_model_df.xlsx")

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---ITS DIFFERENCED LCOE MODEL-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Constructing an ITS Model using feols()
its_lcoe_model <- feols(Annual_Installed_Capacity_MW ~ Years_after_policy + LCOE_centered + Years_after_policy:LCOE_centered, data = its_data,
                        #Specifying heteroskedasticity-robust standard errors using 'hetero'
                        vcov = "hetero")
summary(its_lcoe_model)
#Creating a database for storing the summary of the model
its_lcoe_model_df <- tidy(summary(its_lcoe_model))
#Storing the summary database as an excel file
#write_xlsx(its_lcoe_model_df,path = "its_lcoe_model_df.xlsx")


#---PART 2 - ANALYSIS USING STATE LEVEL DATA-----------------------------------------------------------------------------------------------------------------------------------------------

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---INITIAL DATA LOADING & CLEANING-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Using the dataset for timeline 2015 to 2024 to ensure consistency across both the sectors
#Loading the state level annual installed capacity additions data - wind
winds <- read_xlsx("winds.xlsx")
names(winds)
windsp <- winds %>%
  #Pivoting the dataset longer for plotting
  pivot_longer(
    cols = c("2015", "2016","2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
    names_to = "Year",
    values_to = "Installed_Capacity"
  ) %>%
  #Adding a column indicating the sector name
  mutate( Sector = "Wind")%>%
  select(States_Uts,Sector,Year,Installed_Capacity)%>%
  #Removing NAs across the dataset to remove data discrepancies
  mutate(across(where(is.numeric), ~replace_na(.,0)))

#Loading the state level annual installed capacity additions data - solar
solar <- read_xlsx("solar.xlsx")
names(solar)
solarp <- solar %>%
  #Pivoting the dataset longer for plotting
  pivot_longer(
    cols = c("2015", "2016","2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
    names_to = "Year",
    values_to = "Installed_Capacity"
  ) %>%
  #Adding a column indicating the sector name
  mutate( Sector = "Solar")%>%
  select(States_Uts,Sector,Year,Installed_Capacity)%>%
  #Removing NAs across the dataset to remove data discrepancies
  mutate(across(where(is.numeric), ~replace_na(.,0)))

#Combining the solar & wind datasets
sw_states <- full_join(windsp,solarp, by = c("States_Uts","Year"))%>%
  #Filtering out the nine windy states for analysis
  filter(States_Uts %in% c("ANDHRA PRADESH","GUJARAT","KARNATAKA","KERALA","MADHYA PRADESH","MAHARASHTRA","RAJASTHAN","TAMIL NADU","TELANGANA"))%>%
  mutate(
    #Trimming the white spaces in States_Uts column
    States_Uts = trimws(States_Uts),
    #Specifying the Year column as numeric datatype
    Year = as.numeric(Year))%>%
  arrange(States_Uts,Year)


#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---DESCRIPTIVE PLOTTING 1 - ANNUAL INSTALLED CAPACITY STATE WISE-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#dev.off() #Uncomment if graphics issue arises
#Plotting annual installed capacity for both sectors
sw_states_plot <- ggplot(sw_states, aes(x=Year,y=Installed_Capacity,colour = Sector,group = Sector))+
  #Defining the line size
  geom_line(size = 1)+
  #Defining the point size
  geom_point(size = 1.5)+
  labs(
    title = "Statewise Annual Installed Capacity Additions: Wind vs Solar",
    subtitle = "Year : From 2015 to 2024",
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
  #Plotting separate graphs for each state
  facet_wrap(~ States_Uts, scales = "free_y")+
  #Using 'continuous' to ensure all years are indexed in the plot
  scale_x_continuous(breaks = unique(sw_states$Year))+
  #Using comma for capacity values for readability
  scale_y_continuous(labels = scales::comma)+
  theme(
    #Customizing the plot elements
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

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---EVENT STUDY MODEL - ANNUAL INSTALLED CAPACITY WITH FIXED EFFECTS FOR STATES-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#dev.off()
#Using the dataset for timeline 2010 to 2023 for event study analysis

#Loading the wind annual installed capacity dataset
wind <- read.csv("wind_11_24.csv") %>%
  #Renaming the column names
  rename_with(~ sub("^X", "", .), starts_with("X")) %>%
  #Replacing NAs with 0
  mutate(across(where(is.numeric), ~replace_na(.,0)))

wind_data <- wind %>%
  #Pivoting the dataset longer for plotting
  pivot_longer(
    cols = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"),
    names_to = "Year",
    values_to = "Annual_Installed_Capacity_MW"
  ) %>%
  #Specifying the datatype of the Year column as numeric
  mutate(Year = as.numeric(Year)) %>%
  #Creating a variable 'time_to_event' for event study 
  mutate(time_to_event = Year - intervention_year)

#Creating an event study model using FEOLS()
wind_event_study_model <- feols(Annual_Installed_Capacity_MW ~ i(time_to_event, ref = -1)
                                #Using fixed effects for States
                                |States_Uts,
                                data = wind_data)
summary(wind_event_study_model)
#Creating a database for storing the summary of the model
wind_event_study_model_df <- tidy(summary(wind_event_study_model))
#Storing the summary database as an excel file
#write_xlsx(wind_event_study_model_df,path = "wind_event_study_model_df.xlsx")


#Defining the global parameters of font and plot margin
par(family = my_font, mar = c(6, 6, 4, 12) + 0.1 ) # Adding +0.1 to match default margins starting at 0.1

#Plotting of Event Study Model
wind_event_study_plot <- iplot(wind_event_study_model,
                               xlab = "Years Relative to Policy (2017)",
                               ylab = "Changes in Installed Capacity",
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

print(wind_event_study_plot)

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---EVENT STUDY MODEL - ANNUAL INSTALLED CAPACITY WITH HETEROGENEITY EFFECTS -----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


wind_hetero <- wind_data %>%
  mutate( 
    #Creating a dummy variable for top 2 wind performers for robust analysis
    is_GT_TN = ifelse(States_Uts %in% c("GUJARAT","TAMIL NADU"),1,0))


#Creating a 'time_to_event' - Years relative to the policy year
wind_hetero$time_to_event <- wind_hetero$Year - 2017
wind_hetero_event_study_model <- feols(Annual_Installed_Capacity_MW ~ i(time_to_event, ref = -1)+
                                       i(time_to_event,is_GT_TN,ref = -1)| States_Uts,
                                     data = wind_hetero,
                                     cluster = ~States_Uts)

#Creating clusters to eliminate assumption of independency
summary(wind_hetero_event_study_model)
wind_hetero_event_study_df <- tidy(summary(wind_hetero_event_study_model, cluster = ~States_Uts))
print(wind_hetero_event_study_df)


#Createing a database for event study
wind_hetero_event_study_plot_df <- wind_hetero_event_study_df %>%
  filter(str_detect(term, "^time_to_event::")) %>% 
  mutate(
    # Extracting the time_to_event value
    event_time = as.numeric(str_extract(term, "(?<=::)-?\\d+")),
    # Create a group variable for overall effect vs differential effect
    group = ifelse(str_detect(term, "is_GT_TN"), "Differential_Effect_GT_TN", "Overall Effect")
  ) %>%
  select(event_time, estimate, std.error, group) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )


#Creating the event study plot using ggplot2
wind_hetero_event_study_plot <- ggplot(wind_hetero_event_study_plot_df, aes(x = event_time, y = estimate, color = group, group = group)) +
  #Defining the point size and line width
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_line(linewidth = 1, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 0.8, position = position_dodge(width = 0.2))+ 
  #Adding a horizontal line at y=0 (no effect)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  #Adding a vertical line at time_to_event = 0 (policy implementation year)
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  
  
  
  #Customising labels and title
  labs(
    title = "Event Study: Impact of Policy on Annual Installed Wind Capacity",
    x = "Years Relative to Policy Year (2017)",
    y = "Estimated Change in Annual Installed Capacity (MW)",
    color = "Effect Type"
  ) +
  
  #Setting x-axis breaks to show all event times clearly
  scale_x_continuous(breaks = unique(wind_hetero_event_study_plot_df$event_time)) +
  
  theme_minimal() +
  theme(
    #Customizing the plot elements
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



print(wind_hetero_event_study_plot)



#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---ITS DATASET CREATION USING STATE LEVEL DATA-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Creating a database using state level data
its_states_data <- wind_data %>%
  mutate(
    #Creating a variable to indicate years after policy shift
    Policy_shift = ifelse(Year >= intervention_year,1,0),
    #Creating a variable to indicate the number of years after policy shift
    Years_after_policy =  ifelse(Year >= intervention_year, Year - intervention_year,0)) 


#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---ITS POLICY MODEL WITH FIXED EFFECTS FOR STATES-----------------------------------------------------------------------------------------------------------------------------------------------
#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Creating an ITS Model with fixed effects for states
its_states_policy_model <- feols(Annual_Installed_Capacity_MW ~ Years_after_policy + Policy_shift |States_Uts,
                          data = its_states_data,
                          vcov = ~States_Uts) 

summary(its_states_policy_model)
its_states_policy_model_df <- tidy(summary(its_states_policy_model))
#Storing the summary database as an excel file
#write_xlsx(its_states_policy_model_df,path = "its_states_policy_model_df.xlsx")

#/---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

