# 01_data_loading_and_cleaning.R
# This script handles the initial loading and cleaning of all raw datasets.
# It prepares the data for subsequent analysis steps.

# --- Initial Data Loading & Cleaning (National Level) ---

# Loading and cleaning 'Annual Cumulative Capacity' dataset
sw_cumulative_raw <- read.csv("data/raw/Annual_Cumulative_Capacity.csv")

sw_cumulative <- sw_cumulative_raw %>%
  # Replacing NAs with 0 and removing whitespaces
  mutate(across(where(is.numeric), ~replace_na(.,0)),
         across(where(is.character), trimws))

# Changing the datatype of Sector to factor for plotting convenience
sw_cumulative$Sector <- as.factor(sw_cumulative$Sector)
sw_cumulative$Year <- as.numeric(sw_cumulative$Year)

# Loading and cleaning 'Annual Installed Capacity' dataset
sw_installed_raw <- read.csv("data/raw/Annual_Installed_Capacity.csv")

sw_installed <- sw_installed_raw %>%
  # Replacing NAs with 0 and removing whitespaces
  mutate(across(where(is.numeric), ~replace_na(.,0)),
         across(where(is.character), trimws))

# Changing the datatype of Sector to factor for plotting convenience
sw_installed$Sector <- as.factor(sw_installed$Sector)
sw_installed$Year <- as.numeric(sw_installed$Year)

# --- ITS Data Creation with National Level Data ---

# Importing the tariff dataset
tariff <- read.csv("data/raw/Tariff.csv") %>%
  # Removing data discrepancies (filter(Year != is.na(NA)) is unusual, assuming it means remove NA in Year)
  filter(!is.na(Year)) # Correct way to remove NA in Year

# Calculating the means of the tariff variables for centering
mean_solar_tariff <- mean(tariff$Solar_Avg_Tarriff, na.rm = TRUE)
mean_wind_tariff <- mean(tariff$Wind_Avg_Tarriff, na.rm = TRUE)

tariff <- tariff %>%
  mutate(
    # Calculating centered values of tariff variables
    Solar_Avg_Tariff_Centered = Solar_Avg_Tarriff - mean_solar_tariff,
    Wind_Avg_Tariff_Centered = Wind_Avg_Tarriff - mean_wind_tariff)

# Importing the LCOE dataset
lcoe <- read.csv("data/raw/LCOE.csv") %>%
  # Removing data discrepancies (filter(Year != is.na(NA)) is unusual, assuming it means remove NA in Year)
  filter(!is.na(Year)) %>% # Correct way to remove NA in Year
  mutate(LCOE_Difference = LCOE_Solar - LCOE_Wind)

# Calculating the mean of the LCOE variable for centering
mean_lcoe_diff <- mean(lcoe$LCOE_Difference, na.rm = TRUE)

lcoe <- lcoe %>%
  mutate(
    # Calculating the centered values of LCOE variable
    LCOE_centered = LCOE_Difference - mean_lcoe_diff
  )

# Creating a dataset for ITS Modelling (Wind Sector only for national ITS)
its_data <- sw_installed %>%
  filter(Sector == "Wind") %>%
  mutate(
    # Creating a variable to indicate years after policy shift
    Policy_shift = ifelse(Year >= intervention_year, 1, 0),
    # Creating a variable to indicate the number of years after policy shift
    Years_after_policy =  ifelse(Year >= intervention_year, Year - intervention_year, 0)
  ) %>%
  # Joining the tariff dataset
  left_join(tariff %>% select(Year, Solar_Avg_Tariff_Centered, Wind_Avg_Tariff_Centered), by = "Year") %>%
  # Joining the LCOE dataset
  left_join(lcoe %>% select(Year, LCOE_centered), by = "Year")

# --- Initial Data Loading & Cleaning (State Level) ---

# Using the dataset for timeline 2015 to 2024 to ensure consistency across both the sectors
# Loading the state level annual installed capacity additions data - wind
winds_raw <- read_xlsx("data/raw/winds.xlsx")

windsp <- winds_raw %>%
  # Pivoting the dataset longer for plotting
  pivot_longer(
    cols = c("2015", "2016","2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
    names_to = "Year",
    values_to = "Installed_Capacity"
  ) %>%
  # Adding a column indicating the sector name
  mutate(Sector = "Wind") %>%
  select(States_Uts, Sector, Year, Installed_Capacity) %>%
  # Removing NAs across the dataset to remove data discrepancies
  mutate(across(where(is.numeric), ~replace_na(.,0)))

# Loading the state level annual installed capacity additions data - solar
solar_raw <- read_xlsx("data/raw/solar.xlsx")

solarp <- solar_raw %>%
  # Pivoting the dataset longer for plotting
  pivot_longer(
    cols = c("2015", "2016","2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
    names_to = "Year",
    values_to = "Installed_Capacity"
  ) %>%
  # Adding a column indicating the sector name
  mutate(Sector = "Solar") %>%
  select(States_Uts, Sector, Year, Installed_Capacity) %>%
  # Removing NAs across the dataset to remove data discrepancies
  mutate(across(where(is.numeric), ~replace_na(.,0)))

# Combining the solar & wind datasets for state-level plots
sw_states <- full_join(windsp, solarp, by = c("States_Uts","Year","Sector","Installed_Capacity")) %>% 
  # Filtering out the nine windy states for analysis
  filter(States_Uts %in% c("ANDHRA PRADESH","GUJARAT","KARNATAKA","KERALA","MADHYA PRADESH","MAHARASHTRA","RAJASTHAN","TAMIL NADU","TELANGANA")) %>%
  mutate(
    # Trimming the white spaces in States_Uts column
    States_Uts = trimws(States_Uts),
    # Specifying the Year column as numeric datatype
    Year = as.numeric(Year)
  ) %>%
  arrange(States_Uts, Year)

# Loading the wind annual installed capacity dataset for Event Study
wind_11_24_raw <- read.csv("data/raw/wind_11_24.csv")

wind_data <- wind_11_24_raw %>%
  # Renaming the column names (e.g., X2011 to 2011)
  rename_with(~ sub("^X", "", .), starts_with("X")) %>%
  # Replacing NAs with 0
  mutate(across(where(is.numeric), ~replace_na(.,0))) %>%
  # Pivoting the dataset longer for analysis
  pivot_longer(
    cols = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"),
    names_to = "Year",
    values_to = "Annual_Installed_Capacity_MW"
  ) %>%
  # Specifying the datatype of the Year column as numeric
  mutate(Year = as.numeric(Year)) %>%
  # Creating a variable 'time_to_event' for event study
  mutate(time_to_event = Year - intervention_year)

# Creating ITS data for state-level ITS model
its_states_data <- wind_data %>%
  mutate(
    # Creating a variable to indicate years after policy shift
    Policy_shift = ifelse(Year >= intervention_year, 1, 0),
    # Creating a variable to indicate the number of years after policy shift
    Years_after_policy =  ifelse(Year >= intervention_year, Year - intervention_year, 0)
  )
