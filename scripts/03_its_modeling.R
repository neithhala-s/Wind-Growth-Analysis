# 03_its_modeling.R
# This script performs Interrupted Time Series (ITS) modeling
# using national-level data.

# --- ITS Policy Model ---

# Constructing an ITS Model using feols()
its_policy_model <- feols(Annual_Installed_Capacity_MW ~ Years_after_policy + Policy_shift, data = its_data,
                          # Specifying heteroskedasticity-robust standard errors using 'hetero'
                          vcov = "hetero")

summary(its_policy_model)
# Creating a database for storing the summary of the model
its_policy_model_df <- tidy(summary(its_policy_model))
# Storing the summary database as an excel file
write_xlsx(its_policy_model_df, path = file.path(tables_dir, "its_policy_model_df.xlsx"))

# --- ITS Tariff Model ---

# Constructing an ITS Model using feols()
its_tariff_model <- feols(Annual_Installed_Capacity_MW ~ Years_after_policy + Policy_shift + Solar_Avg_Tariff_Centered +
                            Wind_Avg_Tariff_Centered + I(Wind_Avg_Tariff_Centered * Years_after_policy), data = its_data,
                          # Specifying heteroskedasticity-robust standard errors using 'hetero'
                          vcov = "hetero"
)

summary(its_tariff_model)
# Creating a database for storing the summary of the model
its_tariff_model_df <- tidy(summary(its_tariff_model))
# Storing the summary database as an excel file
write_xlsx(its_tariff_model_df, path = file.path(tables_dir, "its_tariff_model_df.xlsx"))

# --- ITS Differenced LCOE Model ---

# Constructing an ITS Model using feols()
its_lcoe_model <- feols(Annual_Installed_Capacity_MW ~ Years_after_policy + LCOE_centered + Years_after_policy:LCOE_centered, data = its_data,
                        # Specifying heteroskedasticity-robust standard errors using 'hetero'
                        vcov = "hetero")
summary(its_lcoe_model)
# Creating a database for storing the summary of the model
its_lcoe_model_df <- tidy(summary(its_lcoe_model))
# Storing the summary database as an excel file
write_xlsx(its_lcoe_model_df, path = file.path(tables_dir, "its_lcoe_model_df.xlsx"))
