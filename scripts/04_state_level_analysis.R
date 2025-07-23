# 04_state_level_analysis.R
# This script focuses on state-level data analysis and visualization.

# --- Descriptive Plotting 1 - Annual Installed Capacity State Wise ---

# Plotting annual installed capacity for both sectors
sw_states_plot <- ggplot(sw_states, aes(x=Year,y=Installed_Capacity,colour = Sector,group = Sector))+
  geom_line(size = 1)+ # Defining the line size
  geom_point(size = 1.5)+ # Defining the point size
  labs(
    title = "Statewise Annual Installed Capacity Additions: Wind vs Solar",
    subtitle = "Year : From 2015 to 2024",
    x = "",
    y = "Annual Installed Capacity (MW)" # Corrected Y-axis label
  )+
  theme_minimal()+
  # Using a line to represent the intervention - policy shift in 2017
  # Using an immediately previous time period for clarity
  geom_vline(xintercept = 2016.5,linetype = "dashed",color = "grey50", size = 0.8)+
  # Defining the labels for the intervention line
  annotate("text",x=2016.5,y=max(sw_states$Installed_Capacity)*0.9,
           label = "Policy Shift (2017)",color = "grey30",hjust = -0.1)+
  # Plotting separate graphs for each state
  facet_wrap(~ States_Uts, scales = "free_y")+
  # Using 'continuous' to ensure all years are indexed in the plot
  scale_x_continuous(breaks = unique(sw_states$Year))+
  # Using comma for capacity values for readability
  scale_y_continuous(labels = scales::comma)+
  theme(
    # Customizing the plot elements
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

ggsave(filename = file.path(plots_dir,"sw_states_plot.png"),
       plot = sw_states_plot,
       device = "png",
       width = 12,
       height = 10,
       units = "in",
       dpi = 300)

# --- ITS Policy Model with Fixed Effects for States ---

# Creating an ITS Model with fixed effects for states
its_states_policy_model <- feols(Annual_Installed_Capacity_MW ~ Years_after_policy + Policy_shift | States_Uts,
                                 data = its_states_data,
                                 vcov = ~States_Uts) # Clustered standard errors by state

summary(its_states_policy_model)
its_states_policy_model_df <- tidy(summary(its_states_policy_model))
# Storing the summary database as an excel file
write_xlsx(its_states_policy_model_df, path = file.path(tables_dir, "its_states_policy_model_df.xlsx"))
