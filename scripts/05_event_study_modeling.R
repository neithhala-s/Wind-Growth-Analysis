# 05_event_study_modeling.R
# This script performs Event Study analysis on state-level data.

# --- Event Study Model - Annual Installed Capacity with Fixed Effects for States ---

# Creating an event study model using FEOLS()
wind_event_study_model <- feols(Annual_Installed_Capacity_MW ~ i(time_to_event, ref = -1) | States_Uts,
                                data = wind_data,
                                cluster = ~States_Uts) # Clustered standard errors by state
summary(wind_event_study_model)
# Creating a database for storing the summary of the model
wind_event_study_model_df <- tidy(summary(wind_event_study_model))
# Storing the summary database as an excel file (uncomment if you want to save)
# write_xlsx(wind_event_study_model_df, path = file.path(tables_dir, "wind_event_study_model_df.xlsx"))

# Saving the iplot to a PNG file
png(filename = file.path(plots_dir, "wind_event_study_plot.png"),
    width = 12,
    height = 10,
    units = "in",
    res = 300)

# Plotting of Event Study Model (this draws to the opened PNG device)
iplot(wind_event_study_model,
      xlab = "Years Relative to Policy (2017)",
      ylab = "Changes in Installed Capacity",
      main = "Event Study: Dynamic Impact of Policy",
      col = "darkblue", # Defining color for coefficients
      ci.col = "gray70", # Defining confidence interval color
      lwd = 2,
      # Customizing the zero line parameters - color and line type
      zero.par = list(col = "red", lty = 2),
      ref.line = TRUE, # Setting this to TRUE to draw the ref line
      # Customizing ref line parameters - color and line type
      ref.line.par = list(col = "steelblue", lty = 2),
      # Using grid for readability
      grid = TRUE)

# Adding a vertical line at the policy year (time_to_event = 0)
abline(v = 0, col = "darkgreen", lty = 3)
legend(x = par("usr")[2]+ 0.3, # Placing the legend to the right of the plot area
       y = par("usr")[4],      # Placing the legend to the top of the plot area
       legend = c("Coefficient Estimates", "Confidence Interval", "Zero Effect Line",
                  "Reference Period (t=-1)", "Policy Change Year (t=0)"),
       col = c("darkblue", "gray70", "red", "steelblue", "darkgreen"),
       lty = c(1, 1, 2, 2, 3), # Solid for coefficients/CI, dashed for zero/ref, dotted for policy year
       lwd = c(1.5, 1, 1, 1, 1), # Line width for each item
       bty = "n", # No box around the legend
       cex = 0.9, # Defining legend text size
       xpd = TRUE)

dev.off() # Close the graphics device to save the plot


# --- Event Study Model - Annual Installed Capacity with Heterogeneity Effects ---

wind_hetero <- wind_data %>%
  mutate(
    # Creating a dummy variable for top 2 wind performers for robust analysis
    is_GT_TN = ifelse(States_Uts %in% c("GUJARAT","TAMIL NADU"),1,0))

# Creating a 'time_to_event' - Years relative to the policy year
# This was already created in wind_data, but re-creating here for clarity in this script's context
wind_hetero$time_to_event <- wind_hetero$Year - intervention_year

wind_hetero_event_study_model <- feols(Annual_Installed_Capacity_MW ~ i(time_to_event, ref = -1) +
                                         i(time_to_event, is_GT_TN, ref = -1) | States_Uts,
                                       data = wind_hetero,
                                       cluster = ~States_Uts)

# Creating clusters to eliminate assumption of independency
summary(wind_hetero_event_study_model)
wind_hetero_event_study_df <- tidy(summary(wind_hetero_event_study_model, cluster = ~States_Uts))
print(wind_hetero_event_study_df)


# Creating a database for event study plot
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


# Creating the event study plot using ggplot2
wind_hetero_event_study_plot <- ggplot(wind_hetero_event_study_plot_df, aes(x = event_time, y = estimate, color = group, group = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) + # Defining the point size and line width
  geom_line(linewidth = 1, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 0.8, position = position_dodge(width = 0.2))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") + # Adding a horizontal line at y=0 (no effect)
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") + # Adding a vertical line at time_to_event = 0 (policy implementation year)
  labs( # Customising labels and title
    title = "Event Study: Impact of Policy on Annual Installed Wind Capacity",
    x = "Years Relative to Policy Year (2017)",
    y = "Estimated Change in Annual Installed Capacity (MW)",
    color = "Effect Type"
  ) +
  scale_x_continuous(breaks = unique(wind_hetero_event_study_plot_df$event_time)) + # Setting x-axis breaks to show all event times clearly
  theme_minimal() +
  theme( # Customizing the plot elements
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

# Saving the ggplot as a PNG file
ggsave(filename = file.path(plots_dir,"wind_hetero_event_study_plot.png"),
       plot = wind_hetero_event_study_plot,
       device = "png",
       width = 12,
       height = 10,
       units = "in",
       dpi = 300)
