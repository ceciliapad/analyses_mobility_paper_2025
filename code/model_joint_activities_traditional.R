#####################################################
# MODELS LOOKING AT DISTANCE TRAVELLED PER ACTIVITY #
#####################################################

library(ggplot2)
library(ggstatsplot)
library(brms)
library(tidyr)

# Read all distances travelled for the 9 most common activities

# Define the directory containing your CSV files
file_directory <- "/results/reasons_visit_traditional/distances_all_activities"

# Get the list of all CSV files in the directory
file_list <- list.files(file_directory, pattern = "*.csv", full.names = TRUE)

# Initialize an empty list to store processed data frames
all_data <- list()

# Process each file
for (file in file_list) {
  # Read the CSV file
  data <- read.csv(file)
  data <- data[, -((ncol(data)-4):ncol(data))]
  
  # Extract the activity from the file name
  activity <- sub(".*max_(.*)\\.csv", "\\1", basename(file))
  
  # Extract the region from the file name
  region <- ifelse(grepl("_betou", file), "betou",
                          ifelse(grepl("_enyelle", file), "enyelle",
                                 ifelse(grepl("_minganga", file), "minganga",
                                        ifelse(grepl("_macao", file), "macao", NA))))
  
  # Apply specific transformations based on the file type
  if (grepl("_22", file) | grepl("_dongou", file) | grepl("_betou", file)| grepl("_enyelle", file)) {
    # Remove columns 1, 3, 4, 5
    data <- data[, -c(1, 3, 4, 5)]
    # Rename columns
    colnames(data)[1:5] <- c("residence_camp", "ref", "sex", "birthplace", "age")
  } else if (grepl("_minganga_23", file)) {
    # Remove columns 1, 3, 4, 7, 8, 9
    data <- data[, -c(1, 3, 4, 7, 8, 9)]
    # Rename columns
    colnames(data)[1:5] <- c("residence_camp", "ref", "sex", "birthplace", "age")
  } else if (grepl("_macao_23", file)) {
    # Remove columns 1, 6, 7, 8
    data <- data[, -c(1, 6, 7, 8)]
    # Reorder columns so that the resulting second column becomes the fifth
    data <- data[, c(1, 3, 4, 5, 2, seq(6, ncol(data)))]
    # Rename columns
    colnames(data)[1:5] <- c("residence_camp", "ref", "sex", "birthplace", "age")
  }
  
  # Determine columns for distance & rest
  distance_cols <- 6:ncol(data)
  fixed_cols <- 1:5
  
  # Reshape data: gather distance columns into long format
  data_long <- pivot_longer(
    data, 
    cols = all_of(distance_cols), 
    values_to = "distance"
  )
  
  # Remove rows where "distance" is NA or 0
  data_long <- data_long[!(is.na(data_long$distance) | data_long$distance == 0), ]
  
  # Add the "activity" column
  data_long$activity <- activity
  
  # Add region
  data_long$region <- region
  
  # Append the processed data frame to the list
  all_data <- append(all_data, list(data_long))
}

# Combine all processed data frames into one
combined_data <- do.call(rbind, all_data)

# Write the combined data to a single CSV file
write.csv(combined_data, file = "results/reasons_visit_traditional/distances_all_activities.csv", row.names = FALSE)

# read combined_data
combined_data <- read.csv("results/reasons_visit_traditional/distances_all_activities.csv")
## Prepare models

# Gamma distribution because distance is positive and right-skewed
# 1459 displacements for one of those 9 activities

formula_m1 <- bf(distance ~ age + sex + activity +
                   (1 + sex + activity | region) +  # Random slopes per region
                   (1 | region:residence_camp) + #
                   (1 | ref), 
                 family = Gamma(link = "log"))

formula_m2 <- bf(distance ~ age + sex*activity + # Interaction between sex and activity
                   (1 + sex + activity | region) +  
                   (1 | region:residence_camp) +
                   (1 | ref), 
                 family = Gamma(link = "log"))

formula_m3 <- bf(distance ~ age + sex*activity + # Interaction between sex and activity
                   (1 | region) +  # Random effects of region and residence camp
                   (1 | region:residence_camp) +
                   (1 | ref), 
                 family = Gamma(link = "log"))

formula_m4 <- bf(distance ~ age + sex + activity + 
                   (1 | region) +  # Random effects of region and residence camp
                   (1 | region:residence_camp) +
                   (1 | ref), 
                 family = Gamma(link = "log"))

m1.prior_weak <- c(
  prior(student_t(3, 2.5, 2), class = Intercept),  # Log-scale mean ~ log(11 km)
  prior(gamma(2, 0.1), class = shape),             # Shape parameter
  prior(student_t(3, 0, 2), class = sd),           # Random effect SD
  prior(normal(0, 2), class = b)                   # Fixed effect coefficients
)

# Make some plots
intercept_prior <- rt(1000, df = 3) * 2 + 2.5
shape_prior <- rgamma(1000, shape = 2, rate = 0.1)
random_effect_sd_prior <- rt(1000, df = 3) * 3
fixed_effect_prior <- rnorm(1000, mean = 0, sd = 2)
prior_data <- data.frame(
  value = c(intercept_prior, shape_prior, random_effect_sd_prior, fixed_effect_prior),
  prior = rep(c("Intercept Prior", "Shape Prior", 
                "Random Effect SD Prior", "Fixed Effect Coefficient Prior"), each = 1000)
)

# Plot prior distributions
ggplot(prior_data, aes(x = value, fill = prior)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.7, position = "identity") +
  facet_wrap(~prior, scales = "free") +
  theme_minimal() +
  labs(title = "Prior Predictive Distributions", x = "Value", y = "Density") +
  scale_fill_brewer(palette = "Set2", guide = "none")

ggplot2::ggsave("results/reasons_visit_traditional/prior_dist.png", width = 8, height = 7)

combined_data <- subset(combined_data, combined_data$sex != "FALSE")
combined_data <- subset(combined_data, combined_data$sex != "")
combined_data <- subset(combined_data, combined_data$age != "FALSE")
combined_data <- subset(combined_data, combined_data$age != "")

# Fit the models
m1 <- brm(formula_m1, data = combined_data, prior = m1.prior_weak, 
          chains = 3, iter = 5000, warmup = 1000, cores = 6, control=list(adapt_delta=0.95))

m2 <- brm(formula_m2, data = combined_data, prior = m1.prior_weak, 
          chains = 3, iter = 5000, warmup = 1000, cores = 6, control=list(adapt_delta=0.95))

# m3 did not converge properly
m3 <- brm(formula_m3, data = combined_data, prior = m1.prior_weak,
          chains = 3, iter = 5000, warmup = 1000, cores = 6, control=list(adapt_delta=0.95))

m4 <- brm(formula_m4, data = combined_data, prior = m1.prior_weak,
          chains = 3, iter = 5000, warmup = 1000, cores = 6, control=list(adapt_delta=0.95))

# Save the models
saveRDS(m1, file = "models/distances_all_activities_m1_traditional.rds")
saveRDS(m2, file = "models/distances_all_activities_m2_traditional.rds")
saveRDS(m3, file = "models/distances_all_activities_m3_traditional.rds")
saveRDS(m4, file = "models/distances_all_activities_m4_traditional.rds")

# load the models
m1 <- readRDS("models/distances_all_activities_m1_traditional.rds")
m2 <- readRDS("models/distances_all_activities_m2_traditional.rds")
m3 <- readRDS("models/distances_all_activities_m3_traditional.rds")
m4 <- readRDS("models/distances_all_activities_m4_traditional.rds")

# Compare models
loo_m1 <- loo(m1)
loo_m2 <- loo(m2) # best model
loo_m3 <- loo(m3)
loo_m4 <- loo(m4)
comparison <- loo_compare(loo_m1, loo_m2, loo_m3, loo_m4)
write.csv(comparison, file = "results/reasons_visit_traditional/loo_comparison_reasons_visit.csv", row.names = FALSE)

# Perform posterior predictive checks
pp_check(m1)
pp_check(m2)
pp_check(m3)
pp_check(m4)

####################
# Prepare plotting #
####################

# Count Observations by Group
group_counts <- combined_data %>%
  group_by(activity, age, sex, region) %>%
  summarise(n = n(), .groups = "drop")

# Filter Groups with Fewer Than 5 Observations
valid_groups <- group_counts %>%
  filter(n >=1)

region_levels <- c("macao", "minganga", "enyelle", "betou")

# Modify `newdata` to match your variables: age, sex, activity, region
newdata <- expand.grid(
  age = c("Adolescent", "Adulte", "Personne âgée"),
  sex = c("M", "F"),
  activity = unique(combined_data$activity),
  region = factor(unique(combined_data$region), levels = region_levels)  # Set the factor levels for region
)

# Filter newdata to include only valid groups
newdata <- newdata %>%
  inner_join(valid_groups, by = c("activity", "age", "sex", "region"))

posterior_predictions <- fitted(
  m1, # best model
  newdata = newdata,
  re_formula = ~ (1 + sex + activity | region),  # Exclude random effects for `ref` and `residence_camp`
  probs = c(0.025, 0.10, 0.25, 0.75, 0.9, 0.975)  # 95% and 50% credible intervals
) %>%
  as_tibble() %>%
  bind_cols(newdata) %>%
  mutate(region = factor(region, levels = region_levels))

region_colors <- MetPalettes$Isfahan2[[1]][-1]  # Base colors
region_darker_colors <- scales::muted(region_colors, l = 50)  # Darker versions

# save posterior predictions as csv
write.csv(posterior_predictions, file = "results/reasons_visit_traditional/posterior_predictions_distance_visit_random_slopes.csv", row.names = FALSE)

# Massive panel plot
# Create the plot
posterior_predictions %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = region, group = region)) +
  # Outer ribbon for 10%-90% quantiles
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = region), alpha = 0.2, color = NA) +
  # Inner ribbon for 25%-75% quantiles
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = region), alpha = 0.5, color = NA) +
  # Lines for each region
  geom_line(size = 1, alpha = 1) +
  # Facet by activity and sex
  facet_wrap(~ activity + sex, scales = "fixed", ncol = 6) +
  labs(
    x = "Age class",
    y = "Predicted distance (km)"
  ) +
  ggtitle("Regional differences in travel") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  scale_color_manual(values = region_darker_colors) +  # Darker lines
  scale_fill_manual(values = region_colors)   +        # Lighter ribbons
  scale_y_log10()  # Log-transform the y-axis

# Save the plot
ggplot2::ggsave("results/reasons_visit_traditional/predicted_distance_panel.png", width = 12, height = 10)

posterior_predictions_filtered <- posterior_predictions %>%
  filter(Estimate / 1000 >= 1, Q10 / 1000 >= 1, Q90 / 1000 >= 1)

# Replace sex values with "Women" and "Men"
posterior_predictions_filtered <- posterior_predictions_filtered %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

activity_labels <- c(
  "dance" = "Rituals",
  "fam_visit" = "Visit family",
  "fish" = "Fish",
  "friend_visit" = "Visit friends",
  "gather" = "Gather",
  "hunt" = "Hunt",
  "just_visit" = "Explore",
  "live" = "Live"
)


region_levels <- c("Macao", "Minganga", "Enyelle", "Betou")
posterior_predictions_filtered <- posterior_predictions_filtered %>%
  mutate(activity = recode(activity, !!!activity_labels),
         region = str_to_title(region) )

library(stringr)
# Create a full grid of all possible combinations to ensure consistent bar widths
full_grid <- expand.grid(
  age = unique(posterior_predictions_filtered$age),
  activity = unique(posterior_predictions_filtered$activity),
  sex = unique(posterior_predictions_filtered$sex),
  region = factor(unique(posterior_predictions_filtered$region), levels = region_levels)
)

# Merge the filtered data with the full grid, filling missing combinations with NA
posterior_predictions_complete <- full_grid %>%
  left_join(posterior_predictions_filtered, by = c("age", "activity", "sex", "region")) %>%
  mutate(region = factor(region, levels = region_levels))

# Create bar plot with consistent bar widths, equal row heights, and activity titles
posterior_predictions_complete %>%
  ggplot(aes(x = age, y = Estimate / 1000, fill = region)) +
  # Bar plot with posterior mean
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, na.rm = TRUE) +
  # Add error bars for credible intervals
  geom_errorbar(
    aes(ymin = Q10 / 1000, ymax = Q90 / 1000),
    position = position_dodge(width = 0.8),
    width = 0.25,
    na.rm = TRUE
  ) +
  # Facet by activity and sex with equal row heights
  facet_grid(activity ~ sex, scales = "fixed", space = "fixed", switch = "y") +
  labs(
    x = "Age class",
    y = "Predicted distance (km)"
  ) +
  ggtitle("Regional differences in travel") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text.x = element_text(size = 14, face = "bold"),  # Sex facet labels
    strip.text.y = element_text(size = 14, face = "bold"),  # Activity facet labels
    strip.placement = "outside",  # Place activity labels outside the plot
    panel.spacing = unit(1, "lines")  # Consistent spacing between panels
    #panel.grid.major.y = element_line(color = "grey80", linetype = "dotted")  # Add horizontal grid lines
  ) +
  scale_fill_manual(values = region_colors) 

# Save the plot
ggplot2::ggsave("results/reasons_visit_traditional/predicted_distance_barplot_equal_rows.png", width = 12, height = 15)

# save data frame
write.csv(posterior_predictions_filtered, file = "results/reasons_visit_traditional/posterior_predictions_distance_visit_filtered.csv", row.names = FALSE)

# Average across age categories
posterior_predictions_averaged <- posterior_predictions_filtered %>%
  group_by(region, sex, activity) %>%
  summarise(
    Estimate = mean(Estimate, na.rm = TRUE),
    Q10 = mean(Q10, na.rm = TRUE),
    Q90 = mean(Q90, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(region = factor(region, levels = region_levels))

# Create the bar plot
posterior_predictions_averaged %>%
  ggplot(aes(x = sex, y = Estimate / 1000, fill = region)) +
  # Bar plot with posterior mean
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, na.rm = TRUE) +
  # Add error bars for credible intervals
  geom_errorbar(
    aes(ymin = Q10 / 1000, ymax = Q90 / 1000),
    position = position_dodge(width = 0.8),
    width = 0.25,
    na.rm = TRUE
  ) +
  # Facet by activity
  facet_wrap(~ activity, scales = "fixed", ncol = 3) +
  labs(
    x = "Sex",
    y = "Predicted Distance (km)"
  ) +
  ggtitle("Regional Differences in Travel (Averaged Across Age Categories)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text.x = element_text(size = 14, face = "bold"),  # Activity facet labels
    panel.spacing = unit(1, "lines")  # Consistent spacing between panels
  ) +
  scale_fill_manual(values = region_colors)

# Save the plot
ggplot2::ggsave("results/reasons_visit_traditional/predicted_distance_barplot_averaged_age.png", width = 10, height = 8)

# save data frame
write.csv(posterior_predictions_averaged, file = "results/reasons_visit_traditional/posterior_predictions_distance_visit_averaged_age.csv", row.names = FALSE)

# Now we exclude all random effects (but we remove for those activities for which)
# one sex doesn't participate

group_counts <- combined_data %>%
  group_by(activity, age, sex) %>%
  summarise(n = n(), .groups = "drop")

# Step 2: Filter Groups with Fewer Than 5 Observations
valid_groups <- group_counts %>%
  filter(n >=2)


# Step 3: Prepare `newdata` for Predictions
newdata <- expand.grid(
  age = c("Adolescent", "Adulte", "Personne âgée"),
  sex = c("M", "F"),
  activity = unique(combined_data$activity)
)

# Filter newdata to include only valid groups
newdata <- newdata %>%
  inner_join(valid_groups, by = c("activity", "age", "sex"))


# Generate posterior predictions averaged across grouping levels
posterior_predictions <- fitted(
  m2,
  newdata = newdata,
  re_formula = NA , # Exclude all random effects
  probs = c(0.025, 0.10, 0.25, 0.75, 0.90, 0.975)
) %>%
  as_tibble() %>%
  bind_cols(newdata)

# Set up colors for sexes
dark_sex_colors <- c( MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])  # Dark colors for lines
light_sex_colors <- scales::muted(dark_sex_colors, l = 50)  # Lighter colors for ribbons

posterior_predictions_filtered <- posterior_predictions %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

activity_labels <- c(
  "dance" = "Rituals",
  "fam_visit" = "Visit family",
  "fish" = "Fish",
  "friend_visit" = "Visit friends",
  "gather" = "Gather",
  "hunt" = "Hunt",
  "just_visit" = "Explore",
  "live" = "Live"
)



posterior_predictions_filtered <- posterior_predictions_filtered %>%
  mutate(activity = recode(activity, !!!activity_labels))

# Create the panel plot
posterior_predictions_filtered %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = sex, group = sex)) +
  # Outer ribbon for 10%-90% quantiles
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), alpha = 0.2, color = NA) +
  # Inner ribbon for 25%-75% quantiles
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), alpha = 0.6, color = NA) +
  # Lines for each sex
  geom_line(size = 1) +
  # Facet by activity
  facet_wrap(~activity, scales = "fixed", ncol = 3) +
  labs(
    x = "Age class",
    y = "Predicted distance (km)",
    title = "Average across regions"
  ) +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  scale_color_manual(values = dark_sex_colors) +  # Line colors
  scale_fill_manual(values = dark_sex_colors) +   # Ribbon colors
  ylim(0, 75)  # Set y-axis limits

# Save the plot
ggplot2::ggsave("results/reasons_visit_traditional/predicted_distance_avg_regions_panel.png", width = 10, height = 8)

# save data frame
write.csv(posterior_predictions_filtered, file = "results/reasons_visit_traditional/posterior_predictions_distance_avg_regions.csv", row.names = FALSE)

# Average across age categories
posterior_predictions_averaged <- posterior_predictions %>%
  group_by(activity, sex) %>%
  summarise(
    Estimate = mean(Estimate, na.rm = TRUE),
    Q10 = mean(Q10, na.rm = TRUE),
    Q90 = mean(Q90, na.rm = TRUE),
    Q25 = mean(Q25, na.rm = TRUE),
    Q75 = mean(Q75, na.rm = TRUE),
    .groups = "drop"
  )

# Replace sex values with "Women" and "Men"
posterior_predictions_averaged <- posterior_predictions_averaged %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

# Rename activities
activity_labels <- c(
  "dance" = "Rituals",
  "fam_visit" = "Visit Family",
  "fish" = "Fish",
  "friend_visit" = "Visit Friends",
  "gather" = "Gather",
  "hunt" = "Hunt",
  "just_visit" = "Explore",
  "live" = "Live"
)

posterior_predictions_averaged <- posterior_predictions_averaged %>%
  mutate(activity = recode(activity, !!!activity_labels))

# Set up colors for sexes
dark_sex_colors <- c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])  # Dark colors for bars
dark_dot_colors <- scales::muted(dark_sex_colors, l = 50)  # Darken the colors for dots

# Create a dot and interval plot for sex differences with two intervals
posterior_predictions_averaged %>%
  ggplot(aes(x = sex, y = Estimate / 1000, color = sex)) +
  # Narrow interval (25%-75%) as a thicker linerange (simulates rectangles)
  geom_linerange(
    aes(ymin = Q25 / 1000, ymax = Q75 / 1000, color = sex),
    size = 5, alpha = 0.5, position = position_dodge(width = 0.5)
  ) +
  # Wider interval (10%-90%) as error bars
  geom_errorbar(
    aes(ymin = Q10 / 1000, ymax = Q90 / 1000),
    width = 0.2, position = position_dodge(width = 0.5)
  ) +
  # Points for posterior mean
  geom_point(
    size = 3,
    aes(color = sex),  # Use darker colors for the dots
    position = position_dodge(width = 0.5)
  ) +
  # Facet by activity
  facet_wrap(~ activity, scales = "fixed", ncol = 3) +
  labs(
    x = "Sex",
    y = "Predicted Distance (km)",
    title = "Sex Differences in Predicted Travel Distances per Activity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since colors are intuitive
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text = element_text(size = 12),  # Activity facet labels
    panel.spacing = unit(1, "lines")  # Consistent spacing between panels
  ) +
  scale_color_manual(values = dark_dot_colors ) +  # Original colors for intervals
  ylim(0, 75)  # Set y-axis limits

# Save the plot
ggplot2::ggsave("results/reasons_visit_traditional/predicted_distance_dotplot_two_intervals_corrected.png", width = 9, height = 8)

# save data frame
write.csv(posterior_predictions_averaged, file = "results/reasons_visit_traditional/posterior_predictions_distance_avg_regions_avg_age.csv", row.names = FALSE)

