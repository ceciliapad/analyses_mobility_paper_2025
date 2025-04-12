####################################
# Half range and max dist lifetime #
####################################


visits_dist_all <- read.csv("results/half_range/exploration_range_max_dist_everyone.csv")
visits_fit_all <- read.csv("results/half_range/exploration_range_max_dist_fitness.csv")


# MAKE PLOTS, DO CALCULATIONS
# remove kids

visits_dist_all <- subset(visits_dist_all, visits_dist_all$age != "Enfant")
visits_dist_all <- subset(visits_dist_all, visits_dist_all$age != "FALSE")
visits_dist_all <- subset(visits_dist_all, visits_dist_all$age != "")

visits_dist_all[visits_dist_all == -Inf] <- NA

visits_dist_all <- subset(visits_dist_all, visits_dist_all$sex != "")

sum_df_by_age_sex <- visits_dist_all %>% group_by(age, sex) %>%
  summarise(median_dist_mean = mean(median_dist, na.rm=T)/1000,
            median_dist_median = median(median_dist, na.rm=T)/1000,
            max_dist_mean = mean(max_dist, na.rm=T)/1000,
            max_dist_median = median(max_dist, na.rm=T)/1000) %>%
  as.data.frame()
sum_df_by_age_sex

sum_df_by_age <- visits_dist_all %>% group_by(age) %>%
  summarise(median_dist_mean = mean(median_dist, na.rm=T)/1000,
            median_dist_median = median(median_dist, na.rm=T)/1000,
            max_dist_mean = mean(max_dist, na.rm=T)/1000,
            max_dist_median = median(max_dist, na.rm=T)/1000) %>%
  as.data.frame()
sum_df_by_age

write.csv(sum_df_by_age_sex, "results/half_range/sum_df_by_age_sex_explor_range.csv")
write.csv(sum_df_by_age, "results/half_range/sum_df_by_age_explor_range.csv")

sum_df_by_age_sex_reg <- visits_dist_all %>% group_by(age, sex, region) %>%
  summarise(median_dist_mean = mean(median_dist, na.rm=T)/1000,
            median_dist_median = median(median_dist, na.rm=T)/1000,
            max_dist_mean = mean(max_dist, na.rm=T)/1000,
            max_dist_median = median(max_dist, na.rm=T)/1000) %>%
  as.data.frame()
sum_df_by_age_sex_reg
write.csv(sum_df_by_age_sex_reg, "results/half_range/sum_df_by_sex_age_reg_explor_range.csv")

visits_dist_all$median_dist_km <- visits_dist_all$median_dist/1000
p1 <- visits_dist_all %>% dplyr::select(median_dist_km, age, sex) %>%
  #pivot_longer(., cols = c(dist_living_own_bp, dist_living_spouse_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = age, y = median_dist_km, fill = sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  labs(x = "Age group", y = "Half range (km from residence)") +
  theme_minimal() +
  geom_boxplot()

# Relabel the age groups
visits_dist_all <- visits_dist_all %>%
  mutate(
    age = case_when(
      age == "Adolescent" ~ "Adolescent",
      age == "Adulte" ~ "Adult",
      age == "Personne âgée" ~ "Old person",
      TRUE ~ age
    )
  )

# Calculate 99% quantile limits for the y-axis
y_limits <- quantile(visits_dist_all$median_dist_km, c(0.01, 0.99), na.rm = TRUE)

# Define colors (same as before, but modify alpha for lighter fill)
sex_colors <- c(
  "F" = MetPalettes$Isfahan1[[1]][4], # Original darker red
  "M" = MetPalettes$Isfahan1[[1]][6]   # Original darker teal
)
sex_fill_colors <- c(
  "F" = scales::alpha(MetPalettes$Isfahan1[[1]][4], 0.3), # Lighter red
  "M" = scales::alpha(MetPalettes$Isfahan1[[1]][6], 0.3)    # Lighter teal
)

# Create the plot with boxplots and overlaid points
p_region_boxplot <- visits_dist_all %>%
  dplyr::select(median_dist_km, age, sex, region) %>%
  ggplot(aes(x = age, y = median_dist_km, color = sex, fill = sex)) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.9, # Darker points
    size = 1.5
  ) +
  # Add dark red rhombuses at the median
  stat_summary(
    fun = median,
    geom = "point",
    shape = 18, # Rhombus shape
    color = "darkred",
    size = 4,
    position = position_dodge(width = 0.8)
  ) +
  scale_color_manual(values = sex_colors) +  # Points use the original colors
  scale_fill_manual(values = sex_fill_colors) +  # Boxes use lighter versions
  labs(x = "Age group", y = "Half range (km from residence)") +
  theme_minimal() +
  coord_cartesian(ylim = y_limits, clip = "on") +
  facet_wrap(~region, scales = "free", ncol = 2) +  # Facet by region
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

p_region_boxplot
# Save the faceted boxplot
ggplot2::ggsave("results/half_range/sex_age_boxplot_with_points_by_region.png", plot = p_region_boxplot, width = 9, height = 11)

# Now only macao and minganga
visits_dist_macmin <- subset(visits_dist_all, visits_dist_all$region=="Macao" |  visits_dist_all$region=="Minganga")

# Calculate 99% quantile limits for the y-axis
y_limits <- quantile(visits_dist_macmin$median_dist_km, c(0.01, 0.99), na.rm = TRUE)

# Create the plot with boxplots and overlaid points
p_region_boxplot_macmin <- visits_dist_macmin %>%
  dplyr::select(median_dist_km, age, sex, region) %>%
  ggplot(aes(x = age, y = median_dist_km, color = sex, fill = sex)) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.9, # Darker points
    size = 1.5
  ) +
  # Add dark red rhombuses at the median
  stat_summary(
    fun = median,
    geom = "point",
    shape = 18, # Rhombus shape
    color = "darkred",
    size = 4,
    position = position_dodge(width = 0.8)
  ) +
  scale_color_manual(values = sex_colors) +  # Points use the original colors
  scale_fill_manual(values = sex_fill_colors) +  # Boxes use lighter versions
  labs(x = "Age group", y = "Half range (km from residence)") +
  theme_minimal() +
  coord_cartesian(ylim = y_limits, clip = "on") +
  facet_wrap(~region, scales = "free", ncol = 2) +  # Facet by region
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

p_region_boxplot_macmin
# Save the faceted boxplot
ggplot2::ggsave("results/half_range/sex_age_boxplot_with_points_by_region_macmin.png", plot = p_region_boxplot_macmin, width = 9, height = 6)


# Prepare an empty data frame to store the results
results <- data.frame(
  Region = character(),
  Age_Group = character(),
  W_Statistic = numeric(),
  P_Value = numeric(),
  Female_Median = numeric(),
  Male_Median = numeric(),
  Female_N = integer(),
  Male_N = integer(),
  stringsAsFactors = FALSE
)

# Iterate through each region and age group
for (region in unique(visits_dist_all$region)) {
  for (age_group in unique(visits_dist_all$age)) {
    # Filter data for the current region and age group
    subset <- visits_dist_all %>%
      filter(region == !!region & age == !!age_group)

    # Split the data by sex
    female_data <- subset %>%
      filter(sex == "F") %>%
      pull(median_dist_km)
    male_data <- subset %>%
      filter(sex == "M") %>%
      pull(median_dist_km)

    # Perform Wilcoxon test only if both groups have more than 1 observation
    if (length(female_data) > 1 & length(male_data) > 1) {
      wilcox_test <- wilcox.test(female_data, male_data, na.rm = TRUE, exact = FALSE)
      w_stat <- wilcox_test$statistic
      p_value <- wilcox_test$p.value
    } else {
      w_stat <- NA
      p_value <- NA
    }

    # Store the results
    results <- rbind(
      results,
      data.frame(
        Region = as.character(region),
        Age_Group = as.character(age_group),
        W_Statistic = ifelse(!is.null(w_stat), w_stat, NA),
        P_Value = ifelse(!is.null(p_value), p_value, NA),
        Female_Median = ifelse(length(female_data) > 0, median(female_data, na.rm = TRUE), NA),
        Male_Median = ifelse(length(male_data) > 0, median(male_data, na.rm = TRUE), NA),
        Female_N = length(female_data),
        Male_N = length(male_data)
      )
    )
  }
}

print(results)
# Optionally save the results to a CSV file
write.csv(results, "results/half_range/wilcoxon_results_sex_differences.csv", row.names = FALSE)

# NOW MAX DISTANCE

visits_dist_all$max_dist_km <- visits_dist_all$max_dist/1000

# Calculate 99% quantile limits for the y-axis
y_limits <- quantile(visits_dist_all$max_dist_km, c(0.01, 0.99), na.rm = TRUE)


# Create the plot with boxplots and overlaid points
p_region_boxplot <- visits_dist_all %>%
  dplyr::select(max_dist_km, age, sex, region) %>%
  ggplot(aes(x = age, y = max_dist_km, color = sex, fill = sex)) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.9, # Darker points
    size = 1.5
  ) +
  # Add dark red rhombuses at the median
  stat_summary(
    fun = median,
    geom = "point",
    shape = 18, # Rhombus shape
    color = "darkred",
    size = 4,
    position = position_dodge(width = 0.8)
  ) +
  scale_color_manual(values = sex_colors) +  # Points use the original colors
  scale_fill_manual(values = sex_fill_colors) +  # Boxes use lighter versions
  labs(x = "Age group", y = "Max distance in lifetime (km from residence)") +
  theme_minimal() +
  coord_cartesian(ylim = y_limits, clip = "on") +
  facet_wrap(~region, scales = "free", ncol = 2) +  # Facet by region
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

p_region_boxplot
# Save the faceted boxplot
ggplot2::ggsave("results/half_range/sex_age_boxplot_with_points_by_region_max.png", plot = p_region_boxplot, width = 9, height = 11)

# Now only macao and minganga
visits_dist_macmin <- subset(visits_dist_all, visits_dist_all$region=="Macao" |  visits_dist_all$region=="Minganga")

# Calculate 99% quantile limits for the y-axis
y_limits <- quantile(visits_dist_macmin$max_dist_km, c(0.01, 0.99), na.rm = TRUE)

# Create the plot with boxplots and overlaid points
p_region_boxplot_macmin <- visits_dist_macmin %>%
  dplyr::select(max_dist_km, age, sex, region) %>%
  ggplot(aes(x = age, y = max_dist_km, color = sex, fill = sex)) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.9, # Darker points
    size = 1.5
  ) +
  # Add dark red rhombuses at the median
  stat_summary(
    fun = median,
    geom = "point",
    shape = 18, # Rhombus shape
    color = "darkred",
    size = 4,
    position = position_dodge(width = 0.8)
  ) +
  scale_color_manual(values = sex_colors) +  # Points use the original colors
  scale_fill_manual(values = sex_fill_colors) +  # Boxes use lighter versions
  labs(x = "Age group", y = "Max distance in lifetime(km from residence)") +
  theme_minimal() +
  coord_cartesian(ylim = y_limits, clip = "on") +
  facet_wrap(~region, scales = "free", ncol = 2) +  # Facet by region
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

p_region_boxplot_macmin
# Save the faceted boxplot
ggplot2::ggsave("results/half_range/sex_age_boxplot_with_points_by_region_macmin_max.png", plot = p_region_boxplot_macmin, width = 9, height = 6)


# Prepare an empty data frame to store the results
results <- data.frame(
  Region = character(),
  Age_Group = character(),
  W_Statistic = numeric(),
  P_Value = numeric(),
  Female_Median = numeric(),
  Male_Median = numeric(),
  Female_N = integer(),
  Male_N = integer(),
  stringsAsFactors = FALSE
)

# Iterate through each region and age group
for (region in unique(visits_dist_all$region)) {
  for (age_group in unique(visits_dist_all$age)) {
    # Filter data for the current region and age group
    subset <- visits_dist_all %>%
      filter(region == !!region & age == !!age_group)

    # Split the data by sex
    female_data <- subset %>%
      filter(sex == "F") %>%
      pull(max_dist_km)
    male_data <- subset %>%
      filter(sex == "M") %>%
      pull(max_dist_km)

    # Perform Wilcoxon test only if both groups have more than 1 observation
    if (length(female_data) > 1 & length(male_data) > 1) {
      wilcox_test <- wilcox.test(female_data, male_data, na.rm = TRUE, exact = FALSE)
      w_stat <- wilcox_test$statistic
      p_value <- wilcox_test$p.value
    } else {
      w_stat <- NA
      p_value <- NA
    }

    # Store the results
    results <- rbind(
      results,
      data.frame(
        Region = as.character(region),
        Age_Group = as.character(age_group),
        W_Statistic = ifelse(!is.null(w_stat), w_stat, NA),
        P_Value = ifelse(!is.null(p_value), p_value, NA),
        Female_Median = ifelse(length(female_data) > 0, median(female_data, na.rm = TRUE), NA),
        Male_Median = ifelse(length(male_data) > 0, median(male_data, na.rm = TRUE), NA),
        Female_N = length(female_data),
        Male_N = length(male_data)
      )
    )
  }
}

print(results)
# Optionally save the results to a CSV file
write.csv(results, "results/half_range/wilcoxon_results_sex_differences_max_dist.csv", row.names = FALSE)

visits_dist_all %>%
  #select(c('Age','Sexe.x', 'max')) %>%
  #tidyr::pivot_longer(cols = -Age)
  #separate(Age, into = c("Age", "Sexe.x"), sep = " \\(") %>%
  #mutate(Cluster = sub("\\)", "", Cluster)) %>%
  group_by(age) %>%
  ggstatsplot::grouped_ggbetweenstats(x = sex, y = "median_dist_km",
                                      plot.type = "box",
                                      grouping.var = age,
                                      type="p",
                                      violin.args = list(width = 0),
                                      package="wesanderson",
                                      palette="Darjeeling1",
                                      pairwise.display = "significant",
                                      ylab="Half range (km from residence)",
                                      xlab="Sex") +
  ggplot2::scale_y_continuous(limits = c(0, 120)) +
  ggplot2::scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]))
ggplot2::ggsave("results/half_range/sex_age_median_from_res_stats.png", width = 17, height=8)

visits_dist_all %>%
  #select(c('Age','Sexe.x', 'max')) %>%
  #tidyr::pivot_longer(cols = -Age)
  #separate(Age, into = c("Age", "Sexe.x"), sep = " \\(") %>%
  #mutate(Cluster = sub("\\)", "", Cluster)) %>%
  group_by(region) %>%
  ggstatsplot::grouped_ggbetweenstats(x = sex, y = "median_dist_km",
                                      plot.type = "box",
                                      grouping.var = region,
                                      type="p",
                                      violin.args = list(width = 0),
                                      package="wesanderson",
                                      palette="Darjeeling1",
                                      pairwise.display = "significant",
                                      ylab="Half range (km from residence)",
                                      xlab="Sex",
                                      ggplot.component = ggplot2::scale_y_continuous(limits = (c(1, 200))
                                      ))
  ggplot.component = ggplot2::scale_y_continuous(limits = (c(1, 200)))
  ggplot2::scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]))
ggplot2::ggsave("results/half_range/sex_region_median_from_res_stats.png", width = 12, height=10)

visits_dist_all %>%
  #select(c('Age','Sexe.x', 'max')) %>%
  #tidyr::pivot_longer(cols = -Age)
  #separate(Age, into = c("Age", "Sexe.x"), sep = " \\(") %>%
  #mutate(Cluster = sub("\\)", "", Cluster)) %>%
  group_by(sex) %>%
  ggstatsplot::grouped_ggbetweenstats(x = age, y = "median_dist_km",
                                      plot.type = "box",
                                      grouping.var = sex,
                                      type="p",
                                      violin.args = list(width = 0),
                                      package="wesanderson",
                                      palette="Darjeeling1",
                                      pairwise.display = "significant",
                                      ylab="Half range (km from residence)",
                                      xlab="Age group") +
  ggplot2::scale_y_continuous(limits = c(0, 120))
  #ggplot2::scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]))
ggplot2::ggsave("results/half_range/age_sex_median_from_res_stats.png", width = 11, height=7)

# now max distance

visits_dist_all$max_dist_km <- visits_dist_all$max_dist/1000
p1 <- visits_dist_all %>% dplyr::select(max_dist_km, age, sex) %>%
  #pivot_longer(., cols = c(dist_living_own_bp, dist_living_spouse_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = age, y = max_dist_km, fill = sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  labs(x = "Age group", y = "Max distance in lifetime (km from residence)") +
  theme_minimal() +
  geom_boxplot()

# 99% of observations plotted
pi <- p1 + coord_cartesian(ylim = quantile(visits_dist_all$max_dist_km, c(0.01, 0.99), na.rm=T), clip="on")
pi
ggplot2::ggsave("results/half_range/sex_age_max_from_res.png", width = 7, height=6)

visits_dist_all %>%
  #select(c('Age','Sexe.x', 'max')) %>%
  #tidyr::pivot_longer(cols = -Age)
  #separate(Age, into = c("Age", "Sexe.x"), sep = " \\(") %>%
  #mutate(Cluster = sub("\\)", "", Cluster)) %>%
  group_by(age) %>%
  ggstatsplot::grouped_ggbetweenstats(x = sex, y = "max_dist_km",
                                      plot.type = "box",
                                      grouping.var = age,
                                      type="p",
                                      violin.args = list(width = 0),
                                      package="wesanderson",
                                      palette="Darjeeling1",
                                      pairwise.display = "significant",
                                     # ylim= c(0,1000),
                                      ylab="Max distance in lifetime (km from residence)",
                                      xlab="Sex",
                                      ggplot.component = ggplot2::scale_y_continuous(limits = (c(1, 900))))
  #ggplot2::scale_y_continuous(limits = c(0, 800)) +
  #ggplot2::scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]))
ggplot2::ggsave("results/half_range/sex_age_max_from_res_stats.png", width = 17, height=8)

visits_dist_all %>%
  #select(c('Age','Sexe.x', 'max')) %>%
  #tidyr::pivot_longer(cols = -Age)
  #separate(Age, into = c("Age", "Sexe.x"), sep = " \\(") %>%
  #mutate(Cluster = sub("\\)", "", Cluster)) %>%
  group_by(sex) %>%
  ggstatsplot::grouped_ggbetweenstats(x = age, y = "max_dist_km",
                                      plot.type = "box",
                                      grouping.var = sex,
                                      type="p",
                                      violin.args = list(width = 0),
                                      package="wesanderson",
                                      palette="Darjeeling1",
                                      pairwise.display = "significant",
                                      ylab="Max distance in lifetime (km from residence)",
                                      xlab="Age group",
                                      ggplot.component = ggplot2::scale_y_continuous(limits = (c(1, 850))))
  #ggplot2::scale_y_continuous(limits = c(0, 850))
#ggplot2::scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]))
ggplot2::ggsave("results/half_range/age_sex_max_from_res_stats.png", width = 11, height=7)

# Histogram of max dist in lifetime
png("results/half_range/max_dist_MEAN.png",7,7, units="in", res=600)
mean_val <- (mean(visits_dist_all$max_dist, na.rm=T))/1000
hist((visits_dist_all$max_dist)/1000, breaks=25, main="Maximum distance in lifetime",
     xlab="Max. distance in lifetime (km)", ylim=c(0,250), xlim=c(0,1200), col=MetPalettes$Isfahan2[[1]][2])
abline(v = mean_val, col = MetPalettes$Isfahan1[[1]][6], lty = 2, lw = 2.5)
text(x = mean_val + 50, y = 180, labels = paste("Median =", round(mean_val, 2)), pos = 4, col = MetPalettes$Isfahan1[[1]][6])
dev.off()

sdv <- (sd(visits_dist_all$max_dist, na.rm=T))/1000
mean_val
# now from bp

dongou_visited[dongou_visited == "Boucy-boucy"] <- "Boucyboucy"


