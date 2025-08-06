#############################
# Validation of half ranges #
#############################

library(ggplot2)
library(ggstatsplot)
library(MetBrewer)

# read files
enyelle_22 <- read.csv("data/initial/enyelle_2022.csv")
dongou_22 <- read.csv("data/initial/dongou_2022.csv")
betou_23 <- read.csv("data/initial/betou_2023.csv")
minganga_22 <- read.csv("data/initial/minganga_2022.csv")
macao_22 <- read.csv("data/initial/macao_2022.csv")
minganga_23 <- read.csv("data/initial/minganga_2023.csv")
macao_23 <- read.csv("data/initial/macao_2023.csv")

geneal <- read.csv("data/final/all_genealogies_2023_complete.csv")[,-1]
regions <- read.csv("data/final/regions_locations_2023_complete_fixed.csv",  encoding = "UTF-8")

# Get individual information


macao_personal_22 <- subset(macao_22, select=c("Nom.du.campement", "Old_person",
                                               "Adulte",
                                               "Enfant...Notez.si.la.personne.est.un.enfant.",
                                               "MY.REFERENCE",
                                               "Sexe",
                                               "Lieu.de.naissance.coded",
                                               "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Combien.d.enfants.en.vie"))

minganga_personal_22 <- subset(minganga_22, select=c("Nom.du.campement", "Old_person",
                                                     "Adulte",
                                                     "Enfant...Notez.si.la.personne.est.un.enfant.",
                                                     "MY.REFERENCE",
                                                     "Sexe",
                                                     "Lieu.de.naissance.coded",
                                                     "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Combien.d.enfants.en.vie"))

enyelle_personal_22 <- subset(enyelle_22, select=c("Nom.du.campement", "Old_person",
                                                   "Adulte",
                                                   "Enfant...Notez.si.la.personne.est.un.enfant.",
                                                   "MY.REFERENCE",
                                                   "Sexe",
                                                   "Lieu.de.naissance.coded",
                                                   "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Combien.d.enfants.en.vie"))

betou_personal_23 <- subset(betou_23, select=c("Nom.du.campement", "Old_person",
                                                   "Adulte",
                                                   "Enfant...Notez.si.la.personne.est.un.enfant.",
                                                   "MY.REFERENCE",
                                                   "Sexe",
                                                   "Lieu.de.naissance.coded",
                                                   "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Combien.d.enfants.en.vie"))

dongou_personal_22 <- subset(dongou_22, select=c("Nomducampement", "Old_person",
                                                 "Adulte",
                                                 "Enfant",
                                                 "MY.REFERENCE",
                                                 "Sexe",
                                                 "Lieu.de.naissance.coded",
                                                 "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Combien.d.enfants.en.vie"))


macao_personal_23 <- subset(macao_23, select=c("Nom.du.campement.coded",
                                               "Tranche.d.age",
                                               "MY.REFERENCE",
                                               "Sexe",
                                               "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....1",
                                               "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Combien.d.enfants.en.vie",
                                               "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Combien.d.enfants.morts",
                                               "Lieu.de.naissance.coded"))


minganga_personal_23 <- subset(minganga_23, select=c("Nom.du.campement.coded", "Old_person",
                                                     "Enfant...Notez.si.la.personne.est.un.enfant.",
                                                     "MY.REFERENCE",
                                                     "Sexe",
                                                     "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Oui.",
                                                     "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Combien.d.enfants.en.vie",
                                                     "Est.ce.que.la.personne.à.des.enfants.....La.personne.à.des.enfants....Combien.d.enfants.morts",
                                                     "Lieu.de.naissance.coded"))


dongou_22_age <- vector(length=nrow(dongou_personal_22))
for (i in 1:nrow(dongou_personal_22)) {
  if(dongou_personal_22[i,3] == 1 && dongou_personal_22[i,2] == 1) {
    dongou_22_age[i] <- "Personne âgée"
  } else if (dongou_personal_22[i,3] == 0 && dongou_personal_22[i,4] == 0) {
    dongou_22_age[i] <- "Adolescent"
  } else if (dongou_personal_22[i,4] == 1) {
    dongou_22_age[i] <- "Enfant"
  } else if (dongou_personal_22[i,3] == 1 && dongou_personal_22[i,2] == 0) {
    dongou_22_age[i] <- "Adulte"
  }
}
dongou_personal_22$Tranche.d.age <- dongou_22_age

enyelle_22_age <- vector(length=nrow(enyelle_personal_22))
for (i in 1:nrow(enyelle_personal_22)) {
  if(enyelle_personal_22[i,2] == 1 && enyelle_personal_22[i,3] == 1) {
    enyelle_22_age[i] <- "Personne âgée"
  } else if (enyelle_personal_22[i,3] == 0 && enyelle_personal_22[i,4] == "Non") {
    enyelle_22_age[i] <- "Adolescent"
  } else if (enyelle_personal_22[i,4] == "Oui") {
    enyelle_22_age[i] <- "Enfant"
  } else if (enyelle_personal_22[i,3] == 1 && enyelle_personal_22[i,2] == 0) {
    enyelle_22_age[i] <- "Adulte"
  }
}
enyelle_personal_22$Tranche.d.age <- enyelle_22_age

macao_personal_22 <- macao_personal_22[1:168,]
macao_22_age <- vector(length=nrow(macao_personal_22))
for (i in 1:nrow(macao_personal_22)) {
  #print(i)
  if(macao_personal_22[i,2] == 1 && macao_personal_22[i,3] == 1) {
    macao_22_age[i] <- "Personne âgée"
  } else if (macao_personal_22[i,3] == 0 && macao_personal_22[i,4] == "Non") {
    macao_22_age[i] <- "Adolescent"
  } else if (macao_personal_22[i,4] == "Oui") {
    macao_22_age[i] <- "Enfant"
  } else if (macao_personal_22[i,3] == 1 && macao_personal_22[i,2] == 0) {
    macao_22_age[i] <- "Adulte"
  }
}
macao_personal_22$Tranche.d.age <- macao_22_age

minganga_personal_23$Tranche.d.age <- minganga_personal_23$Enfant...Notez.si.la.personne.est.un.enfant.

minganga_22_age <- vector(length=nrow(minganga_personal_22))

for (i in 1:nrow(minganga_personal_22)) {
  if(minganga_personal_22[i,2] == 1 && minganga_personal_22[i,3] == 1) {
    minganga_22_age[i] <- "Personne âgée"
  } else if (minganga_personal_22[i,3] == 0 && minganga_personal_22[i,4] == 0) {
    minganga_22_age[i] <- "Adolescent"
  } else if (minganga_personal_22[i,4] == 1) {
    minganga_22_age[i] <- "Enfant"
  } else if (minganga_personal_22[i,3] == 1 && minganga_personal_22[i,2] == 0) {
    minganga_22_age[i] <- "Adulte"
  }
}
minganga_personal_22$Tranche.d.age <- minganga_22_age

betou_23_age <- vector(length=nrow(betou_personal_23))

for (i in 1:nrow(betou_personal_23)) {
  print(i)
  if(betou_personal_23[i,3] == 1 && betou_personal_23[i,2] == 1) {
    betou_23_age[i] <- "Personne âgée"
  } else if (betou_personal_23[i,3] == 0 && betou_personal_23[i,4] == "Non") {
    betou_23_age[i] <- "Adolescent"
  } else if (betou_personal_23[i,4] == "Oui") {
    betou_23_age[i] <- "Enfant"
  } else if (betou_personal_23[i,3] == 1 && betou_personal_23[i,2] == 0) {
    betou_23_age[i] <- "Adulte"
  }
}
betou_personal_23$Tranche.d.age <- betou_23_age



camps <- subset(regions, regions$camp_region=="C")
point_camps <- cbind(regions$long, regions$lat)
regions <- subset(regions, regions$camp_region=="R")


# now calculate distances between all camps in a given region

# Filter only camp-region sites (C)
camp_coords <- regions %>%
  filter(camp_region == "C" & !is.na(long) & !is.na(lat)) %>%
  dplyr::select(region, macro_region, long, lat)

# Compute pairwise distances within each macro_region
camp_distances <- camp_coords %>%
  group_by(macro_region) %>%
  group_split() %>%
  lapply(function(df) {
    if (nrow(df) < 2) return(NULL)  # skip if only one camp in region
    
    # Compute pairwise haversine distances (in meters)
    dist_matrix <- distm(df[, c("long", "lat")], fun = distHaversine)
    
    # Convert matrix to long format with camp names
    distance_df <- as.data.frame(as.table(dist_matrix)) %>%
      rename(from_index = Var1, to_index = Var2, distance_m = Freq) %>%
      mutate(
        from_camp = df$region[from_index],
        to_camp = df$region[to_index],
        macro_region = df$macro_region[1]
      ) %>%
      dplyr::select(macro_region, from_camp, to_camp, distance_m) %>%
      filter(from_camp != to_camp)  # remove self-distances
  }) %>%
  bind_rows()


camp_distances <- camp_distances %>%
  mutate(distance_km = distance_m / 1000)


camp_distances <- subset(camp_distances, camp_distances$macro_region != "")

camp_distances <- camp_distances %>%
  mutate(macro_region = factor(macro_region, levels = c("Dongou", "Macao", "Minganga", "Enyelle", "Betou")))

ggplot(camp_distances, aes(x = macro_region, y = distance_km, fill = macro_region)) +
  geom_boxplot(outlier.shape = 1, color = "black") +
  scale_fill_manual(values = region_colors) +
  labs(
    x = "Region",
    y = "Distance between camps (km)",
    title = "Pairwise distances between camps"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(size = 14)
  )
ggsave("results/half_range_validation/camp_distances_by_macro_region.png", width = 8, height = 6)

kruskal.test(distance_km ~ macro_region, data = camp_distances)
wilcox <- pairwise.wilcox.test(
  camp_distances$distance_km,
  camp_distances$macro_region,
  p.adjust.method = "BH"  # Benjamini-Hochberg for multiple testing correction
)
# save as df
write.csv(camp_distances, "results/half_range_validation/camp_distances_by_macro_region.csv", row.names = FALSE)

# Compute medians per region
medians_df <- plot_data %>%
  group_by(macro_region) %>%
  summarise(median_dist = median(distance_km, na.rm = TRUE)) %>%
  mutate(x = as.numeric(factor(macro_region, levels = levels(plot_data$macro_region))))

ggplot(plot_data, aes(x = macro_region, y = distance_km, fill = macro_region)) +
  
  # Jittered points
  geom_jitter(
    width = 0.25,
    alpha = 0.3,
    size = 1.5,
    shape = 21,
    stroke = 0
  ) +
  
  # Draw bold black lines for medians
  geom_segment(
    data = medians_df,
    aes(
      x = x - 0.3,
      xend = x + 0.3,
      y = median_dist,
      yend = median_dist
    ),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1.2
  ) +
  
  # Colors
  scale_fill_manual(values = region_colors) +
  
  labs(
    x = "Region",
    y = "Distance between camps (km)",
    title = "Camp-to-camp distances per region "
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14)
  )
ggsave("results/half_range_validation/camp_distances_by_macro_region_jittered.png", width = 8, height = 6)


## Now let's plot the camp types in which individuals live in each of the regions

## camp types

interviewed_camps <- data.frame(
  campement = c(
    macao_personal_22[[1]],
    minganga_personal_22[[1]],
    enyelle_personal_22[[1]],
    betou_personal_23[[1]],
    dongou_personal_22[[1]],
    macao_personal_23[[1]],
    minganga_personal_23[[1]]
  ),
  
  sexe = c(
    macao_personal_22$Sexe,
    minganga_personal_22$Sexe,
    enyelle_personal_22$Sexe,
    betou_personal_23$Sexe,
    dongou_personal_22$Sexe,
    macao_personal_23$Sexe,
    minganga_personal_23$Sexe
  ),
  
  age = c(
    macao_personal_22[[ncol(macao_personal_22)]],
    minganga_personal_22[[ncol(minganga_personal_22)]],
    enyelle_personal_22[[ncol(enyelle_personal_22)]],
    betou_personal_23[[ncol(betou_personal_23)]],
    dongou_personal_22[[ncol(dongou_personal_22)]],
    macao_personal_23[[ncol(macao_personal_23)]],
    minganga_personal_23[[ncol(minganga_personal_23)]]
  )
)


# remove chilren
interviewed_camps <- subset(interviewed_camps, interviewed_camps$age != "Enfant")
interviewed_camps <- subset(interviewed_camps, interviewed_camps$age != "FALSE")
interviewed_camps <- subset(interviewed_camps, interviewed_camps$sexe != "")
interviewed_camps <- subset(interviewed_camps, interviewed_camps$age != "")

# Make sure camp names are character type
interviewed_camps$campement <- as.character(interviewed_camps$campement)

# Filter the `camps` data frame for matching region names
matched_camps <- subset(camps, region %in% interviewed_camps$campement, 
                        select = c("region", "macro_region", "loc"))

# Merge camp-level metadata into interviewed_camps
interviewed_camps_merged <- merge(interviewed_camps, matched_camps,
                                  by.x = "campement", by.y = "region",
                                  all.x = TRUE)

interviewed_camps_merged <- interviewed_camps_merged %>%
  mutate(loc = ifelse(loc %in% c("V", "R"), "V/R", loc))

# remove those that have N/A for loc
interviewed_camps_merged <- interviewed_camps_merged %>%
  filter(!is.na(loc) & loc != "")

summary_counts <- interviewed_camps_merged %>%
  group_by(macro_region, loc, sexe) %>%
  summarise(count = n(), .groups = "drop")


ggplot(summary_counts, aes(x = loc, y = count, fill = sexe)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ macro_region) +
  scale_fill_manual(values = sex_colors, labels = c("F" = "Women", "M" = "Men")) +
  scale_x_discrete(labels = c("F" = "Forest", "V/R" = "Village / Road")) +
  labs(
    #title = "Number of Interviewed Adults by Location Type and Sex",
    x = "Camp type",
    y = "Number of people",
    fill = "Gender"
  ) +
  theme_minimal()

# save plot
ggsave("results/half_range_validation/people_per_camp_type.png", width = 8, height = 6)



