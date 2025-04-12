#######################################
# Models exploring reasons for travel #
#######################################

# load packages
library(measurements)
library(tidyverse)
library(ggplot2)
library(ggstatsplot)
library(brms)

# read files
enyelle_22 <- read.csv("data/initial/enyelle_2022.csv")
dongou_22 <- read.csv("data/initial/dongou_2022.csv")
minganga_22 <- read.csv("data/initial/minganga_2022.csv")
macao_22 <- read.csv("data/initial/macao_2022.csv")
minganga_23 <- read.csv("data/initial/minganga_2023.csv")
macao_23 <- read.csv("data/initial/macao_2023.csv")
betou_23 <- read.csv("data/initial/betou_2023.csv")


geneal <- read.csv("data/final/all_genealogies_2023_complete.csv")[,-1]
regions <- read.csv("data/final/regions_locations_2023_complete_fixed.csv",  encoding = "UTF-8")


macao_personal_22 <- subset(macao_22, select=c("Nom.du.campement", "Old_person",
                                               "Adulte",
                                               "Enfant...Notez.si.la.personne.est.un.enfant.",
                                               "MY.REFERENCE",
                                               "Sexe",
                                               "Lieu.de.naissance.coded"))

minganga_personal_22 <- subset(minganga_22, select=c("Nom.du.campement", "Old_person",
                                                     "Adulte",
                                                     "Enfant...Notez.si.la.personne.est.un.enfant.",
                                                     "MY.REFERENCE",
                                                     "Sexe",
                                                     "Lieu.de.naissance.coded"))

betou_personal_22 <- subset(betou_23, select=c("Nom.du.campement", "Old_person",
                                               "Adulte",
                                               "Enfant...Notez.si.la.personne.est.un.enfant.",
                                               "MY.REFERENCE",
                                               "Sexe",
                                               "Lieu.de.naissance.coded"))

enyelle_personal_22 <- subset(enyelle_22, select=c("Nom.du.campement", "Old_person",
                                                   "Adulte",
                                                   "Enfant...Notez.si.la.personne.est.un.enfant.",
                                                   "MY.REFERENCE",
                                                   "Sexe",
                                                   "Lieu.de.naissance.coded"))

dongou_personal_22 <- subset(dongou_22, select=c("Nomducampement", "Old_person",
                                                 "Adulte",
                                                 "Enfant",
                                                 "MY.REFERENCE",
                                                 "Sexe",
                                                 "Lieu.de.naissance.coded"))


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

betou_22_age <- vector(length=nrow(betou_personal_22))
for (i in 1:nrow(betou_personal_22)) {
  if(betou_personal_22[i,2] == 1 && betou_personal_22[i,3] == 1) {
    betou_22_age[i] <- "Personne âgée"
  } else if (betou_personal_22[i,3] == 0 && betou_personal_22[i,4] == "Non") {
    betou_22_age[i] <- "Adolescent"
  } else if (betou_personal_22[i,4] == "Oui") {
    betou_22_age[i] <- "Enfant"
  } else if (betou_personal_22[i,3] == 1 && betou_personal_22[i,2] == 0) {
    betou_22_age[i] <- "Adulte"
  }
}
betou_personal_22$Tranche.d.age <- betou_22_age

# Select visited places and reasons for those visits

dongou_visited <- dplyr::select(dongou_22,contains("Visité."))
#dongou_visited %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(dongou_visited ) = gsub(pattern = "Regionsvisitésaumoinsunefois..", replacement = "", x = names(dongou_visited ))
names(dongou_visited ) = gsub(pattern = "Campementsvisitésaumoinsunefois..", replacement = "", x = names(dongou_visited ))
names(dongou_visited ) = gsub(pattern = "..Visité.", replacement = "", x = names(dongou_visited ))
names(dongou_visited ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(dongou_visited ))
# remove first column
dongou_visited <- dongou_visited[,-1]

dongou_reasons <- dplyr::select(dongou_22,contains("Raison"))
#dongou_reasons %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(dongou_reasons ) = gsub(pattern = "Regionsvisitésaumoinsunefois..", replacement = "", x = names(dongou_reasons ))
names(dongou_reasons ) = gsub(pattern = "Campementsvisitésaumoinsunefois..", replacement = "", x = names(dongou_reasons ))
names(dongou_reasons ) = gsub(pattern = "..Raisondelavisite", replacement = "", x = names(dongou_reasons ))
names(dongou_reasons ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(dongou_reasons ))
# remove first column
dongou_reasons <- dongou_reasons[,-1]
dongou_visited <- dongou_visited[,-1]

write.csv(dongou_reasons, "data/initial/dongou_reasons_clean.csv")
write.csv(dongou_visited, "data/initial/dongou_visits_clean.csv")

# Ensure dongou_visited and dongou_reasons have the same dimensions
if (!all(dim(dongou_visited) == dim(dongou_reasons))) {
  stop("The dimensions of dongou_visited and dongou_reasons do not match.")
}

# Replace "Visité?" in dongou_visited with NA where dongou_reasons is NA or empty
dongou_visited <- as.data.frame(
  mapply(function(visit, reason) {
    ifelse(reason == "" | is.na(reason), NA, visit)
  }, dongou_visited, dongou_reasons, SIMPLIFY = FALSE)
)

# Ensure column names are preserved
colnames(dongou_visited) <- colnames(dongou_reasons)

# Write the cleaned dongou_visited data frame to a CSV file
write.csv(dongou_visited, "data/initial/dongou_visits_cleaned_only_reasoned.csv", row.names = FALSE)


macao_visited_22 <- dplyr::select(macao_22,contains("Visité."))
#macao_visited %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(macao_visited_22) = gsub(pattern = "Regions.visités..au.moins.une.fois.....", replacement = "", x = names(macao_visited_22))
names(macao_visited_22) = gsub(pattern = "Campements.visités..au.moins.une.fois.....", replacement = "", x = names(macao_visited_22))
names(macao_visited_22) = gsub(pattern = "..Visité.", replacement = "", x = names(macao_visited_22))
names(macao_visited_22) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(macao_visited_22))
names(macao_visited_22) = sub(pattern = "\\.", replacement = "", x=names(macao_visited_22))
names(macao_visited_22) = sub(pattern = "\\.", replacement = "", x=names(macao_visited_22))
names(macao_visited_22) = sub(pattern = "\\.", replacement = "", x=names(macao_visited_22))
macao_visited_22 <- macao_visited_22[,-c(1,21,68,69)]
for (i in 1:nrow(macao_visited_22)) {
  furthest_place <- macao_visited_22[i,65]
  if (length(furthest_place > 0)) {
    col_place <- which(colnames(macao_visited_22) == furthest_place)
    if (length(col_place > 0)) {
      macao_visited_22[i,col_place] <- "Visité?"
    } else {
      macao_visited_22[i,ncol(macao_visited_22)+1] <- "Visité?"
      colnames(macao_visited_22)[ncol(macao_visited_22)] <- furthest_place
    }
  }
}
macao_visited_22$Lieuplusloinvisité.coded <- NULL


macao_reasons_22 <- dplyr::select(macao_22,contains("Raison"))
#macao_reasons %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(macao_reasons_22) = gsub(pattern = "Regions.visités..au.moins.une.fois.....", replacement = "", x = names(macao_reasons_22))
names(macao_reasons_22) = gsub(pattern = "Campements.visités..au.moins.une.fois.....", replacement = "", x = names(macao_reasons_22))
names(macao_reasons_22) = gsub(pattern = "....Raison.de.la.visite", replacement = "", x = names(macao_reasons_22))
names(macao_reasons_22) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(macao_reasons_22))
names(macao_reasons_22) = sub(pattern = "\\.", replacement = "", x=names(macao_reasons_22))
macao_reasons_22 <- macao_reasons_22[,-c(1,21,68,69)]
macao_reasons_22 <- macao_reasons_22[,-c(66:98)]
macao_reasons_22 <- cbind(macao_reasons_22, macao_visited_22[,c(65:70)] )
macao_reasons_22$Lieuplusloin.visité.coded <- NULL
for (j in 66:71) {
  for (row_index in 1:nrow(macao_reasons_22)) {
    if (macao_reasons_22[row_index, j] == "Visité?" && !is.na(macao_reasons_22[row_index, j])) {
      macao_reasons_22[row_index, j] <- as.character(macao_reasons_22[row_index, 65])
    }
  }
}

macao_reasons_22$Raisonpour.plus.loin.lieu.coded <- NULL
write.csv(macao_reasons_22, "data/initial/macao_reasons_22_clean.csv")
write.csv(macao_visited_22, "data/initial/macao_visits_22_clean.csv")

# Ensure macao_visited and macao_reasons have the same dimensions
if (!all(dim(macao_visited_22) == dim(macao_reasons_22))) {
  stop("The dimensions of macao_visited and macao_reasons do not match.")
}

# Replace "Visité?" in macao_visited with NA where macao_reasons is NA or empty
macao_visited_22 <- as.data.frame(
  mapply(function(visit, reason) {
    ifelse(reason == "" | is.na(reason), NA, visit)
  }, macao_visited_22, macao_reasons_22, SIMPLIFY = FALSE)
)

# Ensure column names are preserved
colnames(macao_visited_22) <- colnames(macao_reasons_22)

# Write the cleaned macao_visited data frame to a CSV file
write.csv(macao_visited_22, "data/initial/macao_visits_22_cleaned_only_reasoned.csv", row.names = FALSE)


macao_visited_23 <- dplyr::select(macao_23,contains("Visité.") )
#macao_visited %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(macao_visited_23) = gsub(pattern = "Regions.visités..au.moins.une.fois.....", replacement = "", x = names(macao_visited_23))
names(macao_visited_23) = gsub(pattern = "Campements.visités..au.moins.une.fois.....", replacement = "", x = names(macao_visited_23))
names(macao_visited_23) = gsub(pattern = "..Visité.", replacement = "", x = names(macao_visited_23))
names(macao_visited_23) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(macao_visited_23))
names(macao_visited_23) = sub(pattern = "\\.", replacement = "", x=names(macao_visited_23))
names(macao_visited_23) = sub(pattern = "\\.", replacement = "", x=names(macao_visited_23))
names(macao_visited_23) = gsub(pattern = "Estceque.la.personne.a.visité.un.autre.Pays..sélectionner.toutes.les.options.qui.s.appliquent.....", replacement = "", x = names(macao_visited_23))
macao_visited_23 <- macao_visited_23[,-c(56,60,61,62,63,65,66,67,68,69,71:73,75:77,79:81,83,84)]
for (i in 1:nrow(macao_visited_23)) {
  furthest_place <- macao_visited_23[i,59]
  print(furthest_place)
  if (length(furthest_place > 0)) {
    col_place <- which(colnames(macao_visited_23) == furthest_place)
    if (length(col_place > 0)) {
      macao_visited_23[i,col_place] <- "Visité?"
    } else {
      macao_visited_23[i,ncol(macao_visited_23)+1] <- "Visité?"
      colnames(macao_visited_23)[ncol(macao_visited_23)] <- furthest_place
    }
  }
}
macao_visited_23$Lieuplusloin.visité.coded <- NULL
for (i in 1:nrow(macao_visited_23)) {
  for (j in seq(from=59, to=62, by=1)) {
    place <- macao_visited_23[i,j]
    #print(place)
    if (length(place > 0) && !is.na(place) && place !="") {
      print(place)
      macao_visited_23[i,ncol(macao_visited_23)+1] <- "Visité?"
      colnames(macao_visited_23)[ncol(macao_visited_23)] <- place
    }
  }
}
macao_visited_23 <- macao_visited_23[,-c(59:62)]


macao_reasons_23 <- dplyr::select(macao_23,contains("Raison") )
#macao_reasons %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(macao_reasons_23) = gsub(pattern = "Regions.visités..au.moins.une.fois.....", replacement = "", x = names(macao_reasons_23))
names(macao_reasons_23) = gsub(pattern = "Campements.visités..au.moins.une.fois.....", replacement = "", x = names(macao_reasons_23))
names(macao_reasons_23) = gsub(pattern = "....Raison.de.la.visite", replacement = "", x = names(macao_reasons_23))
names(macao_reasons_23) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(macao_reasons_23))
names(macao_reasons_23) = sub(pattern = "\\.", replacement = "", x=names(macao_reasons_23))
names(macao_reasons_23) = sub(pattern = "\\.", replacement = "", x=names(macao_reasons_23))
names(macao_reasons_23) = gsub(pattern = "Estceque.la.personne.a.visité.un.autre.Pays..sélectionner.toutes.les.options.qui.s.appliquent.....", replacement = "", x = names(macao_reasons_23))
macao_reasons_23 <- macao_reasons_23[,-c(1:84)]

macao_reasons_23 <- macao_reasons_23[,-c(56,60:63)]
macao_visited_23 <- macao_visited_23[,-c(59:72)]

write.csv(macao_reasons_23, "data/initial/macao_reasons_23_clean.csv")
write.csv(macao_visited_23, "data/initial/macao_visits_23_clean.csv")

# Ensure macao_visited and macao_reasons have the same dimensions
if (!all(dim(macao_visited_23) == dim(macao_reasons_23))) {
  stop("The dimensions of macao_visited and macao_reasons do not match.")
}

# Replace "Visité?" in macao_visited with NA where macao_reasons is NA or empty
macao_visited_23 <- as.data.frame(
  mapply(function(visit, reason) {
    ifelse(reason == "" | is.na(reason), NA, visit)
  }, macao_visited_23, macao_reasons_23, SIMPLIFY = FALSE)
)

# Ensure column names are preserved
colnames(macao_visited_23) <- colnames(macao_reasons_23)

# Write the cleaned macao_visited data frame to a CSV file
write.csv(macao_visited_23, "data/initial/macao_visits_23_cleaned_only_reasoned.csv", row.names = FALSE)


minganga_visited_22 <- dplyr::select(minganga_22,contains("Visité."))
#minganga_visited %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(minganga_visited_22 ) = gsub(pattern = "Regions.visités..au.moins.1.....", replacement = "", x = names(minganga_visited_22 ))
names(minganga_visited_22 ) = gsub(pattern = "Campements.visités..au.moins.1.....", replacement = "", x = names(minganga_visited_22 ))
names(minganga_visited_22 ) = gsub(pattern = "....Visité.", replacement = "", x = names(minganga_visited_22 ))
names(minganga_visited_22 ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(minganga_visited_22 ))
names(minganga_visited_22 ) = sub(pattern = "\\.", replacement = "", x=names(minganga_visited_22 ))
names(minganga_visited_22 ) = gsub(pattern = "Estce.que.la.personne.a.visité.un.autre.Pays..sélectionner.toutes.les.options.qui.s.appliquent.....", replacement = "", x = names(minganga_visited_22 ))
#names(minganga_visited ) = gsub(pattern = ".", replacement = "", x = names(minganga_visited ))
minganga_visited_22 <- minganga_visited_22[,-c(1,17,70,72:77,78,80,81,83,84,86)]
for (i in 1:nrow(minganga_visited_22)) {
  furthest_place <- minganga_visited_22[i,68]
  if (length(furthest_place > 0)) {
    col_place <- which(colnames(minganga_visited_22) == furthest_place)
    if (length(col_place > 0)) {
      minganga_visited_22[i,col_place] <- "Visité?"
    } else {
      minganga_visited_22[i,ncol(minganga_visited_22)+1] <- "Visité?"
      colnames(minganga_visited_22)[ncol(minganga_visited_22)] <- furthest_place
    }
  }
}
for (i in 1:nrow(minganga_visited_22)) {
  for (j in seq(from=69, to=71, by=1)) {
    place <- minganga_visited_22[i,j]
    #print(place)
    if (length(place > 0) && !is.na(place) && place !="") {
      print(place)
      minganga_visited_22[i,ncol(minganga_visited_22)+1] <- "Visité?"
      colnames(minganga_visited_22)[ncol(minganga_visited_22)] <- place
    }
  }
}
minganga_visited_22 <- minganga_visited_22[,-c(69:71)]
minganga_visited_22$Lieuplus.loin.visité.coded <- NULL


minganga_reasons_22 <- dplyr::select(minganga_22,contains("Raison"))
#minganga_reasons %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(minganga_reasons_22 ) = gsub(pattern = "Regions.visités..au.moins.1.....", replacement = "", x = names(minganga_reasons_22 ))
names(minganga_reasons_22 ) = gsub(pattern = "Campements.visités..au.moins.1.....", replacement = "", x = names(minganga_reasons_22 ))
names(minganga_reasons_22 ) = gsub(pattern = "....Raison.de.la.visite", replacement = "", x = names(minganga_reasons_22 ))
names(minganga_reasons_22 ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(minganga_reasons_22 ))
names(minganga_reasons_22 ) = sub(pattern = "\\.", replacement = "", x=names(minganga_reasons_22 ))
names(minganga_reasons_22 ) = gsub(pattern = "Estce.que.la.personne.a.visité.un.autre.Pays..sélectionner.toutes.les.options.qui.s.appliquent.....", replacement = "", x = names(minganga_reasons_22 ))
#names(minganga_reasons ) = gsub(pattern = ".", replacement = "", x = names(minganga_reasons ))
minganga_reasons_22 <- minganga_reasons_22[,-c(1,17,70,72:77,78,80,81,83,84,86)]
minganga_reasons_22 <- minganga_reasons_22[,-c(69:121)]

minganga_reasons_22 <- cbind(minganga_reasons_22, minganga_visited_22[,c(68:71)] )

for (j in 69:72) {
  for (row_index in 1:nrow(minganga_reasons_22)) {
    if (minganga_reasons_22[row_index, j] == "Visité?" && !is.na(minganga_reasons_22[row_index, j])) {
      minganga_reasons_22[row_index, j] <- as.character(minganga_reasons_22[row_index, 68])
    }
  }
}

minganga_reasons_22$Raisonpour.plus.loin.lieu.coded <- NULL

write.csv(minganga_reasons_22, "data/initial/minganga_reasons_22_clean.csv")
write.csv(minganga_visited_22, "data/initial/minganga_visits_22_clean.csv")

# Ensure minganga_visited and minganga_reasons have the same dimensions
if (!all(dim(minganga_visited_22) == dim(minganga_reasons_22))) {
  stop("The dimensions of minganga_visited and minganga_reasons do not match.")
}

# Replace "Visité?" in minganga_visited with NA where minganga_reasons is NA or empty
minganga_visited_22 <- as.data.frame(
  mapply(function(visit, reason) {
    ifelse(reason == "" | is.na(reason), NA, visit)
  }, minganga_visited_22, minganga_reasons_22, SIMPLIFY = FALSE)
)

# Ensure column names are preserved
colnames(minganga_visited_22) <- colnames(minganga_reasons_22)

# Write the cleaned minganga_visited data frame to a CSV file
write.csv(minganga_visited_22, "data/initial/minganga_visits_22_cleaned_only_reasoned.csv", row.names = FALSE)


minganga_visited_23 <- dplyr::select(minganga_23,contains("Visité."))
#minganga_visited %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(minganga_visited_23 ) = gsub(pattern = "Regionsvisités..au.moins.une.fois.....", replacement = "", x = names(minganga_visited_23 ))
names(minganga_visited_23 ) = gsub(pattern = "Campements.visités..au.moins.une.fois.....", replacement = "", x = names(minganga_visited_23 ))
names(minganga_visited_23 ) = gsub(pattern = "....Visité.", replacement = "", x = names(minganga_visited_23 ))
names(minganga_visited_23 ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(minganga_visited_23 ))
names(minganga_visited_23 ) = sub(pattern = "\\.", replacement = "", x=names(minganga_visited_23 ))
names(minganga_visited_23 ) = gsub(pattern = "Estce.que.la.personne.a.visité.un.autre.Pays..sélectionner.toutes.les.options.qui.s.appliquent.....", replacement = "", x = names(minganga_visited_23 ))
#names(minganga_visited ) = gsub(pattern = ".", replacement = "", x = names(minganga_visited ))
names(minganga_visited_23 ) = gsub(pattern = "Regionsvisités..au.moins.une.fois.....", replacement = "", x = names(minganga_visited_23 ))

minganga_visited_23 <- minganga_visited_23[,-c(1,57,62,63:68,70:72,74:76,78:80,82,83)]
for (i in 1:nrow(minganga_visited_23)) {
  furthest_place <- minganga_visited_23[i,59]
  if (length(furthest_place > 0) && !is.na(furthest_place)) {
    col_place <- which(colnames(minganga_visited_23) == furthest_place)
    if (length(col_place > 0)) {
      minganga_visited_23[i,col_place] <- "Visité?"
    } else {
      minganga_visited_23[i,ncol(minganga_visited_23)+1] <- "Visité?"
      colnames(minganga_visited_23)[ncol(minganga_visited_23)] <- furthest_place
    }
  }
}
for (i in 1:nrow(minganga_visited_23)) {
  for (j in seq(from=60, to=63, by=1)) {
    place <- minganga_visited_23[i,j]
    #print(place)
    if (length(place > 0) && !is.na(place) && place !="") {
      print(place)
      minganga_visited_23[i,ncol(minganga_visited_23)+1] <- "Visité?"
      colnames(minganga_visited_23)[ncol(minganga_visited_23)] <- place
    }
  }
}
minganga_visited_23 <- minganga_visited_23[,-c(60:63)]
minganga_visited_23$Lieuplus.loin.visité.coded <- NULL

minganga_reasons_23 <- dplyr::select(minganga_23,contains("Raison"))
#minganga_reasons %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(minganga_reasons_23 ) = gsub(pattern = "Regionsvisités..au.moins.une.fois.....", replacement = "", x = names(minganga_reasons_23 ))
names(minganga_reasons_23 ) = gsub(pattern = "Campements.visités..au.moins.une.fois.....", replacement = "", x = names(minganga_reasons_23 ))
names(minganga_reasons_23 ) = gsub(pattern = "....Raison.de.la.visite", replacement = "", x = names(minganga_reasons_23 ))
names(minganga_reasons_23 ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(minganga_reasons_23 ))
names(minganga_reasons_23 ) = sub(pattern = "\\.", replacement = "", x=names(minganga_reasons_23 ))
names(minganga_reasons_23 ) = gsub(pattern = "Estce.que.la.personne.a.visité.un.autre.Pays..sélectionner.toutes.les.options.qui.s.appliquent.....", replacement = "", x = names(minganga_reasons_23 ))
#names(minganga_reasons ) = gsub(pattern = ".", replacement = "", x = names(minganga_reasons ))
names(minganga_reasons_23 ) = gsub(pattern = "Regionsvisités..au.moins.une.fois.....", replacement = "", x = names(minganga_reasons_23 ))
minganga_reasons_23 <- minganga_reasons_23[,-c(1:55)]
minganga_reasons_23 <- minganga_reasons_23[,-c(56,61:64)]

londongou <- minganga_reasons_23$Loundongou
minganga_reasons_23 <- minganga_reasons_23[,-56]
minganga_visited_23 <- minganga_visited_23[,-c(59,61)]
minganga_reasons_23$Loundoungou <- londongou

write.csv(minganga_reasons_23, "data/initial/minganga_reasons_23_clean.csv")
write.csv(minganga_visited_23, "data/initial/minganga_visits_23_clean.csv")

# Ensure minganga_visited and minganga_reasons have the same dimensions
if (!all(dim(minganga_visited_23) == dim(minganga_reasons_23))) {
  stop("The dimensions of minganga_visited and minganga_reasons do not match.")
}

# Replace "Visité?" in minganga_visited with NA where minganga_reasons is NA or empty
minganga_visited_23 <- as.data.frame(
  mapply(function(visit, reason) {
    ifelse(reason == "" | is.na(reason), NA, visit)
  }, minganga_visited_23, minganga_reasons_23, SIMPLIFY = FALSE)
)

# Ensure column names are preserved
colnames(minganga_visited_23) <- colnames(minganga_reasons_23)

# Write the cleaned minganga_visited data frame to a CSV file
write.csv(minganga_visited_23, "data/initial/minganga_visits_23_cleaned_only_reasoned.csv", row.names = FALSE)



betou_visited <- dplyr::select(betou_23,contains("Visité."))
#betou_visited %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(betou_visited ) = gsub(pattern = "Regionsisitésaumoins...", replacement = "", x = names(betou_visited ))
names(betou_visited ) = gsub(pattern = "Campementsisitésaumoins...", replacement = "", x = names(betou_visited ))
names(betou_visited ) = gsub(pattern = "..Visité.", replacement = "", x = names(betou_visited ))
names(betou_visited ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(betou_visited ))
names(betou_visited ) = sub(pattern = "\\..", replacement = "", x=names(betou_visited ))
names(betou_visited ) = sub(pattern = "\\..", replacement = "", x=names(betou_visited ))
names(betou_visited ) = sub(pattern = "\\.", replacement = "", x=names(betou_visited ))
names(betou_visited ) = sub(pattern = "\\..", replacement = "", x=names(betou_visited ))
names(betou_visited ) = sub(pattern = "\\..", replacement = "", x=names(betou_visited ))
names(betou_visited ) = sub(pattern = "\\..", replacement = "", x=names(betou_visited ))
names(betou_visited ) = gsub(pattern = "Regionsisitésaumoins.", replacement = "", x = names(betou_visited ))
names(betou_visited ) = gsub(pattern = "Campementsisitésaumoins.", replacement = "", x = names(betou_visited ))
names(betou_visited ) = sub(pattern = "\\..", replacement = "", x=names(betou_visited ))
#names(betou_visited ) = gsub(pattern = ".", replacement = "", x = names(betou_visited ))
betou_visited <- betou_visited[,-c(1,81:103)]

betou_reasons <- dplyr::select(betou_23,contains("Raison"))
#betou_reasons %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(betou_reasons ) = gsub(pattern = "Regions.visités..au.moins.une.fois.....", replacement = "", x = names(betou_reasons ))
names(betou_reasons ) = gsub(pattern = "Campements.visités..au.moins.une.fois.....", replacement = "", x = names(betou_reasons ))
names(betou_reasons ) = gsub(pattern = "Campementsisitésaumoins1.....", replacement = "", x = names(betou_reasons ))
names(betou_reasons ) = gsub(pattern = "Regionsisitésaumoins1.....", replacement = "", x = names(betou_reasons ))
names(betou_reasons ) = gsub(pattern = "Campementsisitésaumoins1.....", replacement = "", x = names(betou_reasons ))
names(betou_reasons ) = gsub(pattern = "Regionsisitésaumoins1.....", replacement = "", x = names(betou_reasons ))
names(betou_reasons ) = gsub(pattern = "....Raison.de.la.visite", replacement = "", x = names(betou_reasons ))
names(betou_reasons ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(betou_reasons ))
names(betou_reasons ) = sub(pattern = "\\..", replacement = "", x=names(betou_reasons ))
names(betou_reasons ) = sub(pattern = "\\..", replacement = "", x=names(betou_reasons ))
names(betou_reasons ) = sub(pattern = "\\.", replacement = "", x=names(betou_reasons ))
names(betou_reasons ) = sub(pattern = "\\.", replacement = "", x=names(betou_reasons ))
names(betou_reasons ) = gsub(pattern = "Campementsisitésaumoins1.....", replacement = "", x = names(betou_reasons ))
names(betou_reasons ) = gsub(pattern = "Regionsisitésaumoins1.....", replacement = "", x = names(betou_reasons ))
#names(betou_reasons ) = gsub(pattern = ">>", replacement = "", x = names(betou_reasons ))
#names(betou_reasons ) = gsub(pattern = "Visité?", replacement = "", x = names(betou_reasons ))
#names(betou_reasons ) = gsub(pattern = ".", replacement = "", x = names(betou_reasons ))
betou_reasons <- betou_reasons[,-c(1:60, 140:144)]
betou_reasons <- betou_reasons[,-c(80, 82:85)]

betou_visited <- betou_visited[,-80]

write.csv(betou_reasons, "data/initial/betou_reasons_clean.csv")
write.csv(betou_visited, "data/initial/betou_visited_clean.csv")

# Ensure betou_visited and betou_reasons have the same dimensions
if (!all(dim(betou_visited) == dim(betou_reasons))) {
  stop("The dimensions of betou_visited and betou_reasons do not match.")
}

# Replace "Visité?" in betou_visited with NA where betou_reasons is NA or empty
betou_visited <- as.data.frame(
  mapply(function(visit, reason) {
    ifelse(reason == "" | is.na(reason), NA, visit)
  }, betou_visited, betou_reasons, SIMPLIFY = FALSE)
)

# Ensure column names are preserved
colnames(betou_visited) <- colnames(betou_reasons)

# Write the cleaned betou_visited data frame to a CSV file
write.csv(betou_visited, "data/initial/betou_visits_cleaned_only_reasoned.csv", row.names = FALSE)


enyelle_visited <- dplyr::select(enyelle_22,contains("Visité."))
#enyelle_visited %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(enyelle_visited ) = gsub(pattern = "Regions.visités..au.moins.une.fois.....", replacement = "", x = names(enyelle_visited ))
names(enyelle_visited ) = gsub(pattern = "Campements.visités..au.moins.une.fois.....", replacement = "", x = names(enyelle_visited ))
names(enyelle_visited ) = gsub(pattern = "..Visité.", replacement = "", x = names(enyelle_visited ))
names(enyelle_visited ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(enyelle_visited ))
names(enyelle_visited ) = sub(pattern = "\\..", replacement = "", x=names(enyelle_visited ))
names(enyelle_visited ) = sub(pattern = "\\..", replacement = "", x=names(enyelle_visited ))
names(enyelle_visited ) = sub(pattern = "\\.", replacement = "", x=names(enyelle_visited ))
names(enyelle_visited ) = sub(pattern = "\\.", replacement = "", x=names(enyelle_visited ))
#names(enyelle_visited ) = gsub(pattern = ">>", replacement = "", x = names(enyelle_visited ))
#names(enyelle_visited ) = gsub(pattern = "Visité?", replacement = "", x = names(enyelle_visited ))
#names(enyelle_visited ) = gsub(pattern = ".", replacement = "", x = names(enyelle_visited ))
enyelle_visited <- enyelle_visited[,-c(1,34,80,82:87)]
for (i in 1:nrow(enyelle_visited)) {
  furthest_place <- enyelle_visited[i,78]
  if (length(furthest_place > 0) && !is.na(furthest_place)) {
    col_place <- which(colnames(enyelle_visited) == furthest_place)
    if (length(col_place > 0)) {
      enyelle_visited[i,col_place] <- "Visité?"
    } else {
      enyelle_visited[i,ncol(enyelle_visited)+1] <- "Visité?"
      colnames(enyelle_visited)[ncol(enyelle_visited)] <- furthest_place
    }
  }
}
enyelle_visited$Lieulusoinvisité.coded <- NULL

enyelle_reasons <- dplyr::select(enyelle_22,contains("Raison"))
#enyelle_reasons %>% dplyr::mutate(col = str_remove(col, "Campements.visités..au.moins.une.fois"))
names(enyelle_reasons ) = gsub(pattern = "Regions.visités..au.moins.une.fois.....", replacement = "", x = names(enyelle_reasons ))
names(enyelle_reasons ) = gsub(pattern = "Campements.visités..au.moins.une.fois.....", replacement = "", x = names(enyelle_reasons ))
names(enyelle_reasons ) = gsub(pattern = "....Raison.de.la.visite", replacement = "", x = names(enyelle_reasons ))
names(enyelle_reasons ) = gsub(pattern = "Boucy.boucy", replacement = "Boucyboucy", x = names(enyelle_reasons ))
names(enyelle_reasons ) = sub(pattern = "\\..", replacement = "", x=names(enyelle_reasons ))
names(enyelle_reasons ) = sub(pattern = "\\..", replacement = "", x=names(enyelle_reasons ))
names(enyelle_reasons ) = sub(pattern = "\\.", replacement = "", x=names(enyelle_reasons ))
names(enyelle_reasons ) = sub(pattern = "\\.", replacement = "", x=names(enyelle_reasons ))
#names(enyelle_reasons ) = gsub(pattern = ">>", replacement = "", x = names(enyelle_reasons ))
#names(enyelle_reasons ) = gsub(pattern = "Visité?", replacement = "", x = names(enyelle_reasons ))
#names(enyelle_reasons ) = gsub(pattern = ".", replacement = "", x = names(enyelle_reasons ))
enyelle_reasons <- enyelle_reasons[,-c(1,34,80,82:87)]
enyelle_reasons <- enyelle_reasons[,-c(79:126)]

enyelle_reasons <- cbind(enyelle_reasons, enyelle_visited[,c(78:96)] )

for (j in 79:97) {
  for (row_index in 1:nrow(enyelle_reasons)) {
    if (enyelle_reasons[row_index, j] == "Visité?" && !is.na(enyelle_reasons[row_index, j])) {
      enyelle_reasons[row_index, j] <- as.character(enyelle_reasons[row_index, 78])
    }
  }
}

enyelle_reasons$Raisonourlusloinlieu.coded <- NULL
write.csv(enyelle_reasons, "data/initial/enyelle_reasons_clean.csv")
write.csv(enyelle_visited, "data/initial/enyelle_visited_clean.csv")

enyelle_visited <- enyelle_visited[,-97]

# Ensure betou_visited and betou_reasons have the same dimensions
if (!all(dim(enyelle_visited) == dim(enyelle_reasons))) {
  stop("The dimensions of enyelle_visited and enyelle_reasons do not match.")
}

# Replace "Visité?" in enyelle_visited with NA where enyelle_reasons is NA or empty
enyelle_visited <- as.data.frame(
  mapply(function(visit, reason) {
    ifelse(reason == "" | is.na(reason), NA, visit)
  }, enyelle_visited, enyelle_reasons, SIMPLIFY = FALSE)
)

# Ensure column names are preserved
colnames(enyelle_visited) <- colnames(enyelle_reasons)

# Write the cleaned enyelle_visited data frame to a CSV file
write.csv(enyelle_visited, "data/initial/enyelle_visits_cleaned_only_reasoned.csv", row.names = FALSE)


#save.image("results/intermediate/reasons_clean.RData")


# separate by means of transport
# do calcs

# dongou_visited <- cbind(dongou_personal_22, dongou_visited)
# macao_visited_22 <- cbind(macao_personal_22, macao_visited_22)
# macao_visited_23 <- cbind(macao_personal_23, macao_visited_23)
# minganga_visited_22 <- cbind(minganga_personal_22, minganga_visited_22)
# minganga_visited_23 <- cbind(minganga_personal_23, minganga_visited_23)
# enyelle_visited <- cbind(enyelle_personal_22, enyelle_visited)


camps <- subset(regions, regions$camp_region=="C")
point_camps <- cbind(regions$long, regions$lat)

# Import natural earth

library(raster)
my_extent <- extent( min(regions$long)-0.2, max(regions$long)+0.2, min(regions$lat)-0.2, max(regions$lat)+0.2)
my_extent_c <- extent( min(camps$long)-1, max(camps$long)+1, min(camps$lat)-1, max(camps$lat)+1)
#map_full <- raster("/Users/Cecilia/Documents/PhD/African_Pygmies/Spatial analysis culture/My_data/Nov_2021/NE1_HR_LC_SR_W.tif")

#setwd("/Users/Cecilia/Documents/PhD/African_Pygmies/Spatial analysis culture/My_data/shapefile_borders")
library(sf)
borders <- st_read("/Users/Cecilia/Documents/PhD/African_Pygmies/Spatial analysis culture/My_data/shapefile_borders/borders.congo.shp")
area_places <- crop(map_full, my_extent)
area_camps <- crop(map_full, my_extent_c)

macao_visited_22 <- macao_visited_22[1:168,]
macao_reasons_22 <- macao_reasons_22[1:168,]
macao_personal_22 <- macao_personal_22[1:168,]

## WE START WITH HUNT ##

png ("results/reasons_visit/places_all_regions_hunt.png", units="in", width=6, height=5.9, res=500)
plot(area_places, col=rev(hcl.colors(100, palette="Earth", rev=F)[1:70]), legend=F, box=F)
#points(point_regions, pch=16, col=holy_mountain(2)[4], cex=0.5)
plot(borders, add=T)
#map(add=T)
library(scales)
visits_dist_dongou <- as.data.frame(matrix(data=0,nrow=nrow(dongou_visited), ncol=ncol(dongou_visited)))
visits_dist_macao_22 <- as.data.frame(matrix(data=0,nrow=nrow(macao_visited_22), ncol=ncol(macao_visited_22)))
visits_dist_macao_23 <- as.data.frame(matrix(data=0,nrow=nrow(macao_visited_23), ncol=ncol(macao_visited_23)))
visits_dist_enyelle <- as.data.frame(matrix(data=0,nrow=nrow(enyelle_visited), ncol=ncol(enyelle_visited)))
visits_dist_minganga_22 <- as.data.frame(matrix(data=0,nrow=nrow(minganga_visited_22), ncol=ncol(minganga_visited_22)))
visits_dist_minganga_23 <- as.data.frame(matrix(data=0,nrow=nrow(minganga_visited_23), ncol=ncol(minganga_visited_23)))
visits_dist_betou <- as.data.frame(matrix(data=0,nrow=nrow(betou_visited), ncol=ncol(betou_visited)))

# visits_dist_dongou  <- visits_dist_dongou [,-c(1:8)]
# visits_dist_macao_22 <- visits_dist_macao_22[,-c(1:8)]
# visits_dist_macao_23 <- visits_dist_macao_23[,-c(1:8)]
# visits_dist_enyelle <- visits_dist_enyelle[,-c(1:8)]
# visits_dist_minganga_22 <- visits_dist_minganga_22[,-c(1:8)]
# visits_dist_minganga_23 <- visits_dist_minganga_23[,-c(1:10)]

distances_to_places_dongou <- as.data.frame(matrix(data=0,nrow=nrow(visits_dist_dongou), ncol=ncol(visits_dist_dongou)))
distances_to_places_macao_22 <- as.data.frame(matrix(data=0,nrow=nrow(visits_dist_macao_22), ncol=ncol(visits_dist_macao_22)))
distances_to_places_macao_23 <- as.data.frame(matrix(data=0,nrow=nrow(visits_dist_macao_23), ncol=ncol(visits_dist_macao_23)))
distances_to_places_enyelle <- as.data.frame(matrix(data=0,nrow=nrow(visits_dist_enyelle), ncol=ncol(visits_dist_enyelle)))
distances_to_places_betou <- as.data.frame(matrix(data=0,nrow=nrow(visits_dist_enyelle), ncol=ncol(visits_dist_betou)))
distances_to_places_minganga_22 <- as.data.frame(matrix(data=0,nrow=nrow(visits_dist_minganga_22), ncol=ncol(visits_dist_minganga_22)))
distances_to_places_minganga_23 <- as.data.frame(matrix(data=0,nrow=nrow(visits_dist_minganga_23), ncol=ncol(visits_dist_minganga_23)))

for (i in 1:nrow(dongou_visited)){
  print(i)
  my_places <- dongou_visited[i,]
  my_reasons <- dongou_reasons[i,]
  my_residence <- dongou_personal_22[i,1]
  #my_bp <- dongou_visited[i,7]
  res <- which(regions$region==my_residence)
  #bp <- which(regions$region==my_bp)
  # location of camp of residence
  res_lat <- as.numeric(regions[res,16])
  res_long <- as.numeric(regions[res,15])
  my_pos <- cbind(res_long, res_lat)
  # location of birthplace
  #bp_lat <- as.numeric(regions[bp,16])
  #bp_long <- as.numeric(regions[bp,15])
  #my_bp <- cbind(bp_long, bp_lat)
  my_reasons <- as.character(my_reasons[1,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  #visited <- which(my_places=="Visité?" | my_places=="1")
  # change line below to my_bp if you want to do from BP
  points(my_pos, pch=16, col=MetPalettes$Isfahan1[[1]][8], cex=0.5)
  # calculate the distances to all the places in the dataset for that particular region
  if (length(reasons > 0)) {
    for (j in 1:length(reasons)) {
      place <- which(regions$region==colnames(my_places)[reasons[j]])
      print(colnames(my_places)[reasons[j]])
      #visited_point <- point_regions[visited[j],]
      #visited_point <- cbind(regions[place,2], regions[place,3])
      visited_point_long <- as.numeric(regions[place,15])
      visited_point_lat <- as.numeric(regions[place,16])
      ifelse(visited_point_lat>5, print(paste(place, "WHAT IS THIS", sep="")),print("."))
      visited_point <- cbind(visited_point_long, visited_point_lat)
      points(visited_point, pch=16, col=MetPalettes$Isfahan1[[1]][8], cex=0.5)
      #print(visited_point)
      arc <- geosphere::gcIntermediate( c(visited_point[1], visited_point[2]),
                                        c(my_pos[1], my_pos[2]),
                                        n=0, addStartEnd=T)
      dist_travelled <- geosphere::distHaversine(c(visited_point[1], visited_point[2]), c(my_pos[1], my_pos[2]),)
      visits_dist_dongou[i,reasons[j]] <- dist_travelled
      lines(arc, col=alpha(MetPalettes$Isfahan1[[1]][8], 0.4))
    }
  }
}
for (i in 1:nrow(macao_visited_22)){
  print(i)
  my_places <- macao_visited_22[i,]
  my_reasons <- macao_reasons_22[i,]
  my_residence <- macao_personal_22[i,1]
  #my_bp <- macao_visited_22[i,7]
  res <- which(regions$region==my_residence)
  #bp <- which(regions$region==my_bp)
  # location of camp of residence
  res_lat <- as.numeric(regions[res,16])
  res_long <- as.numeric(regions[res,15])
  my_pos <- cbind(res_long, res_lat)
  # location of birthplace
  #bp_lat <- as.numeric(regions[bp,16])
  #bp_long <- as.numeric(regions[bp,15])
  #my_bp <- cbind(bp_long, bp_lat)
  my_reasons <- as.character(my_reasons[1,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  #visited <- which(my_places=="Visité?" | my_places=="1")
  # change line below to my_bp if you want to do from BP
  points(my_pos, pch=16, col=MetPalettes$Isfahan1[[1]][5], cex=0.5)
  # calculate the distances to all the places in the dataset for that particular region
  if (length(reasons > 0)) {
    for (j in 1:length(reasons)) {
      place <- which(regions$region==colnames(my_places)[reasons[j]])
      print(colnames(my_places)[reasons[j]])
      #visited_point <- point_regions[visited[j],]
      #visited_point <- cbind(regions[place,2], regions[place,3])
      visited_point_long <- as.numeric(regions[place,15])
      visited_point_lat <- as.numeric(regions[place,16])
      ifelse(visited_point_lat>5, print(paste(place, "WHAT IS THIS", sep="")),print("."))
      visited_point <- cbind(visited_point_long, visited_point_lat)
      points(visited_point, pch=16, col=MetPalettes$Isfahan1[[1]][5], cex=0.5)
      #print(visited_point)
      arc <- geosphere::gcIntermediate( c(visited_point[1], visited_point[2]),
                                        c(my_pos[1], my_pos[2]),
                                        n=0, addStartEnd=T)
      dist_travelled <- geosphere::distHaversine(c(visited_point[1], visited_point[2]), c(my_pos[1], my_pos[2]),)
      visits_dist_macao_22[i,reasons[j]] <- dist_travelled
      lines(arc, col=alpha(MetPalettes$Isfahan1[[1]][5], 0.4))
    }
  }
}
for (i in 1:nrow(macao_visited_23)){
  print(i)
  my_places <- macao_visited_23[i,]
  my_reasons <- macao_reasons_23[i,]
  my_residence <- macao_personal_23[i,1]
  #my_bp <- macao_visited_23[i,7]
  res <- which(regions$region==my_residence)
  #bp <- which(regions$region==my_bp)
  # location of camp of residence
  res_lat <- as.numeric(regions[res,16])
  res_long <- as.numeric(regions[res,15])
  my_pos <- cbind(res_long, res_lat)
  # location of birthplace
  #bp_lat <- as.numeric(regions[bp,16])
  #bp_long <- as.numeric(regions[bp,15])
  #my_bp <- cbind(bp_long, bp_lat)
  my_reasons <- as.character(my_reasons[1,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  #visited <- which(my_places=="Visité?" | my_places=="1")
  # change line below to my_bp if you want to do from BP
  points(my_pos, pch=16, col=MetPalettes$Isfahan1[[1]][5], cex=0.5)
  # calculate the distances to all the places in the dataset for that particular region
  if (length(reasons > 0)) {
    for (j in 1:length(reasons)) {
      place <- which(regions$region==colnames(my_places)[reasons[j]])
      if (length(place) > 0) {
        print(colnames(my_places)[reasons[j]])
        #visited_point <- point_regions[visited[j],]
        #visited_point <- cbind(regions[place,2], regions[place,3])
        visited_point_long <- as.numeric(regions[place,15])
        visited_point_lat <- as.numeric(regions[place,16])
        ifelse(visited_point_lat>5, print(paste(place, "WHAT IS THIS", sep="")),print("."))
        visited_point <- cbind(visited_point_long, visited_point_lat)
        points(visited_point, pch=16, col=MetPalettes$Isfahan1[[1]][5], cex=0.5)
        #print(visited_point)
        arc <- geosphere::gcIntermediate( c(visited_point[1], visited_point[2]),
                                          c(my_pos[1], my_pos[2]),
                                          n=0, addStartEnd=T)
        dist_travelled <- geosphere::distHaversine(c(visited_point[1], visited_point[2]), c(my_pos[1], my_pos[2]),)
        visits_dist_macao_23[i,reasons[j]] <- dist_travelled
        lines(arc, col=alpha(MetPalettes$Isfahan1[[1]][5], 0.4))
      }
    }
  }
}
for (i in 1:nrow(enyelle_visited)){
  print(i)
  my_places <- enyelle_visited[i,]
  my_reasons <- enyelle_reasons[i,]
  my_residence <- enyelle_personal_22[i,1]
  #my_bp <- enyelle_visited[i,7]
  res <- which(regions$region==my_residence)
  #bp <- which(regions$region==my_bp)
  # location of camp of residence
  res_lat <- as.numeric(regions[res,16])
  res_long <- as.numeric(regions[res,15])
  my_pos <- cbind(res_long, res_lat)
  # location of birthplace
  #bp_lat <- as.numeric(regions[bp,16])
  #bp_long <- as.numeric(regions[bp,15])
  #my_bp <- cbind(bp_long, bp_lat)
  my_reasons <- as.character(my_reasons[1,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  #visited <- which(my_places=="Visité?" | my_places=="1")
  # change line below to my_bp if you want to do from BP
  points(my_pos, pch=16, col=MetPalettes$Isfahan2[[1]][1], cex=0.5)
  # calculate the distances to all the places in the dataset for that particular region
  if (length(reasons > 0)) {
    for (j in 1:length(reasons)) {
      place <- which(regions$region==colnames(my_places)[reasons[j]])[1]
      if (length(place > 0) && !is.na(place)) {
        print(colnames(my_places)[reasons[j]])
        #visited_point <- point_regions[visited[j],]
        #visited_point <- cbind(regions[place,2], regions[place,3])
        visited_point_long <- as.numeric(regions[place,15])
        visited_point_lat <- as.numeric(regions[place,16])
        ifelse(visited_point_lat>5, print(paste(place, "WHAT IS THIS", sep="")),print("."))
        visited_point <- cbind(visited_point_long, visited_point_lat)
        points(visited_point, pch=16, col=MetPalettes$Isfahan2[[1]][1], cex=0.5)
        #print(visited_point)
        arc <- geosphere::gcIntermediate( c(visited_point[1], visited_point[2]),
                                          c(my_pos[1], my_pos[2]),
                                          n=0, addStartEnd=T)
        dist_travelled <- geosphere::distHaversine(c(visited_point[1], visited_point[2]), c(my_pos[1], my_pos[2]),)
        visits_dist_enyelle[i,reasons[j]] <- dist_travelled
        lines(arc, col=alpha(MetPalettes$Isfahan2[[1]][1], 0.4))
      }
    }
  }
}
for (i in 1:nrow(betou_visited)){
  print(i)
  my_places <- betou_visited[i,]
  my_reasons <- betou_reasons[i,]
  my_residence <- betou_personal_22[i,1]
  #my_bp <- betou_visited[i,7]
  res <- which(regions$region==my_residence)
  #bp <- which(regions$region==my_bp)
  # location of camp of residence
  res_lat <- as.numeric(regions[res,16])
  res_long <- as.numeric(regions[res,15])
  my_pos <- cbind(res_long, res_lat)
  # location of birthplace
  #bp_lat <- as.numeric(regions[bp,16])
  #bp_long <- as.numeric(regions[bp,15])
  #my_bp <- cbind(bp_long, bp_lat)
  my_reasons <- as.character(my_reasons[1,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  #visited <- which(my_places=="Visité?" | my_places=="1")
  # change line below to my_bp if you want to do from BP
  points(my_pos, pch=16, col=MetPalettes$Kandinsky[[1]][3], cex=0.5)
  # calculate the distances to all the places in the dataset for that particular region
  if (length(reasons > 0)) {
    for (j in 1:length(reasons)) {
      place <- which(regions$region==colnames(my_places)[reasons[j]])[1]
      if (length(place > 0) && !is.na(place)) {
        print(colnames(my_places)[reasons[j]])
        #visited_point <- point_regions[visited[j],]
        #visited_point <- cbind(regions[place,2], regions[place,3])
        visited_point_long <- as.numeric(regions[place,15])
        visited_point_lat <- as.numeric(regions[place,16])
        ifelse(visited_point_lat>5, print(paste(place, "WHAT IS THIS", sep="")),print("."))
        visited_point <- cbind(visited_point_long, visited_point_lat)
        points(visited_point, pch=16, col=MetPalettes$Kandinsky[[1]][3], cex=0.5)
        #print(visited_point)
        arc <- geosphere::gcIntermediate( c(visited_point[1], visited_point[2]),
                                          c(my_pos[1], my_pos[2]),
                                          n=0, addStartEnd=T)
        dist_travelled <- geosphere::distHaversine(c(visited_point[1], visited_point[2]), c(my_pos[1], my_pos[2]),)
        visits_dist_betou[i,reasons[j]] <- dist_travelled
        lines(arc, col=alpha(MetPalettes$Kandinsky[[1]][3], 0.4))
      }
    }
  }
}
for (i in 1:nrow(minganga_visited_22)){
  print(i)
  my_places <- minganga_visited_22[i,]
  my_reasons <- minganga_reasons_22[i,]
  my_residence <- minganga_personal_22[i,1]
  #my_bp <- minganga_visited_22[i,7]
  res <- which(regions$region==my_residence)
  #bp <- which(regions$region==my_bp)
  # location of camp of residence
  res_lat <- as.numeric(regions[res,16])
  res_long <- as.numeric(regions[res,15])
  my_pos <- cbind(res_long, res_lat)
  # location of birthplace
  #bp_lat <- as.numeric(regions[bp,16])
  #bp_long <- as.numeric(regions[bp,15])
  #my_bp <- cbind(bp_long, bp_lat)
  my_reasons <- as.character(my_reasons[1,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  #visited <- which(my_places=="Visité?" | my_places=="1")
  # change line below to my_bp if you want to do from BP
  points(my_pos, pch=16, col=MetPalettes$Isfahan2[[1]][3], cex=0.5)
  # calculate the distances to all the places in the dataset for that particular region
  if (length(reasons > 0)) {
    for (j in 1:length(reasons)) {
      place <- which(regions$region==colnames(my_places)[reasons[j]])
      print(colnames(my_places)[reasons[j]])
      #visited_point <- point_regions[visited[j],]
      #visited_point <- cbind(regions[place,2], regions[place,3])
      visited_point_long <- as.numeric(regions[place,15])
      visited_point_lat <- as.numeric(regions[place,16])
      ifelse(visited_point_lat>5, print(paste(place, "WHAT IS THIS", sep="")),print("."))
      visited_point <- cbind(visited_point_long, visited_point_lat)
      points(visited_point, pch=16, col=MetPalettes$Isfahan2[[1]][3], cex=0.5)
      #print(visited_point)
      arc <- geosphere::gcIntermediate( c(visited_point[1], visited_point[2]),
                                        c(my_pos[1], my_pos[2]),
                                        n=0, addStartEnd=T)
      dist_travelled <- geosphere::distHaversine(c(visited_point[1], visited_point[2]), c(my_pos[1], my_pos[2]),)
      visits_dist_minganga_22[i,reasons[j]] <- dist_travelled
      lines(arc, col=alpha(MetPalettes$Isfahan2[[1]][3], 0.4))
    }
  }
}
for (i in 1:nrow(minganga_visited_23)){
  print(i)
  my_places <- minganga_visited_23[i,]
  my_reasons <- minganga_reasons_23[i,]
  my_residence <- minganga_personal_23[i,1]
  #my_bp <- minganga_visited_23[i,7]
  res <- which(regions$region==my_residence)
  #bp <- which(regions$region==my_bp)
  # location of camp of residence
  res_lat <- as.numeric(regions[res,16])
  res_long <- as.numeric(regions[res,15])
  my_pos <- cbind(res_long, res_lat)
  # location of birthplace
  #bp_lat <- as.numeric(regions[bp,16])
  #bp_long <- as.numeric(regions[bp,15])
  #my_bp <- cbind(bp_long, bp_lat)
  my_reasons <- as.character(my_reasons[1,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  #visited <- which(my_places=="Visité?" | my_places=="1")
  # change line below to my_bp if you want to do from BP
  points(my_pos, pch=16, col=MetPalettes$Isfahan2[[1]][3], cex=0.5)
  # calculate the distances to all the places in the dataset for that particular region
  if (length(reasons > 0)) {
    for (j in 1:length(reasons)) {
      place <- which(regions$region==colnames(my_places)[reasons[j]])
      print(colnames(my_places)[reasons[j]])
      #visited_point <- point_regions[visited[j],]
      #visited_point <- cbind(regions[place,2], regions[place,3])
      visited_point_long <- as.numeric(regions[place,15])
      visited_point_lat <- as.numeric(regions[place,16])
      ifelse(visited_point_lat>5, print(paste(place, "WHAT IS THIS", sep="")),print("."))
      visited_point <- cbind(visited_point_long, visited_point_lat)
      points(visited_point, pch=16, col=MetPalettes$Isfahan2[[1]][3], cex=0.5)
      #print(visited_point)
      arc <- geosphere::gcIntermediate( c(visited_point[1], visited_point[2]),
                                        c(my_pos[1], my_pos[2]),
                                        n=0, addStartEnd=T)
      dist_travelled <- geosphere::distHaversine(c(visited_point[1], visited_point[2]), c(my_pos[1], my_pos[2]),)
      visits_dist_minganga_23[i,reasons[j]] <- dist_travelled
      lines(arc, col=alpha(MetPalettes$Isfahan2[[1]][3], 0.4))
    }
  }
}
dev.off()


visits_dist_dongou$median <- apply(visits_dist_dongou, 1, function(x) median(x[!is.na(x)  & x != 0]))
visits_dist_macao_22$median <- apply(visits_dist_macao_22, 1, function(x) median(x[!is.na(x)  & x != 0]))
visits_dist_macao_23$median <- apply(visits_dist_macao_23, 1, function(x) median(x[!is.na(x)  & x != 0]))
visits_dist_enyelle$median <- apply(visits_dist_enyelle, 1, function(x) median(x[!is.na(x)  & x != 0]))
visits_dist_minganga_22$median <- apply(visits_dist_minganga_22, 1, function(x) median(x[!is.na(x)  & x != 0]))
visits_dist_minganga_23$median <- apply(visits_dist_minganga_23, 1, function(x) median(x[!is.na(x)  & x != 0]))
visits_dist_betou$median <- apply(visits_dist_betou, 1, function(x) median(x[!is.na(x)  & x != 0]))


visits_dist_dongou$max <- apply(visits_dist_dongou, 1, function(x) max(x[!is.na(x)  & x != 0]))
visits_dist_macao_22$max <- apply(visits_dist_macao_22, 1, function(x) max(x[!is.na(x)  & x != 0]))
visits_dist_macao_23$max <- apply(visits_dist_macao_23, 1, function(x) max(x[!is.na(x)  & x != 0]))
visits_dist_enyelle$max <- apply(visits_dist_enyelle, 1, function(x) max(x[!is.na(x)  & x != 0]))
visits_dist_betou$max <- apply(visits_dist_betou, 1, function(x) max(x[!is.na(x)  & x != 0]))
visits_dist_minganga_22$max <- apply(visits_dist_minganga_22, 1, function(x) max(x[!is.na(x)  & x != 0]))
visits_dist_minganga_23$max <- apply(visits_dist_minganga_23, 1, function(x) max(x[!is.na(x)  & x != 0]))

visits_dist_dongou$min <- apply(visits_dist_dongou, 1, function(x) min(x[!is.na(x)  & x != 0]))
visits_dist_macao_22$min <- apply(visits_dist_macao_22, 1, function(x) min(x[!is.na(x)  & x != 0]))
visits_dist_macao_23$min <- apply(visits_dist_macao_23, 1, function(x) min(x[!is.na(x)  & x != 0]))
visits_dist_enyelle$min <- apply(visits_dist_enyelle, 1, function(x) min(x[!is.na(x)  & x != 0]))
visits_dist_betou$min <- apply(visits_dist_betou, 1, function(x) min(x[!is.na(x)  & x != 0]))
visits_dist_minganga_22$min <- apply(visits_dist_minganga_22, 1, function(x) min(x[!is.na(x)  & x != 0]))
visits_dist_minganga_23$min <- apply(visits_dist_minganga_23, 1, function(x) min(x[!is.na(x)  & x != 0]))

dongou_visited[dongou_visited=="Visité?"] <- 1
dongou_visited <- as.data.frame(lapply(dongou_visited, as.numeric))
visits_dist_dongou$places_visited <- rowSums(dongou_visited, na.rm=T)
times_hunt <- vector(length=nrow(visits_dist_dongou))
for (i in 1:nrow(dongou_reasons)) {
  my_reasons <- as.character(dongou_reasons[i,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  total_hunt <- length(reasons)
  times_hunt[i] <- total_hunt
}
visits_dist_dongou$total_hunt <- times_hunt

enyelle_visited[enyelle_visited=="Visité?"] <- 1
enyelle_visited <- as.data.frame(lapply(enyelle_visited, as.numeric))
visits_dist_enyelle$places_visited <- rowSums(enyelle_visited, na.rm=T)
times_hunt <- vector(length=nrow(visits_dist_enyelle))
for (i in 1:nrow(enyelle_reasons)) {
  my_reasons <- as.character(enyelle_reasons[i,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  total_hunt <- length(reasons)
  times_hunt[i] <- total_hunt
}
visits_dist_enyelle$total_hunt <- times_hunt

betou_visited[betou_visited=="Visité?"] <- 1
betou_visited <- as.data.frame(lapply(betou_visited, as.numeric))
visits_dist_betou$places_visited <- rowSums(betou_visited, na.rm=T)
times_hunt <- vector(length=nrow(visits_dist_betou))
for (i in 1:nrow(betou_reasons)) {
  my_reasons <- as.character(betou_reasons[i,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  total_hunt <- length(reasons)
  times_hunt[i] <- total_hunt
}
visits_dist_betou$total_hunt <- times_hunt

macao_visited_22[macao_visited_22=="Visité?"] <- 1
macao_visited_22 <- as.data.frame(lapply(macao_visited_22, as.numeric))
visits_dist_macao_22$places_visited <- rowSums(macao_visited_22, na.rm=T)
times_hunt <- vector(length=nrow(visits_dist_macao_22))
for (i in 1:nrow(macao_reasons_22)) {
  my_reasons <- as.character(macao_reasons_22[i,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  total_hunt <- length(reasons)
  times_hunt[i] <- total_hunt
}
visits_dist_macao_22$total_hunt <- times_hunt

macao_visited_23[macao_visited_23=="Visité?"] <- 1
macao_visited_23 <- as.data.frame(lapply(macao_visited_23, as.numeric))
visits_dist_macao_23$places_visited <- rowSums(macao_visited_23, na.rm=T)
times_hunt <- vector(length=nrow(visits_dist_macao_23))
for (i in 1:nrow(macao_reasons_23)) {
  my_reasons <- as.character(macao_reasons_23[i,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  total_hunt <- length(reasons)
  times_hunt[i] <- total_hunt
}
visits_dist_macao_23$total_hunt <- times_hunt

minganga_visited_22[minganga_visited_22=="Visité?"] <- 1
minganga_visited_22 <- as.data.frame(lapply(minganga_visited_22, as.numeric))
visits_dist_minganga_22$places_visited <- rowSums(minganga_visited_22, na.rm=T)
times_hunt <- vector(length=nrow(visits_dist_minganga_22))
for (i in 1:nrow(minganga_reasons_22)) {
  my_reasons <- as.character(minganga_reasons_22[i,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  total_hunt <- length(reasons)
  times_hunt[i] <- total_hunt
}
visits_dist_minganga_22$total_hunt <- times_hunt

minganga_visited_23[minganga_visited_23=="Visité?"] <- 1
minganga_visited_23 <- as.data.frame(lapply(minganga_visited_23, as.numeric))
visits_dist_minganga_23$places_visited <- rowSums(minganga_visited_23, na.rm=T)
times_hunt <- vector(length=nrow(visits_dist_minganga_23))
for (i in 1:nrow(minganga_reasons_23)) {
  my_reasons <- as.character(minganga_reasons_23[i,])
  reason <- str_detect("hunt", my_reasons)
  reasons <- which(reason==TRUE)
  total_hunt <- length(reasons)
  times_hunt[i] <- total_hunt
}
visits_dist_minganga_23$total_hunt <- times_hunt

visits_dist_dongou <- cbind(dongou_personal_22, visits_dist_dongou)
visits_dist_macao_22 <- cbind(macao_personal_22, visits_dist_macao_22[1:168,])
visits_dist_macao_23 <- cbind(macao_personal_23, visits_dist_macao_23)
visits_dist_minganga_23 <- cbind(minganga_personal_23, visits_dist_minganga_23)
visits_dist_minganga_22 <- cbind(minganga_personal_22, visits_dist_minganga_22)
visits_dist_enyelle <- cbind(enyelle_personal_22, visits_dist_enyelle)
visits_dist_betou <- cbind(betou_personal_22, visits_dist_betou)

# Remove children
visits_dist_dongou <- subset(visits_dist_dongou, visits_dist_dongou$Tranche.d.age !="Enfant")
visits_dist_macao_22 <- subset(visits_dist_macao_22, visits_dist_macao_22$Tranche.d.age !="Enfant")
visits_dist_macao_23 <- subset(visits_dist_macao_23, visits_dist_macao_23$Tranche.d.age !="Enfant")
visits_dist_minganga_23 <- subset(visits_dist_minganga_23, visits_dist_minganga_23$Tranche.d.age !="Enfant")
visits_dist_minganga_22 <- subset(visits_dist_minganga_22, visits_dist_minganga_22$Tranche.d.age !="Enfant")
visits_dist_enyelle <- subset(visits_dist_enyelle, visits_dist_enyelle$Tranche.d.age !="Enfant")
visits_dist_betou <- subset(visits_dist_betou, visits_dist_betou$Tranche.d.age !="Enfant")

write.csv(visits_dist_dongou, "results/reasons_visit/visits_distances_dongou_max_hunt.csv")
write.csv(visits_dist_macao_22, "results/reasons_visit/visits_distances_macao_22_max_hunt.csv")
write.csv(visits_dist_macao_23, "results/reasons_visit/visits_distances_macao_23_max_hunt.csv")
write.csv(visits_dist_enyelle, "results/reasons_visit/visits_distances_enyelle_max_hunt.csv")
write.csv(visits_dist_betou, "results/reasons_visit/visits_distances_betou_max_hunt.csv")
write.csv(visits_dist_minganga_22, "results/reasons_visit/visits_distances_minganga_22_max_hunt.csv")
write.csv(visits_dist_minganga_23, "results/reasons_visit/visits_distances_minganga_23_max_hunt.csv")


# Now make general data frame for statistical analyses


visits_dist_dongou_red <- subset(visits_dist_dongou, select=c("Nomducampement",
                                                              "MY.REFERENCE",
                                                              "Sexe",
                                                              "Lieu.de.naissance.coded",
                                                              "Tranche.d.age",
                                                              "median", "max", "min",
                                                              "places_visited", "total_hunt"))
visits_dist_dongou_red$region <- "Dongou"
colnames(visits_dist_dongou_red) <- c("residence_camp", "ref", "sex", "birthplace", "age", "median_dist",
                                      "max_dist", "min_dist", "places_visited", "times_hunt", "region")

visits_dist_macao_22_red <- subset(visits_dist_macao_22, select=c("Nom.du.campement",
                                                                  "MY.REFERENCE",
                                                                  "Sexe",
                                                                  "Lieu.de.naissance.coded",
                                                                  "Tranche.d.age",
                                                                  "median", "max", "min",
                                                                  "places_visited", "total_hunt"))
visits_dist_macao_22_red$region <- "Macao"
colnames(visits_dist_macao_22_red) <- c("residence_camp", "ref", "sex", "birthplace", "age", "median_dist",
                                        "max_dist", "min_dist", "places_visited", "times_hunt", "region")

visits_dist_macao_23_red <- subset(visits_dist_macao_23, select=c("Nom.du.campement.coded",
                                                                  "MY.REFERENCE",
                                                                  "Sexe",
                                                                  "Lieu.de.naissance.coded",
                                                                  "Tranche.d.age",
                                                                  "median", "max", "min",
                                                                  "places_visited", "total_hunt"))
visits_dist_macao_23_red$region <- "Macao"
colnames(visits_dist_macao_23_red) <- c("residence_camp", "ref", "sex", "birthplace", "age", "median_dist",
                                        "max_dist", "min_dist", "places_visited", "times_hunt", "region")

visits_dist_enyelle_red <- subset(visits_dist_enyelle, select=c("Nom.du.campement",
                                                                "MY.REFERENCE",
                                                                "Sexe",
                                                                "Lieu.de.naissance.coded",
                                                                "Tranche.d.age",
                                                                "median", "max", "min",
                                                                "places_visited", "total_hunt"))
visits_dist_enyelle_red$region <- "Enyelle"
colnames(visits_dist_enyelle_red) <- c("residence_camp", "ref", "sex", "birthplace", "age", "median_dist",
                                       "max_dist","min_dist", "places_visited", "times_hunt",  "region")

visits_dist_betou_red <- subset(visits_dist_betou, select=c("Nom.du.campement",
                                                            "MY.REFERENCE",
                                                            "Sexe",
                                                            "Lieu.de.naissance.coded",
                                                            "Tranche.d.age",
                                                            "median", "max", "min",
                                                            "places_visited", "total_hunt"))
visits_dist_betou_red$region <- "betou"
colnames(visits_dist_betou_red) <- c("residence_camp", "ref", "sex", "birthplace", "age", "median_dist",
                                     "max_dist","min_dist", "places_visited", "times_hunt",  "region")

visits_dist_minganga_22_red <- subset(visits_dist_minganga_22, select=c("Nom.du.campement",
                                                                        "MY.REFERENCE",
                                                                        "Sexe",
                                                                        "Lieu.de.naissance.coded",
                                                                        "Tranche.d.age",
                                                                        "median", "max", "min",
                                                                        "places_visited", "total_hunt"))
visits_dist_minganga_22_red$region <- "Minganga"
colnames(visits_dist_minganga_22_red) <- c("residence_camp", "ref", "sex", "birthplace", "age", "median_dist",
                                           "max_dist", "min_dist", "places_visited", "times_hunt","region")

visits_dist_minganga_23_red <- subset(visits_dist_minganga_23, select=c("Nom.du.campement.coded",
                                                                        "MY.REFERENCE",
                                                                        "Sexe",
                                                                        "Lieu.de.naissance.coded",
                                                                        "Tranche.d.age",
                                                                        "median", "max", "min",
                                                                        "places_visited", "total_hunt"))
visits_dist_minganga_23_red$region <- "Minganga"
colnames(visits_dist_minganga_23_red) <- c("residence_camp", "ref", "sex", "birthplace", "age", "median_dist",
                                           "max_dist",  "min_dist", "places_visited", "times_hunt","region")

visits_dist_all <- rbind(visits_dist_dongou_red, visits_dist_macao_22_red, visits_dist_macao_23_red,
                         visits_dist_minganga_23_red, visits_dist_minganga_22_red, visits_dist_enyelle_red, visits_dist_betou_red)

write.csv(visits_dist_all, "results/reasons_visit/dist_everyone_hunt.csv")


# READ IN DATA

visits_dist_dongou <- read.csv("results/reasons_visit/visits_distances_dongou_max_hunt.csv")[,-c(1:6,8:9)]
visits_dist_macao_22 <- read.csv("results/reasons_visit/visits_distances_macao_22_max_hunt.csv")[,-c(1)]
visits_dist_macao_23 <- read.csv("results/reasons_visit/visits_distances_macao_23_max_hunt.csv")[,-c(1)]
visits_dist_enyelle <- read.csv("results/reasons_visit/visits_distances_enyelle_max_hunt.csv")[,-c(1:6,8:9)]
visits_dist_betou <- read.csv("results/reasons_visit/visits_distances_betou_max_hunt.csv")[,-c(1:6,8:9)]
visits_dist_minganga_22 <- read.csv("results/reasons_visit/visits_distances_minganga_22_max_hunt.csv")[,-c(1)]
visits_dist_minganga_23 <- read.csv("results/reasons_visit/visits_distances_minganga_23_max_hunt.csv")[,-c(1)]


library(dplyr)
library(tidyr)

# Function to align columns between multiple data frames
align_columns <- function(df_list) {
  all_columns <- unique(unlist(lapply(df_list, names))) # Get all unique column names
  aligned_list <- lapply(df_list, function(df) {
    missing_cols <- setdiff(all_columns, names(df))
    df[missing_cols] <- NA # Add missing columns as NA
    df <- df[, all_columns] # Reorder columns
    return(df)
  })
  return(aligned_list)
}

# Combine and align columns for grouped regions
aligned_macao <- align_columns(list(visits_dist_macao_22, visits_dist_macao_23))
visits_dist_macao <- do.call(rbind, aligned_macao)

aligned_minganga <- align_columns(list(visits_dist_minganga_22, visits_dist_minganga_23))
visits_dist_minganga <- do.call(rbind, aligned_minganga)


# List of region-specific datasets and their corresponding CSV filenames
region_datasets <- list(
  "Dongou" = visits_dist_dongou,
  "Macao" = visits_dist_macao,
  "Enyelle" = visits_dist_enyelle,
  "Betou" = visits_dist_betou,
  "Minganga" = visits_dist_minganga
)

# Initialize a list to store summary statistics for all regions
all_summary_stats <- list()

# Loop through each region
for (region_name in names(region_datasets)) {
  # Extract the dataset for the current region
  region_data <- region_datasets[[region_name]]

  # Separate by sex
  region_m <- subset(region_data, Sexe == "M")
  region_f <- subset(region_data, Sexe == "F")

  # Remove unnecessary columns
  cols_to_remove <- c("median", "max", "min", "places_visited", "total_hunt", "Sexe")
  region_m <- region_m[, !(names(region_m) %in% cols_to_remove)]
  region_f <- region_f[, !(names(region_f) %in% cols_to_remove)]

  # Convert to matrices
  region_m <- as.matrix(region_m)  # Exclude the first 8 columns
  region_f <- as.matrix(region_f)  # Exclude the first 8 columns

  # Filter out non-positive values
  region_m <- region_m[region_m > 0]
  region_f <- region_f[region_f > 0]

  region_m <- region_m[!is.na(region_m)]
  region_f <- region_f[!is.na(region_f)]

  # Equalize lengths by padding with NA
  n1 <- length(region_f)
  n2 <- length(region_m)
  if (n1 > n2) {
    region_m <- c(region_m, rep(NA, n1 - n2))
  } else {
    region_f <- c(region_f, rep(NA, n2 - n1))
  }

  # Check if either group is empty
  if (length(region_f) == 0 | length(region_m) == 0) {
    # If one group is missing, output NA
    summary_stats <- data.frame(
      Region = region_name,
      men_mean = NA,
      men_median = NA,
      women_mean = NA,
      women_median = NA,
      t_statistic = NA,
      t_p_value = NA,
      wilcox_statistic = NA,
      wilcox_p_value = NA,
      men_n = n2,
      women_n = n1
    )
    all_summary_stats[[region_name]] <- summary_stats
    next
  }


  # Combine into a single data frame
  region_df <- as.data.frame(cbind(region_f, region_m))
  colnames(region_df) <- c("Females", "Males")
  region_df$Females <- as.numeric(region_df$Females)
  region_df$Males <- as.numeric(region_df$Males)

  # Calculate summary statistics
  men_mean <- mean(region_df$Males, na.rm = TRUE)
  men_median <- median(region_df$Males, na.rm = TRUE)
  women_mean <- mean(region_df$Females, na.rm = TRUE)
  women_median <- median(region_df$Females, na.rm = TRUE)

  # Perform statistical tests
  welch_t_test <- tryCatch(
    t.test(region_df$Males, region_df$Females, var.equal = FALSE),
    error = function(e) return(NULL)
  )

  wilcox_test <- tryCatch(
    wilcox.test(region_df$Males, region_df$Females, var.equal = FALSE),
    error = function(e) return(NULL)
  )

  # Save summary statistics
  summary_stats <- data.frame(
    Region = region_name,
    men_mean = men_mean,
    men_median = men_median,
    women_mean = women_mean,
    women_median = women_median,
    t_statistic = ifelse(!is.null(welch_t_test), welch_t_test$statistic, NA),
    t_p_value = ifelse(!is.null(welch_t_test), welch_t_test$p.value, NA),
    wilcox_statistic = ifelse(!is.null(wilcox_test), wilcox_test$statistic, NA),
    wilcox_p_value = ifelse(!is.null(wilcox_test), wilcox_test$p.value, NA),
    men_n = n2,
    women_n = n1
  )

  # Store in the list
  all_summary_stats[[region_name]] <- summary_stats

  # Save to a CSV file for the current region
  write.csv(summary_stats, file = paste0("results/reasons_visit/summary_statistics_", region_name, "_hunt.csv"), row.names = FALSE)
}

# Combine all summary statistics into a single data frame
combined_summary_stats <- do.call(rbind, all_summary_stats)
combined_summary_stats
# Save the combined summary statistics
write.csv(combined_summary_stats, "results/reasons_visit/summary_statistics_all_regions_hunt.csv", row.names = FALSE)


# Make density plots of distances travelled to hunt
# separate by sex
visits_dist_enyelle_m <- subset(visits_dist_enyelle, visits_dist_enyelle$Sexe=="M")
visits_dist_enyelle_f <- subset(visits_dist_enyelle, visits_dist_enyelle$Sexe=="F")
visits_dist_enyelle_m <- visits_dist_enyelle_m[,-c(1:8)]
visits_dist_enyelle_m$median <- NULL
visits_dist_enyelle_m$max <- NULL
visits_dist_enyelle_m$min <- NULL
visits_dist_enyelle_m$places_visited <- NULL
visits_dist_enyelle_m$total_hunt <- NULL
visits_dist_enyelle_f <- visits_dist_enyelle_f[,-c(1:8)]
visits_dist_enyelle_f$median <- NULL
visits_dist_enyelle_f$max <- NULL
visits_dist_enyelle_f$min <- NULL
visits_dist_enyelle_f$places_visited <- NULL
visits_dist_enyelle_f$total_hunt <- NULL
visits_dist_enyelle_f <- as.matrix(visits_dist_enyelle_f)
visits_dist_enyelle_m <- as.matrix(visits_dist_enyelle_m)
visits_dist_enyelle_index_m <- subset(visits_dist_enyelle_m > 0)
visits_dist_enyelle_index_f <- subset(visits_dist_enyelle_f > 0)
visits_dist_enyelle_m  <- visits_dist_enyelle_m[visits_dist_enyelle_index_m == T]
visits_dist_enyelle_f  <- visits_dist_enyelle_f[visits_dist_enyelle_index_f == T]

visits_dist_betou_m <- subset(visits_dist_betou, visits_dist_betou$Sexe=="M")
visits_dist_betou_f <- subset(visits_dist_betou, visits_dist_betou$Sexe=="F")
visits_dist_betou_m <- visits_dist_betou_m[,-c(1:8)]
visits_dist_betou_m$median <- NULL
visits_dist_betou_m$max <- NULL
visits_dist_betou_m$min <- NULL
visits_dist_betou_m$places_visited <- NULL
visits_dist_betou_m$total_hunt <- NULL
visits_dist_betou_f <- visits_dist_betou_f[,-c(1:8)]
visits_dist_betou_f$median <- NULL
visits_dist_betou_f$max <- NULL
visits_dist_betou_f$min <- NULL
visits_dist_betou_f$places_visited <- NULL
visits_dist_betou_f$total_hunt <- NULL
visits_dist_betou_f <- as.matrix(visits_dist_betou_f)
visits_dist_betou_m <- as.matrix(visits_dist_betou_m)
visits_dist_betou_index_m <- subset(visits_dist_betou_m > 0)
visits_dist_betou_index_f <- subset(visits_dist_betou_f > 0)
visits_dist_betou_m  <- visits_dist_betou_m[visits_dist_betou_index_m == T]
visits_dist_betou_f  <- visits_dist_betou_f[visits_dist_betou_index_f == T]


visits_dist_dongou_m <- subset(visits_dist_dongou, visits_dist_dongou$Sexe=="M")
visits_dist_dongou_f <- subset(visits_dist_dongou, visits_dist_dongou$Sexe=="F")
visits_dist_dongou_m <- visits_dist_dongou_m[,-c(1:8)]
visits_dist_dongou_m$median <- NULL
visits_dist_dongou_m$max <- NULL
visits_dist_dongou_m$min <- NULL
visits_dist_dongou_m$places_visited <- NULL
visits_dist_dongou_m$total_hunt <- NULL
visits_dist_dongou_f <- visits_dist_dongou_f[,-c(1:8)]
visits_dist_dongou_f$median <- NULL
visits_dist_dongou_f$max <- NULL
visits_dist_dongou_f$min <- NULL
visits_dist_dongou_f$places_visited <- NULL
visits_dist_dongou_f$total_hunt <- NULL
visits_dist_dongou_f <- as.matrix(visits_dist_dongou_f)
visits_dist_dongou_m <- as.matrix(visits_dist_dongou_m)
visits_dist_dongou_index_m <- subset(visits_dist_dongou_m > 0)
visits_dist_dongou_index_f <- subset(visits_dist_dongou_f > 0)
visits_dist_dongou_m  <- visits_dist_dongou_m[visits_dist_dongou_index_m == T]
visits_dist_dongou_f  <- visits_dist_dongou_f[visits_dist_dongou_index_f == T]

visits_dist_macao_22_m <- subset(visits_dist_macao_22, visits_dist_macao_22$Sexe=="M")
visits_dist_macao_22_f <- subset(visits_dist_macao_22, visits_dist_macao_22$Sexe=="F")
visits_dist_macao_22_m <- visits_dist_macao_22_m[,-c(1:8)]
visits_dist_macao_22_m$median <- NULL
visits_dist_macao_22_m$max <- NULL
visits_dist_macao_22_m$min <- NULL
visits_dist_macao_22_m$places_visited <- NULL
visits_dist_macao_22_m$total_hunt <- NULL
visits_dist_macao_22_f <- visits_dist_macao_22_f[,-c(1:8)]
visits_dist_macao_22_f$median <- NULL
visits_dist_macao_22_f$max <- NULL
visits_dist_macao_22_f$min <- NULL
visits_dist_macao_22_f$places_visited <- NULL
visits_dist_macao_22_f$total_hunt <- NULL
visits_dist_macao_22_f <- as.matrix(visits_dist_macao_22_f)
visits_dist_macao_22_m <- as.matrix(visits_dist_macao_22_m)
visits_dist_macao_22_index_m <- subset(visits_dist_macao_22_m > 0)
visits_dist_macao_22_index_f <- subset(visits_dist_macao_22_f > 0)
visits_dist_macao_22_m  <- visits_dist_macao_22_m[visits_dist_macao_22_index_m == T]
visits_dist_macao_22_f  <- visits_dist_macao_22_f[visits_dist_macao_22_index_f == T]

visits_dist_macao_23_m <- subset(visits_dist_macao_23, visits_dist_macao_23$Sexe=="M")
visits_dist_macao_23_f <- subset(visits_dist_macao_23, visits_dist_macao_23$Sexe=="F")
visits_dist_macao_23_m <- visits_dist_macao_23_m[,-c(1:8)]
visits_dist_macao_23_m$median <- NULL
visits_dist_macao_23_m$max <- NULL
visits_dist_macao_23_m$min <- NULL
visits_dist_macao_23_m$places_visited <- NULL
visits_dist_macao_23_m$total_hunt <- NULL
visits_dist_macao_23_f <- visits_dist_macao_23_f[,-c(1:8)]
visits_dist_macao_23_f$median <- NULL
visits_dist_macao_23_f$max <- NULL
visits_dist_macao_23_f$min <- NULL
visits_dist_macao_23_f$places_visited <- NULL
visits_dist_macao_23_f$total_hunt <- NULL
visits_dist_macao_23_f <- as.matrix(visits_dist_macao_23_f)
visits_dist_macao_23_m <- as.matrix(visits_dist_macao_23_m)
visits_dist_macao_23_index_m <- subset(visits_dist_macao_23_m > 0)
visits_dist_macao_23_index_f <- subset(visits_dist_macao_23_f > 0)
visits_dist_macao_23_m  <- visits_dist_macao_23_m[visits_dist_macao_23_index_m == T]
visits_dist_macao_23_f  <- visits_dist_macao_23_f[visits_dist_macao_23_index_f == T]

visits_dist_minganga_22_m <- subset(visits_dist_minganga_22, visits_dist_minganga_22$Sexe=="M")
visits_dist_minganga_22_f <- subset(visits_dist_minganga_22, visits_dist_minganga_22$Sexe=="F")
visits_dist_minganga_22_m <- visits_dist_minganga_22_m[,-c(1:8)]
visits_dist_minganga_22_m$median <- NULL
visits_dist_minganga_22_m$max <- NULL
visits_dist_minganga_22_m$min <- NULL
visits_dist_minganga_22_m$places_visited <- NULL
visits_dist_minganga_22_m$total_hunt <- NULL
visits_dist_minganga_22_f <- visits_dist_minganga_22_f[,-c(1:8)]
visits_dist_minganga_22_f$median <- NULL
visits_dist_minganga_22_f$max <- NULL
visits_dist_minganga_22_f$min <- NULL
visits_dist_minganga_22_f$places_visited <- NULL
visits_dist_minganga_22_f$total_hunt <- NULL
visits_dist_minganga_22_f <- as.matrix(visits_dist_minganga_22_f)
visits_dist_minganga_22_m <- as.matrix(visits_dist_minganga_22_m)
visits_dist_minganga_22_index_m <- subset(visits_dist_minganga_22_m > 0)
visits_dist_minganga_22_index_f <- subset(visits_dist_minganga_22_f > 0)
visits_dist_minganga_22_m  <- visits_dist_minganga_22_m[visits_dist_minganga_22_index_m == T]
visits_dist_minganga_22_f  <- visits_dist_minganga_22_f[visits_dist_minganga_22_index_f == T]

visits_dist_minganga_23_m <- subset(visits_dist_minganga_23, visits_dist_minganga_23$Sexe=="M")
visits_dist_minganga_23_f <- subset(visits_dist_minganga_23, visits_dist_minganga_23$Sexe=="F")
visits_dist_minganga_23_m <- visits_dist_minganga_23_m[,-c(1:10)]
visits_dist_minganga_23_m$median <- NULL
visits_dist_minganga_23_m$max <- NULL
visits_dist_minganga_23_m$min <- NULL
visits_dist_minganga_23_m$places_visited <- NULL
visits_dist_minganga_23_m$total_hunt <- NULL
visits_dist_minganga_23_f <- visits_dist_minganga_23_f[,-c(1:10)]
visits_dist_minganga_23_f$median <- NULL
visits_dist_minganga_23_f$max <- NULL
visits_dist_minganga_23_f$min <- NULL
visits_dist_minganga_23_f$places_visited <- NULL
visits_dist_minganga_23_f$total_hunt <- NULL
visits_dist_minganga_23_f <- as.matrix(visits_dist_minganga_23_f)
visits_dist_minganga_23_m <- as.matrix(visits_dist_minganga_23_m)
visits_dist_minganga_23_index_m <- subset(visits_dist_minganga_23_m > 0)
visits_dist_minganga_23_index_f <- subset(visits_dist_minganga_23_f > 0)
visits_dist_minganga_23_m  <- visits_dist_minganga_23_m[visits_dist_minganga_23_index_m == T]
visits_dist_minganga_23_f  <- visits_dist_minganga_23_f[visits_dist_minganga_23_index_f == T]


# By sex
whole_df_f <- c(visits_dist_macao_22_f, visits_dist_macao_22_f,
                visits_dist_minganga_22_f, visits_dist_minganga_23_f,
                visits_dist_enyelle_f, visits_dist_betou_f, visits_dist_dongou_f)
whole_df_m <- c(visits_dist_macao_22_m, visits_dist_macao_22_m,
                visits_dist_minganga_22_m, visits_dist_minganga_23_m,
                visits_dist_enyelle_m, visits_dist_betou_m, visits_dist_dongou_m)


n1 <- length(whole_df_f)
n2 <- length(whole_df_m)

if (n1 > n2) {
  whole_df_m <- c(whole_df_m, rep(NA, n1 - n2))
} else {
  whole_df_f <- c(whole_df_f, rep(NA, n2 - n1))
}

whole_df <- as.data.frame(cbind(whole_df_f, whole_df_m))
colnames(whole_df) <- c("Females", "Males")
graph <- whole_df %>%
  gather(category, value)

colnames (whole_df) <- c("women_distance", "men_distance")
whole_df$men_distance <- as.numeric(whole_df$men_distance)
whole_df$women_distance <- as.numeric(whole_df$women_distance)

# Calculate mean and median for men's distance
men_mean <- mean(as.numeric(whole_df$men_distance), na.rm=T)
men_median <- median(as.numeric(whole_df$men_distance), na.rm=T)

# Calculate mean and median for women's distance
women_mean <- mean(as.numeric(whole_df$women_distance), na.rm=T)
women_median <- median(as.numeric(whole_df$women_distance), na.rm=T)

# the variance is not equal - variance for men a lot higher
welch_t_test_result <- t.test(whole_df$men_distance, whole_df$women_distance, var.equal = FALSE)
wilcox_test_result <- wilcox.test(whole_df$men_distance, whole_df$women_distance, var.equal = FALSE)

# Create a data frame with the summary statistics
summary_df <- data.frame(
  men_mean = men_mean,
  men_median = men_median,
  women_mean = women_mean,
  women_median = women_median,
  t_statistic = welch_t_test_result$statistic,
  p_value = welch_t_test_result$p.value,
  wilcox_result = wilcox_test_result$statistic,
  wilcox_p_value = wilcox_test_result$p.value,
  men_n = length(whole_df_m),
  women_n = length(whole_df_f)
)

# Save the data frame as a CSV file
# only one woman hunted
write.csv(summary_df, "results/reasons_visit/summary_statistics_hunt.csv", row.names = FALSE)



graph$value <- graph$value/1000
graph <- graph[complete.cases(graph),]
mean_vals <- tapply(graph$value, graph$category, median)
ggplot(graph, aes(x = value, fill = category)) +
  geom_histogram(binwidth = 10, color = "black", alpha=0.8) +
  theme_bw() +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][6])) +
  labs(title="Distances travelled to hunt",x="Km", y = "Count") +
  #geom_vline(xintercept = mean_vals[1], color = MetPalettes$Isfahan1[[1]][8], linetype = "dashed", lwd=1) +
  geom_vline(xintercept = mean_vals[1], color = MetPalettes$Isfahan1[[1]][6], linetype = "dashed", lwd=1) +
  #annotate("rect", xmin = mean_vals[1]+1, xmax = mean_vals[1]+15, ymin = 10, ymax = 10.7,
  #fill = "white", color = MetPalettes$Isfahan1[[1]][8], size=1) +
  #annotate("text", x = mean_vals[1]+9, y = 10, label = round(mean_vals[1], 2),
  #color = MetPalettes$Isfahan1[[1]][8], size = 4, vjust = -1) +
  #annotate("rect", xmin = mean_vals[2]-14, xmax = mean_vals[2], ymin = 10, ymax = 10.7,
  #fill = "white", color = MetPalettes$Isfahan2[[1]][1], size=1) +
  annotate("text", x = mean_vals[1]+70, y = 10, label = round(mean_vals[1], 2),
           color = MetPalettes$Isfahan1[[1]][6], size = 5, vjust = -1) +
  ylim(0,200)+
  scale_x_continuous(breaks = seq(0, 1000, by = 100))
ggsave("results/reasons_visit/distance_hunt_by_sex_std.png", width = 6.5, height=5)

# MAKE PLOTS, DO CALCULATIONS

visits_dist_all <- read.csv("results/reasons_visit/dist_everyone_hunt.csv")[,-1]
#visits_dist_all <- subset(visits_dist_all, visits_dist_all$age != "Enfant")
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

write.csv(sum_df_by_age_sex, "results/reasons_visit/sum_df_by_age_sex_hunt.csv")
write.csv(sum_df_by_age, "results/reasons_visit/sum_df_by_age_hunt.csv")

sum_df_by_age_sex_reg <- visits_dist_all %>% group_by(age, sex, region) %>%
  summarise(median_dist_mean = mean(median_dist, na.rm=T)/1000,
            median_dist_median = median(median_dist, na.rm=T)/1000,
            max_dist_mean = mean(max_dist, na.rm=T)/1000,
            max_dist_median = median(max_dist, na.rm=T)/1000) %>%
  as.data.frame()
sum_df_by_age_sex_reg
write.csv(sum_df_by_age_sex_reg, "results/reasons_visit/sum_df_by_sex_age_reg_hunt.csv")

visits_dist_all$median_dist_km <- visits_dist_all$median_dist/1000
p1 <- visits_dist_all %>% dplyr::select(median_dist_km, age, sex) %>%
  #pivot_longer(., cols = c(dist_living_own_bp, dist_living_spouse_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = age, y = median_dist_km, fill = sex)) +
  scale_fill_manual(values=c( MetPalettes$Isfahan1[[1]][6])) +
  labs(x = "Age group", y = "Median distance travelled to hunt (km)") +
  theme_minimal() +
  ggtitle("hunt") +
  geom_boxplot()

# 99% of observations plotted
pi <- p1 + coord_cartesian(ylim = quantile(visits_dist_all$median_dist_km, c(0.01, 0.95), na.rm=T), clip="on")
pi
ggplot2::ggsave("results/reasons_visit/sex_age_median_from_res_hunt.png", width = 7, height=6)

visits_dist_all$median_dist_km <- visits_dist_all$median_dist/1000
visits_dist_all$region[visits_dist_all$region == "betou"] <- "Betou"

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
  labs(x = "Age group", y = "Median distance travelled to hunt (km)") +
  theme_minimal() +
  coord_cartesian(ylim = y_limits, clip = "on") +
  facet_wrap(~region, scales = "free", ncol = 2) +  # Facet by region
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

p_region_boxplot
# Save the faceted boxplot
ggplot2::ggsave("results/reasons_visit/sex_age_boxplot_with_points_by_region_hunt.png", plot = p_region_boxplot, width = 9, height = 11)

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
  labs(x = "Age group", y = "Median distance travelled to hunt (km)") +
  theme_minimal() +
  coord_cartesian(ylim = y_limits, clip = "on") +
  facet_wrap(~region, scales = "free", ncol = 2) +  # Facet by region
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

p_region_boxplot_macmin
# Save the faceted boxplot
ggplot2::ggsave("results/reasons_visit/sex_age_boxplot_with_points_by_region_macmin_hunt.png", plot = p_region_boxplot_macmin, width = 9, height = 6)


# Prepare an empty data frame to store the results
results <- data.frame(
  Region = character(),
  Age_Group = character(),
  W_Statistic = numeric(),
  P_Value = numeric(),
  #Female_Median = numeric(),
  Male_Median = numeric(),
  #Female_N = integer(),
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
    male_data <- subset %>%
      filter(sex == "M") %>%
      pull(median_dist_km)

    sum_men <- subset %>%
      filter(sex == "M") %>% # Filter for men
      summarise(total_hunt = sum(times_hunt, na.rm = TRUE)) %>% # Sum the column, ignoring NA
      pull(total_hunt)

    # Store the results
    results <- rbind(
      results,
      data.frame(
        Region = as.character(region),
        Age_Group = as.character(age_group),
        #W_Statistic = ifelse(!is.null(w_stat), w_stat, NA),
        P_Value = ifelse(!is.null(p_value), p_value, NA),
        #Female_Median = ifelse(length(female_data) > 0, median(female_data, na.rm = TRUE), NA),
        Male_Median = ifelse(length(male_data) > 0, median(male_data, na.rm = TRUE), NA),
        #Female_N = length(female_data),
        Male_N = sum_men
      )
    )
  }
}

print(results)
# Optionally save the results to a CSV file
write.csv(results, "results/reasons_visit/wilcoxon_results_sex_differences_hunt.csv", row.names = FALSE)

# Prepare an empty data frame to store the results
results <- data.frame(
  Region = character(),
  #Age_Group = character(),
  W_Statistic = numeric(),
  P_Value = numeric(),
  #Female_Median = numeric(),
  Male_Median = numeric(),
  #Female_N = integer(),
  Male_N = integer(),
  stringsAsFactors = FALSE
)

# Iterate through each region and age group
for (region in unique(visits_dist_all$region)) {
    # Filter data for the current region and age group
    subset <- visits_dist_all %>%
      filter(region == !!region)

    # Split the data by sex
    male_data <- subset %>%
      filter(sex == "M") %>%
      pull(median_dist_km)

    sum_men <- subset %>%
      filter(sex == "M") %>% # Filter for men
      summarise(total_hunt = sum(times_hunt, na.rm = TRUE)) %>% # Sum the column, ignoring NA
      pull(total_hunt)

    # Store the results
    results <- rbind(
      results,
      data.frame(
        Region = as.character(region),
        #Age_Group = as.character(age_group),
        #W_Statistic = ifelse(!is.null(w_stat), w_stat, NA),
        P_Value = ifelse(!is.null(p_value), p_value, NA),
        #Female_Median = ifelse(length(female_data) > 0, median(female_data, na.rm = TRUE), NA),
        Male_Median = ifelse(length(male_data) > 0, median(male_data, na.rm = TRUE), NA),
        #Female_N = length(female_data),
        Male_N = sum_men
      )
    )
  }


print(results)
# Optionally save the results to a CSV file
write.csv(results, "results/reasons_visit/wilcoxon_results_sex_differences_hunt_allages.csv", row.names = FALSE)


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
                                      ylab="Median distance travelled to hunt (km)",
                                      xlab="Sex",
                                      ggplot.component = ggplot2::scale_y_continuous(limits = (c(1, 100))))
#ggplot.component = scale_y_continuous(limits = c(0, 500))
#ggplot2::scale_y_continuous(limits = c(0, 120)) +
#ggplot2::scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]))
ggplot2::ggsave("results/reasons_visit/sex_age_median_from_res_stats_hunt.png", width = 17, height=8)

visits_dist_all %>%
  #select(c('Age','Sexe.x', 'max')) %>%
  #tidyr::pivot_longer(cols = -Age)
  #separate(Age, into = c("Age", "Sexe.x"), sep = " \\(") %>%
  #mutate(Cluster = sub("\\)", "", Cluster)) %>%
  #group_by(sex) %>%
  ggstatsplot::grouped_ggbetweenstats(x = age, y = median_dist_km,
                                      plot.type = "box",
                                      grouping.var = sex,
                                      type="p",
                                      outlier.shape=NA,
                                      #violin.args = list(width = 0),
                                      package="wesanderson",
                                      palette="Darjeeling1",
                                      pairwise.display = "significant",
                                      ylab="Median distance travelled to hunt (km)",
                                      xlab="Age group",
                                      ggplot.component = ggplot2::scale_y_continuous(limits = (c(1, 100))))
#ggplot2::scale_y_continuous(limits = c(0, 162))
#ggplot2::scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]))
ggplot2::ggsave("results/reasons_visit/age_sex_median_from_res_stats_hunt.png", width = 11, height=7)

