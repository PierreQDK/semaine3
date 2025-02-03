#---------- PACKAGES ----------

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(styler)



#---------- QUESTION 1 ----------



#---------- QUESTION 2 ----------

#----- Importer la base -----
file_path <- "/Users/pierrequintindekercadio/Desktop/MASTER ECAP 2024-2025/R avancé/cours2401/elus-conseillers-municipaux-cm.csv"

# Mise en page de la base de données
data_exercice<- read.table(
  file_path,
  header = TRUE,
  sep = ";",
  fill = TRUE,
  quote = ""
)

#View(data_exercice)


#----- Modifier les formats (lorsque nécessaire) -----

str(data_exercice)

data_exercice$Date.de.naissance <- as.Date(data_exercice$Date.de.naissance,
                                           tryFormats = "%d/%m/%Y")



#----- Créer les dataframe -----

df_Nantes <- data_exercice |>
  filter(Libellé.de.la.commune == "Nantes")

df_Faverelles <- data_exercice |>
  filter(Libellé.de.la.commune == "Faverelles")

df_Loire_Atlantique <- data_exercice |>
  filter(Libellé.du.département == "Loire-Atlantique")

df_Gers <- data_exercice |>
  filter(Libellé.du.département == "Gers")

df_list <- list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers)



#---------- QUESTION 3 ----------

#----- Vérifier le dataframe -----

## Pour la suite, les fonctions prendront en entrée des dataframe. Pour être
## certains qu'ils ont la bonne forme, on crée une fonction qui vérifie cela et
## qui, dans le cas où le df serait mal construit, retourne une erreur pour nous
## avertir.

validate_schema <- function(df) {
  schema <- c(
    "Code.du.département",
    "Libellé.du.département",
    "Code.de.la.collectivité.à.statut.particulier",
    "Libellé.de.la.collectivité.à.statut.particulier",
    "Code.de.la.commune",
    "Libellé.de.la.commune",
    "Nom.de.l.élu",
    "Prénom.de.l.élu",
    "Code.sexe",
    "Date.de.naissance",
    "Code.de.la.catégorie.socio.professionnelle",
    "Libellé.de.la.catégorie.socio.professionnelle",
    "Date.de.début.du.mandat",
    "Libellé.de.la.fonction",
    "Date.de.début.de.la.fonction",
    "Code.nationalité"
  )
  stopifnot(identical(colnames(df), schema))
}


## Exemples :
##    - Valide
##        validate_schema(df_Gers)
##    - Non valide
##        validate_schema(df_Gers[,-1])



#----- Créer la fonction -----

#--- Code Djayan ---

compter_nombre_d_elus <- function(df){
  
  # Créer une nouvelle colonne combinant les trois variables
  
  df$triplet <- paste(df$Nom.de.l.élu, df$Prénom.de.l.élu, df$Date.de.naissance)
  
  # Vérifier si le triplet est unique (c'est-à-dire sans doublons)
  
  unique_triplets <- df[!duplicated(df$triplet), ]
  
  # Compter le nombre de triplets uniques
  
  count = nrow(unique_triplets)
  
  return(count)
}

compter_nombre_d_elus(df_Nantes)



#--- Code Corrigé ---

compter_nombre_d_elus_corr <- function(df) {
  # Vérifier le schéma
  
  validate_schema(df)
  
  
  # Appliquer les transformations
  
  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}


sapply(df_list, compter_nombre_d_elus_corr)





#---------- QUESTION 4 ----------

#--- Code Djayan ---

compter_nombre_d_adjoints <- function(df) {
  
  count <- grepl("adjoint", df$Libellé.de.la.fonction, ignore.case = TRUE) |>
    sum()
  
  return(count)
  
}


lapply(df_list, compter_nombre_d_adjoints)



#--- Code Corrigé ---

compter_nombre_d_adjoints_corr <- function(df) {
  
  validate_schema(df)
  
  df$Libellé.de.la.fonction |>
    str_detect("adjoint") |>
    sum()
  
}

sapply(df_list, compter_nombre_d_adjoints_corr)





#---------- QUESTION 5 ----------


#--- Code Djayan ---

trouver_l_elu_le_plus_age_1 <- function(df) {
  
  # Trier le dataframe par date de naissance (plus ancien en premier)
  df <- df[order(df$Date.de.naissance), ]
  
  # Sélectionner l'élu le plus âgé (première ligne après tri)
  elu_le_plus_age <- df[1, ]
  
  # Calculer son âge
  age <- as.period(interval(elu_le_plus_age$Date.de.naissance, today()),
                   unit = "years")$year
  
  # Retourner les informations
  return(cat("L'élu le plus âgé est :",
             elu_le_plus_age$Nom.de.l.élu,
             elu_le_plus_age$Prénom.de.l.élu,
             "avec un âge de", age, "ans.\n"))
}

lapply(df_list, trouver_l_elu_le_plus_age_1)



#--- Code Djayan 2 ---

trouver_l_elu_le_plus_age_2 <- function(df) {
  
  validate_schema(df)
  
  elu_le_plus_age <- df |>
    arrange(Date.de.naissance) |>  # Trier les données par date de naissance
    slice_head(n = 1) |>            # Sélectionner la première ligne
    mutate(age = as.period(interval(Date.de.naissance, today()), # Calculer l'âge
                           unit = "years")$year)
  
  return(paste("L'élu le plus âgé est ",
               elu_le_plus_age$Nom.de.l.élu,
               elu_le_plus_age$Prénom.de.l.élu,
               "avec un âge de", round(elu_le_plus_age$age), "ans."))
  
}

sapply(df_list, trouver_l_elu_le_plus_age_2)





#---------- QUESTION 6 ----------

calcul_distribution_age <- function(df) {
  
  #validate_schema(df) retirer la vérification car sinon pose problème pour la suite (Q9)
  
  # Calcul des âges en années
  ages <- as.period(interval(df$Date.de.naissance, today()),
                    unit = "years")$year
  
  # Calcul des quantiles
  quantiles <- quantile(ages, probs = c(0, 0.25, 0.5, 0.75, 1))
  
  return(quantiles)
}

sapply(df_list, calcul_distribution_age)





#---------- QUESTION 7 ----------

plot_code_professions <- function(df){
  
  validate_schema(df)
  
  # Compter le nombre d'élus par code professionnel et filtrer ceux à 0
  
  count_professions <- df |>
    count(Code.de.la.catégorie.socio.professionnelle) |>
    filter(n > 0) |>
    arrange(desc(n))
  
  # Générer le bar chart horizontal
  
  ggplot(count_professions, aes(x = n, y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "darkblue") +
    labs(title = "Nombre d'élus par code professionnel",
         x = "Nombre d'élus",
         y = "Code professionnel") +
    theme_minimal()
  
}

lapply(df_list, plot_code_professions) |>
  invisible()   # évite l'affichage de [[1]], [[2]], etc.





#---------- QUESTION 8 ----------

#--- Créer une fonction qui ajoute la classe "commune" si elle n'est pas déjà présente ---

assign_commune_class <- function(df) {
  if (!inherits(df, "commune")) {
    class(df) <- c("commune", class(df))
  }
  return(df)
}


#--- Appliquer la fonction à df_Nantes et df_Faverelles ---

df_Nantes <- assign_commune_class(df_Nantes)
df_Faverelles <- assign_commune_class(df_Faverelles)



#--- Créer la fonction summary.commune ---

summary.commune <- function(x){
  
  validate_schema(x)
  
  # Vérifier que x est bien un objet de type "commune"
  
  if (!inherits(x, "commune")) {
    stop("L'objet doit être de classe 'commune'")
  }
  
  # Énoncer les informations
  
  cat(
    "Nom de la commune :", unique(x$Libellé.de.la.commune), "\n",
    "Nombre d'élus :", compter_nombre_d_elus(x), "\n",
    "Distribution des âges des élus :", paste(calcul_distribution_age(x), collapse = " - "), "\n",
    "Élu le plus âgé :", trouver_l_elu_le_plus_age_2(x), "\n"
  )
}

lapply(list(df_Nantes, df_Faverelles), summary.commune) |>
  invisible()





#---------- QUESTION 9 ----------

#--- Créer une fonction qui ajoute la classe "departement" si elle n'est pas déjà présente ---

assign_departement_class <- function(df) {
  if (!inherits(df, "departement")) {
    class(df) <- c("departement", class(df))
  }
  return(df)
}



#--- Appliquer la fonction à df_Loire_Atlantique et df_Gers ---

df_Loire_Atlantique <- assign_departement_class(df_Loire_Atlantique)
df_Gers <- assign_departement_class(df_Gers)



#--- Adapter la fonction pour les âges extrêmes ---

trouver_elus_extremes_age <- function(df) {
  
  validate_schema(df)
  
  # Trier les élus par date de naissance (du plus vieux au plus jeune)
  df_sorted <- df |> arrange(Date.de.naissance)
  
  # Trouver l'élu le plus âgé (première ligne)
  elu_le_plus_age <- df_sorted |> slice_head(n = 1) |>
    mutate(age = as.period(interval(Date.de.naissance, today()), unit = "years")$year)
  
  # Trouver l'élu le plus jeune (dernière ligne)
  elu_le_plus_jeune <- df_sorted |> slice_tail(n = 1) |>
    mutate(age = as.period(interval(Date.de.naissance, today()), unit = "years")$year)
  
  # Construire les résultats
  elu_plus_age_txt <- paste(
    "L'élu le plus âgé est",
    elu_le_plus_age$Nom.de.l.élu, elu_le_plus_age$Prénom.de.l.élu,
    "avec un âge de", round(elu_le_plus_age$age), "ans."
  )
  
  elu_plus_jeune_txt <- paste(
    "L'élu le plus jeune est",
    elu_le_plus_jeune$Nom.de.l.élu, elu_le_plus_jeune$Prénom.de.l.élu,
    "avec un âge de", round(elu_le_plus_jeune$age), "ans."
  )
  
  return(list(elu_plus_age = elu_plus_age_txt, elu_plus_jeune = elu_plus_jeune_txt))
}


#--- Adapter la fonction de la distribution des âges ---

trouver_communes_extremes_age <- function(df) {
  
  validate_schema(df)
  
  # Calculer l'âge des élus
  df <- df |> mutate(age = as.period(interval(Date.de.naissance, today()), unit = "years")$year)
  
  # Calculer la moyenne d'âge par commune
  moyennes_ages <- df |>
    group_by(Libellé.de.la.commune) |>
    summarise(moyenne_age = mean(age, na.rm = TRUE), .groups = "drop")
  
  # Trouver les communes avec la moyenne d'âge la plus faible et la plus élevée
  commune_plus_jeune <- moyennes_ages |> slice_min(moyenne_age, n = 1) |> pull(Libellé.de.la.commune)
  commune_plus_agee <- moyennes_ages |> slice_max(moyenne_age, n = 1) |> pull(Libellé.de.la.commune)
  
  # Calculer la distribution des âges pour ces communes
  distribution_plus_jeune <- calcul_distribution_age(df |> filter(Libellé.de.la.commune == commune_plus_jeune))
  distribution_plus_agee <- calcul_distribution_age(df |> filter(Libellé.de.la.commune == commune_plus_agee))
  
  return(list(
    commune_plus_jeune = list(
      nom = commune_plus_jeune,
      distribution_ages = distribution_plus_jeune
    ),
    commune_plus_agee = list(
      nom = commune_plus_agee,
      distribution_ages = distribution_plus_agee
    )
  ))
}


#--- Créer la fonction summary.commune ---

summary.departement <- function(x) {
  
  validate_schema(x)
  
  # Vérifier que x est bien un objet de type "departement"
  if (!inherits(x, "departement")) {
    stop("L'objet doit être de classe 'departement'")
  }
  
  # Obtenir les résultats des communes avec âge moyen extrême
  communes_extremes <- trouver_communes_extremes_age(x)
  
  # Extraire et formater les résultats
  commune_plus_agee_txt <- paste(
    "Commune :", communes_extremes$commune_plus_agee$nom,
    "| Distribution des âges :", paste(communes_extremes$commune_plus_agee$distribution_ages, collapse = " - ")
  )
  
  commune_plus_jeune_txt <- paste(
    "Commune :", communes_extremes$commune_plus_jeune$nom,
    "| Distribution des âges :", paste(communes_extremes$commune_plus_jeune$distribution_ages, collapse = " - ")
  )
  
  # Énoncer les informations
  cat(
    "Nom du département :", unique(x$Libellé.du.département), "\n",
    "Nombre d'élus :", compter_nombre_d_elus(x), "\n",
    "Élu le plus âgé :", trouver_elus_extremes_age(x)[[1]], "\n",
    "Élu le plus jeune :", trouver_elus_extremes_age(x)[[2]], "\n",
    "Commune avec l'âge moyen le plus élevé :", commune_plus_agee_txt, "\n",
    "Commune avec l'âge moyen le plus faible :", commune_plus_jeune_txt, "\n"
  )
}

lapply(list(df_Gers, df_Loire_Atlantique), summary.departement) |>
  invisible()





#---------- QUESTION 10 ----------

plot.commune <- function(df) {
  
  validate_schema(df)
  
  
  # Extraire le nom de la commune et du département
  
  nom_commune <- unique(df$Libellé.de.la.commune)
  nom_departement <- unique(df$Libellé.du.département)
  
  
  # Compter le nombre d'élus par code professionnel et filtrer ceux à 0
  
  count_professions <- df |>
    count(Code.de.la.catégorie.socio.professionnelle) |>
    filter(n > 0) |>
    arrange(desc(n))
  
  
  # Construire le titre et l'axe des abscisses
  
  titre_graphique <- paste(nom_commune, "-", nom_departement)
  axe_x <- paste("Libellés des codes professionnels pour les", sum(count_professions$n), "élus")
  
  
  # Générer le bar chart horizontal
  
  ggplot(count_professions, aes(x = n, y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "darkblue") +
    labs(title = titre_graphique,
         x = axe_x,
         y = "Code professionnel") +
    theme_minimal()
}

lapply(list(df_Nantes, df_Faverelles), plot.commune)





#---------- QUESTION 11 ----------

plot.departement <- function(df) {
  
  validate_schema(df)
  
  # Extraire le nom du département
  
  nom_departement <- unique(df$Libellé.du.département)
  
  
  # Calculer le nombre de communes distinctes dans le département
  
  nb_communes <- df |>
    distinct(Libellé.de.la.commune) |>
    nrow()
  
  
  # Compter le nombre d'élus par code professionnel
  
  count_professions <- df |>
    count(Code.de.la.catégorie.socio.professionnelle) |>
    filter(n > 0) |>
    arrange(desc(n)) |>
    slice_head(n = 10)  # Sélectionner les 10 codes professionnels les plus représentés
  
  
  # Construire le titre et l'axe des abscisses
  
  titre_graphique <- paste(nom_departement, "-", nb_communes, "communes")
  axe_x <- paste("Libellés des 10 codes professionnels les plus représentés pour", nom_departement)
  
  
  # Générer le bar chart horizontal
  
  ggplot(count_professions, aes(x = n, y = reorder(Code.de.la.catégorie.socio.professionnelle, n))) +
    geom_bar(stat = "identity", fill = "darkblue") +
    labs(title = titre_graphique,
         x = axe_x,
         y = "Code professionnel") +
    theme_minimal()
}

lapply(list(df_Loire_Atlantique, df_Gers), plot.departement)
