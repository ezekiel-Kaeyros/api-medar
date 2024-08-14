# Charger les packages nécessaires
library(quanteda)      # Pour l'analyse textuelle
library(tidyverse)     # Pour la manipulation de données
library(stringr)       # Pour le traitement des chaînes de caractères
library(stopwords)     # Pour gérer les listes de mots vides
library(readxl)        # Pour lire des fichiers Excel
library(textstem)      # Pour la lemmatisation des mots
library(plumber)       # Pour créer des API en R
library(mongolite)     # Pour interagir avec MongoDB
library(dplyr)         # Pour la manipulation de données
library(jose)          # Pour le décodage des tokens JWT

#### Lecture de la base des catégories via un fichier Excel local ####
file_path <- "./data/categorisation_medar.xlsx"  # Chemin du fichier Excel
categories <- read_excel(file_path)  # Chargement des catégories depuis le fichier Excel

# Fonction pour récupérer des données à partir de MongoDB
fetch_mongodb <- function(connection_string, collection, db) {
  reports <- mongo(collection = collection, db = db, url = connection_string)  # Connexion à la collection MongoDB
  reports <- as.data.frame(reports$find(field = '{}'))  # Récupération des données et conversion en DataFrame
  return(reports)
}

# Définition du chemin de base pour les données
root <- getwd()  # Récupération du répertoire de travail actuel
path_data <- paste(root, "/", "app/data/", sep = "")  # Construction du chemin complet pour les données

# Chaîne de connexion à la base de données MongoDB
connection_string <- "mongodb+srv://medar-user2:medar-pwd2@medar-db.7c6rlum.mongodb.net/?retryWrites=true&w=majority&appName=medar-db"

################## Clé secrète pour le JWT
key <- "Anti-D-2024"

################# Rôle d'utilisateur requis
role <- 2  # Le rôle requis pour accéder aux fonctionnalités

############## Récupération des données de rapports depuis MongoDB
data <- fetch_mongodb(connection_string = connection_string, db = "test", collection = "reports")

############## Récupération des données de connexion des utilisateurs depuis MongoDB
login_data <- fetch_mongodb(connection_string = connection_string, collection = "users", db = "test")

# Définition des stopwords à utiliser pour le prétraitement du texte
stopwords_list <- stopwords::stopwords('de', source = 'stopwords-iso')

# Fonction pour calculer la similarité cosinus entre deux matrices
cosine_similarity <- function(A, B) {
  # Calcul des produits scalaires
  AB <- A %*% t(B)

  # Calcul des normes des vecteurs
  A_norm <- sqrt(rowSums(A^2))
  B_norm <- sqrt(rowSums(B^2))

  # Calcul et renvoi de la similarité cosinus
  similarity <- AB / (A_norm %*% t(B_norm))
  return(similarity)
}

# Fonction de prétraitement pour une chaîne de caractères
preprocess_text <- function(text) {
  # Conversion du texte en minuscules
  text <- tolower(text)

  # Remplacement de la ponctuation par des espaces
  text <- gsub("[[:punct:]]", " ", text)

  # Suppression des chiffres
  text <- gsub("\\d+", "", text)

  # Tokenisation et suppression des mots vides
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[!tokens %in% stopwords_list]

  # Lemmatisation des tokens
  tokens <- lemmatize_words(tokens)

  # Recombinaison des tokens en une seule chaîne de caractères
  text <- paste(tokens, collapse = " ")

  return(text)
}

# Filtre CORS pour autoriser les requêtes depuis n'importe quelle origine
#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()  # Continuer le traitement de la requête
}

# Fonction de classification
#* @param token JWT pour l'authentification
#* @param id Identifiant du rapport
#* @get /categorization
function(id, token) {
  # Décodage du token JWT
  token_json_data <- tryCatch({
    jose::jwt_decode_hmac(token, secret = key)  # Décodage du token avec la clé secrète
  }, error = function(e) {
    return(NULL)  # Retourne NULL en cas d'erreur
  })

  # Si le décodage échoue, retourner une erreur 500
  if (is.null(token_json_data)) {
    return("Error 500 - Internal Server Error")
  }

  # Validation du token et des droits d'accès
  converted_time <- as.POSIXct(token_json_data$exp, origin = "1970-01-01", tz = "Africa/Lagos")
  if (token_json_data$email %in% login_data$email & token_json_data$role == role & converted_time > Sys.time()) {

    # Récupérer la description du rapport correspondant à l'identifiant fourni
    description <- data %>% filter(`_id` == id) %>% select(description)

    # Prétraitement du texte de la description
    discussion <- sapply(description, preprocess_text)

    # Création du corpus et de la matrice document-fréquence (dfm) pour la nouvelle description
    corp_new <- corpus(discussion, docnames = paste0("new_", seq_along(discussion)))
    dfm_new <- dfm(corp_new)

    # Création de la dfm pour les catégories
    dfm_topic <- dfm(corpus(categories$Beispiele))

    # Harmonisation des vocabulaires entre les deux dfms
    dfm_new <- dfm_match(dfm_new, features = featnames(dfm_topic))

    # Conversion des DFMs en matrices denses
    dfm_new_matrix <- as.matrix(dfm_new)
    dfm_topic_matrix <- as.matrix(dfm_topic)

    # Calcul des similarités cosinus
    similarities <- cosine_similarity(dfm_new_matrix, dfm_topic_matrix)

    # Identification des outliers supérieurs au troisième quartile
    Q3_outliers <- similarities > quantile(similarities, probs = 0.75, na.rm = TRUE)

    # Compte des outliers
    num_outliers <- sum(Q3_outliers, na.rm = TRUE)

    # Si aucun outlier n'est trouvé, retourner "No match found"
    if (num_outliers == 0) {
      return("No match found")
    }

    # Sélection des indices des documents les plus similaires
    k <- num_outliers
    knn_indices <- order(similarities, decreasing = TRUE)[1:k]

    # Retourner les catégories correspondantes
    return(categories[knn_indices[1], c("Ebene 1","Ebene 2", "Ebene 3")])
  } else {
    return("Error 500 - Internal Server Error")
  }
}
