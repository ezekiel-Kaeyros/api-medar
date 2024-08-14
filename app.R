library(plumber)

r <- plumb("categorisation_system.R")

# Démarrer le serveur sur le port 8000 (ou tout autre port de votre choix)
r$run(host = "0.0.0.0",port = 8000)
