# Charger les bibliothèques nécessaires
library(lpirfs)  # Package pour projections locales
library(ggplot2) # Pour la visualisation



# Charger les données à partir de la source (remplacez avec vos données)
url <- "https://github.com/AdaemmerP/lpirfs/raw/master/data/ag_data.RData"
download.file(url, destfile = "ag_data.RData", mode = "wb")

#ou
load("ag_data.RData")
# Exemple : Remplacez la ligne ci-dessous avec votre étape de chargement des données
# ag_data <- read.csv("chemin_vers_vos_données.csv")

# Simuler des données pour la démonstration
ag_data <- ag_data
set.seed(123)
ag_data <- matrix(rnorm(200 * 5), ncol = 5)

# Définir la plage d'échantillons
sample_start <- 8
sample_end <- nrow(ag_data)



# Variables endogènes
endog_data <- ag_data[sample_start:sample_end, 3:5]

# Variable de choc
shock <- endog_data[, 1]



# Estimer le modèle avec une constante et 4 retards

# Convertir endog_data en data.frame
endog_data <- as.data.frame(ag_data[sample_start:sample_end, ])

# Assurez-vous que les colonnes sont correctement nommées
colnames(endog_data) <- c("GDP", "Gov")  # Modifiez selon vos colonnes réelles

# Estimation initiale des réponses impulsionnelles
shock <- endog_data$Gov
results_lin_iv <- lp_lin_iv(
  endog_data = endog_data,  # Correction ici : un data.frame est utilisé
  shock = shock,
  lags_endog_lin = 4,
  trend = 0,
  confint = 1.96,
  hor = 20)


shock <- data.frame(shock = endog_data$Gov)

results_lin_iv <- lp_lin_iv(
    endog_data = endog_data,
    shock = shock,
    lags_endog_lin = 4,
    trend = 0,
    confint = 1.96,
    hor = 20
)

#Visualiser les résultats
# Générer les graphiques des réponses impulsionnelles
iv_lin_plots <- plot_lin(results_lin_iv)

# Afficher la première réponse
iv_lin_plots[[1]]

# (Optionnel) Enregistrer le graphique
ggsave("reponse_impulsionnelle_plot1.png", iv_lin_plots[[1]])

GYsc            <- mean(exp(endog_data$GDP)/exp(endog_data$Gov))

multiplier_mean <- results_lin_iv$irf_lin_mean*GYsc
multiplier_up   <- results_lin_iv$irf_lin_up*GYsc
multiplier_low  <- results_lin_iv$irf_lin_low*GYsc

results_lin_iv  <- list(irf_lin_mean  = multiplier_mean,
                        irf_lin_up    = multiplier_up,
                        irf_lin_low   = multiplier_low,
                        specs         = results_lin_iv$specs)

# Make new plots 
 iv_lin_plots   <- plot_lin(results_lin_iv)
 iv_lin_plots[[3]]
ggsave("reponse_impulsionnelle_plot2.png", iv_lin_plots[[3]])


