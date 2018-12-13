# Application web pour réaliser des tests statistiques via Shiny

Ce programme réalisé via le package Shiny de R permet à chaque utilisateur de réaliser des tests statistiques univariés,bivariés,multivariés ainsi que des analyses spatiales à partir d'un jeu de donnée qu'il soumet à l'application.

## Bien débuter

L'application se lance via la commande "Run" disponible dans l'interface Rstudio ou en utilisant la commande suivante si R est utilisé via un terminal: shiny::runApp('/shiny_App')

### Prérequis

Afin d'utiliser l'application l'utilisateur a besoin des librairies suivantes:
-shiny -shinydashboard -FactoMineR -ade4 -factoextra -magrittr
-ggplot2 -gclus -RColorBrewer -vegan -clustsig -labdsv 
-shinycssloaders -stringr -sp -gstat -DT -colourpicker
-shinyalert -lmtest -pwr -PMCMR -ngram -spded

Il est donc nécessaire de les installer, les dépendances à ces libraries sont automatiquement installées en chargant les libraries par la suite.

Exemples:
###INSTALLATION DES PACKAGES
install.packages(c("shiny","shinydashboard","FactoMineR","ade4","factoextra","magrittr","ggplot2","gclus","RColorBrewer","vegan","clustsig","labdsv","shinycssloaders","stringr","sp","gstat","DT","colourpicker","shinyalert","lmtest","pwr","PMCMR","ngram","spdep"))

###CHARGEMENT DES LIBRARIES
x<c("shiny","shinydashboard","FactoMineR","ade4","factoextra","magrittr","ggplot2","gclus","RColorBrewer","vegan","clustsig","labdsv","shinycssloaders","stringr","sp","gstat","DT","colourpicker","shinyalert","lmtest","pwr","PMCMR","ngram","spdep")
lapply(x, require, character.only = TRUE)


## Exécuter des tests

Pour effectuer des tests, une barre laterale est disponible afin de laisser le choix a l'utilisateur concernant le test qu'il souhaite effectuer.
Chaque test, incorpore un onglet principe récapitulant le principe de chacun de ces tests.

Dans un premier temps, il est important que l'utilisateur charge son fichier d'analyse sur la page d'accueil.Ce fichier sera ensuite repris pour effectuer les analyses que l'utilisateur aura ciblée.


## Deploiement

L'application n'est pas disponible sur internet, elle est uniquement dédié à l'heure actuelle à un usage pédaogique et local.

## Coder avec

* [R](https://www.r-project.org/) - Excellent outil pour l'analyse et modélisation statistique.
* [Shiny RStudio](https://shiny.rstudio.com/) - Excellent outil pour la présentation de résultats statistiques.

## Auteurs

* **CLEMENT JOUAN**         **DEBORAH COTTAIS**
* **ARTHUR THOUVENIN** 	    **CYRIL DOURTHE**
* **HANS SCHRIEKE**	    **BENJAMIN BLAIS**
* **JULIE BLASQUIZ**	    **ROMAIN LUCK**
* **LUDWIG DUVAL**	    **GUILLAUME SOTTON**


