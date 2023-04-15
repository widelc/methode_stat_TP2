
# TP2 par William Delisle, Rémi Mailhot, Laurent Vienneau et Mathieu Verville

Pour ce TP, nous avons sélectionné le projet 2 qui porte sur la gestion des risques. 

Package requis pour lance le code:
-PerformanceAnalytics
-here
-ggplot2
-mvtnorm
-copula
-MASS
-fGarch

Informations techniques du fichier remis:
-Le fichier main.r utilise les données du fichier Market.rda ainsi que plusieurs fonctions qu'on peut trouver dans le dossier "Function".

Ce projet comporte 6 parties détaillées ci-dessous:


#1 Fixation du prix d'un portefeuille d'options
-On commence par définir les paramètres initiaux de notre portefeuille composé d'options d'achat européennes.
-La fonction f_eval_portfolio vient calculer la valeur initiale de celui-ci en prenant compte du dernier prix du sous-jacent et de la dernière volatilité.

#2 Un facteur de risque et un modèle gaussien
-À l'aide de la fonction f_price_simul_univariate, on simule 10 000 scénarios du prix du sous-jacent à 5 jours d'avance. Cette fonction suppose que les rendements logarithmiques sont normalement distribués de manière iid.
-La fonction f_eval_portfolio utilise le résulat précédent pour calculer la valeur des options dépendamment du prix du sous-jacent. La volatilité implicite est toujours la même dans ce cas-ci.
-La fonction f_VaR_ES_PL vient ensuite calculer la VaR95, le ES95 et la distribution P&L de notre protefeuille en utilisant les valeurs initiales de la partie 1 ainsi que nos prix sumulés.
-Les résulats sont illustrés et savegardés (univariate_distribution.png) avec les fonctions f_plot_pdf et f_png_save.

#3 Deux facteurs de risque et modèle gaussien
-À l'aide de la fonction f_price_simul_bivariate, on simule 10 000 scénarios du prix du sous-jacent et de la valeur du VIX à 5 jours d'avance. La fonction suppose que les rendements logarithmiques du sous-jacent et du VIX sont normalement distribués de manière invariante.
-La fonction f_eval_portfolio calcule ensuite la valeur du portfolio en prenant les valeurs simulées du sous-jacent et du VIX contrairement à la partie 2.
-Comme à la partie 2, les fonctions f_VaR_ES_PL, f_plot_pdf et f_png_save sont utilisées pour calculer les valeurs de risque, illustrer les résultats et les sauvegarder.

#4 Deux facteurs de risque et modèle copule-marginal
-Pour cette partie, une copule normale et des marginales Student-t sont utilisées. On suppose une distribution student avec 10 degrés de libertés pour le sous-jacent et 5 degrés de libertés pour la volatilité. La fonction f_price_simul_copula vient simuler le tout et retourne 10 000 scénarios.
-La fonction f_eval_portfolio calcule ensuite la valeur du portfolio en prenant les valeurs simulées du sous-jacent et du VIX comme à la partie 3.
-Comme aux 2 parties précédentes, les fonctions f_VaR_ES_PL, f_plot_pdf et f_png_save sont utilisées pour calculer les valeurs de risque, illustrer les résultats et les sauvegarder.


#5 Surface de volatilité
-Pour cette partie, une surface de volatilité est utilisée pour la simulation. La fonction f_price_simul_vol simule des scénarios mais ajuste les prédictions du VIX avec les différents paramètres de la surface de volatilité (strike, time to maturity, IV et moneyness)
-Comme précédemment, les fonctions f_eval_portfolio, f_VaR_ES_PL, f_plot_pdf et f_png_save sont utilisées.

#6 Approche complète
-Pour la dernière partie, les modéles GARCH(1,1) et AR(1) sont utilisés pour filtrer les rendements logarithmiques du sous-jacent et de la volatilité respectivement. 
-La fonction f_price_simul_complete simule les 10 000 observations en utilisant des marginales normales pour les résidus et une copule normale. Les résidus sont convertis en rendements ensuite avec les deux modéles respectifs.
-Comme précédemment, les fonctions f_eval_portfolio, f_VaR_ES_PL, f_plot_pdf et f_png_save sont utilisées.


