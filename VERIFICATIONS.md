# Vérifications de livraison — ldaR6 1.0.0

**Auteur :** MAMPOUYA CLARK Chancy Loïc Franlly  
**Projet individuel — Master 2 SISE, Université Lumière Lyon 2**  
**Dernière mise à jour :** 13 novembre 2026

Le sujet prévoit un travail en équipe. L’acceptation d’une remise individuelle reste à confirmer avec l’enseignant.

## Corrections apportées

- Rapport LaTeX et PDF actualisés avec le nom de l’auteur et le logo fourni.
- Les variables illustratives sont exclues des prédicteurs lors de l’ajustement dans Shiny.
- L’extension des fichiers Excel est conservée lors de leur lecture dans le parcours de prédiction.
- Les mesures de temps distinguent l’ajustement du modèle et la prédiction.

## Vérifications effectuées

| Vérification | Résultat |
| --- | --- |
| Construction et installation de l’archive source sous R 4.4.2 | Réussies |
| `R CMD check --no-manual` sur l’archive source | `Status: OK` |
| Contrôle séparé avec `devtools::check()` | 0 erreur, 0 avertissement et 1 note : R n’a pas pu vérifier l’heure actuelle |
| Installation directe depuis [GitHub](https://github.com/clark94/ldaR6) | Réussie avec `remotes::install_github("clark94/ldaR6")` |
| Lancement local de Shiny après installation depuis GitHub | Réussi avec `ldaR6::run_app()` |
| Prédiction par fichier CSV dans Shiny | Testée en local |

La [démonstration Shiny]**[Launch the Shiny application on Posit Connect Cloud](https://01a1d27f-1653-cd6f-e042-b2aaab58b216.share.connect.posit.cloud/)** est publiée sur Posit Connect Cloud. Les vérifications locales ci-dessus ne constituent pas une recette complète de la version hébergée.

## Mesure locale de performance

Sur des données simulées à 10 variables, un essai a donné **0,45 s** pour ajuster le modèle sur **100 000 observations** et **0,01 s** pour en prédire **10 000**. Ces temps concernent le calcul dans R, hors import de fichier et affichage Shiny. Les comparaisons historiques avec `MASS::lda` sont conservées séparément et n’ont pas été reproduites sur cette révision.

## Reproduire les vérifications

Depuis le dossier parent de `ldaR6/`, avec `R` accessible dans le terminal :

```sh
R CMD build ldaR6
R CMD check --no-manual ldaR6_1.0.0.tar.gz
R CMD INSTALL ldaR6_1.0.0.tar.gz
```

Pour vérifier l’installation depuis GitHub dans une nouvelle session R :

```r
remotes::install_github("clark94/ldaR6", upgrade = "never", force = TRUE)
ldaR6::run_app()
```

Pour relancer le benchmark, exécuter `Rscript benchmarks/benchmark.R` depuis la racine du package. Commencer avec des volumes modestes et adapter les tailles à la mémoire disponible.

## Rapport

Le rapport en français et ses sources LaTeX se trouvent dans `report/`. Pour compiler le rapport, exécuter `pdflatex rapport.tex` deux fois depuis ce dossier.
