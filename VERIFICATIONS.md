# Livraison corrigée — 10 novembre 2026
Auteur : MAMPOUYA CLARK Chancy Loïc Franlly. Travail individuel.

Le sujet officiel prévoit des équipes : l’acceptation d’une remise individuelle reste à confirmer avec le professeur.

## Corrections ciblées
- Auteur, logo fourni et rapport LaTeX/PDF actualisés.
- SAS n’est plus présenté comme une exigence officielle.
- Les variables illustratives sont exclues des variables actives lors du fit dans Shiny.
- Conservation de l’extension du fichier pour la lecture Excel en prédiction Shiny.
- Benchmarks enrichis : prédiction séparée, versions, CPU et mémoire disponibles.
- Liens configurés pour clark94/ldaR6. Aucun dépôt n’a été publié ni accès vérifié.

## Statut des vérifications
R n’est pas disponible dans l’environnement de correction. Aucun nouveau résultat R CMD check, testthat, Shiny ou benchmark n’est revendiqué. Les anciens chiffres sont conservés comme résultats historiques, à reproduire.

## Vérifier avant remise (depuis le dossier parent du package)
```sh
R CMD build ldaR6
R CMD INSTALL ldaR6_1.0.0.tar.gz
R CMD check --no-manual ldaR6_1.0.0.tar.gz
```
Depuis R, après installation :
```r
library(ldaR6)
testthat::test_dir("ldaR6/tests/testthat")
ldaR6::run_app()
```
Depuis la racine du package : `Rscript benchmarks/benchmark.R`.
Commencer par des scénarios modestes et adapter les tailles à la RAM disponible.
Tester Shiny : import TSV, choix cible/actives/illustratives, ajustement, prédiction individuelle, prédiction par fichier et export.

## Rapport
Dans report/ : `pdflatex rapport.tex` deux fois. Le logo est inclus dans figures/.
