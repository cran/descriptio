# **descriptio** <img src="man/figures/descriptio.png" height=140px width=120px alt="" align="right" />

<br>

## Descriptive Statistical Analysis

<!-- badges: start -->
[![R-CMD-check](https://framagit.org/nicolas-robette/descriptio/badges/master/pipeline.svg?key_text=R+CMD+check&key_width=90)](https://framagit.org/nicolas-robette/descriptio/-/commits/master)
[![](https://img.shields.io/gitlab/last-commit/nicolas-robette%2Fdescriptio?gitlab_url=https%3A%2F%2Fframagit.org)](https://img.shields.io/gitlab/last-commit/nicolas-robette%2Fdescriptio?gitlab_url=https%3A%2F%2Fframagit.org)
[![](https://www.r-pkg.org/badges/version/descriptio?color=blue)](https://cran.r-project.org/package=descriptio)
[![](https://www.r-pkg.org/badges/last-release/descriptio?color=blue)](https://cran.r-project.org/package=descriptio)
[![](https://img.shields.io/badge/DOI-10.32614/CRAN.package.descriptio-1f57b6?style=flat&link=https://doi.org/10.32614/CRAN.package.descriptio)](https://doi.org/10.32614/CRAN.package.descriptio)
[![](http://cranlogs.r-pkg.org/badges/last-month/descriptio?color=orange)](https://cran.r-project.org/package=descriptio)
[![](http://cranlogs.r-pkg.org/badges/grand-total/descriptio?color=orange)](https://cran.r-project.org/package=descriptio)
<!-- badges: end -->

[`descriptio`](https://nicolas-robette.github.io/descriptio/) provides functions for the description of statistical associations between two variables :

* measures of local and global association between variables (phi, Cramer's V, point-biserial correlation, eta-squared, Goodman & Kruskal tau, PEM, etc.)
* summaries of bivariate associations among a set of variables
* graphical representations of the associations between two variables (using `ggplot2`)
* weighted statistics
* permutation tests


## Documentation

Please visit [https://nicolas-robette.frama.io/descriptio/](https://nicolas-robette.frama.io/descriptio/) for documentation


## Installation

Execute the following code within `R`:

``` r
if (!require(devtools)){
    install.packages('devtools')
    library(devtools)
}
install_git("https://framagit.org/nicolas-robette/descriptio")
```


## Citation

To cite `descriptio` in publications, use :

Robette N. (2025), *`descriptio` : Descriptive Analysis of Associations between Variables in `R`*, version 1.4, https://nicolas-robette.frama.io/descriptio/


## References

Agresti, A. (2007). *An Introduction to Categorical Data Analysis*, 2nd ed. New York: John Wiley & Sons.

Cibois P., 1993, « Le PEM, pourcentage de l'ecart maximum : un indice de liaison entre modalites d'un tableau de contingence », *Bulletin de Methodologie Sociologique*, 40, pp 43-63, [https://ciboispagesperso.fr/bms93.pdf]

Rakotomalala R., « Comprendre la taille d'effet (effect size) », [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
