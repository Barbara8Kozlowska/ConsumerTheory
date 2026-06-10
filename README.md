# ConsumerTheory
PL:
ConsumerTheory to pakiet języka R, który udostępnia narzędzia obliczeniowe do rozwiązywania i analizowania problemów związanych z zagadnieniem Wyboru Konsumenta. Pakiet łączy w sobie funkcje języka R z implementacjami napisanymi w języku C.

Głównym celem projektu jest umożliwienie użytkownikom łatwego obliczania i analizowania pojęć znanych z Teorii Konsumenta, omawianej podczas kursu mikroekonomii.

## Funkcje

Pakiet obsługuje obecnie:

* Sprawdzanie obojętności koszyków dóbr
* Sprawdzanie zgodności budżetowej koszyka
* Obliczanie wskaźników cen (Laspeyresa i Paasche'a)
* Obliczanie wskaźników ilościowych (Laspeyresa i Paasche'a)
* Obliczanie wskaźnika całkowitej zmiany wartości
* Optymalizację funkcji popytu Marshalla
* Obliczanie użyteczności krańcowej
* Obliczanie krańcowej stopy substytucji (MRS)
* Weryfikację pierwszego i drugiego prawa Gossena
* Obliczanie elastyczności użyteczności
* Obliczanie względnej elastyczności substytucji użyteczności
* Obliczanie płacy rezerwowej
* Klasyfikacja dóbr
* Optymalizacja funkcji popytu Hicksa
* Obliczanie hicksowskiej elastyczności cenowej popytu

W przyszłości chciałbym dodać:

* Obliczanie elastyczności pojedynczej i mieszanej popytu i ceny
* Model wyborów międzyokresowych i związane z nim prawa
* Funkcje makroekonomiczne 

## Instalacja

Pakiet można zainstalować bezpośrednio z serwisu GitHub.

```r
install.packages(„remotes”)

remotes::install_github(„Barbara8Kozlowska/ConsumerTheory”)
```
Po instalacji załaduj pakiet:

```r
library(ConsumerTheory)
```

I (mam nadzieję) baw się dobrze podczas użytkowania!


ENG:
ConsumerTheory is an R package that provides computational tools for solving and analysing consumer choice problems. The package combines R functions with implementations written in C.

The main objective of the project is to allow user to easily compute and analyse concepts known from microeconomics consumer theory.

## Features

The package currently supports:

* Checking indifference of commodity baskets
* Checking budget concurrency of a basket
* Computing price indexes (Laspeyres and Paasche)
* Computing quantity indexes (Laspeyres and Paasche)
* Computing total value changes index
* Marshall's demand function optimization
* Computing marginal utility
* Computing marginal rate of substitution (MRS)
* Verifying first and second Gossen's laws
* Computing elasticity of utility
* Computing relative elasticity of utility substitution
* Computing reservation wage
* Classifying good
* Hick's demand function optimization
* Computing Hicksian Price Elasticity of Demand

In the future I'd like to add:

* Computing single and mixed flexibilities of demand and price
* Intertemporal choice model and laws regarding it
* Macroeconomics functions 

## Installation

You can install package directly from GitHub.

```r
install.packages("remotes")

remotes::install_github("Barbara8Kozlowska/ConsumerTheory")
```
After installation, load the package:

```r
library(ConsumerTheory)
```

And (hopefully) enjoy!
