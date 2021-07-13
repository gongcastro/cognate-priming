# Cognate Priming

Phonological priming paradigm in Spanish-English and Spanish-Catalan bilingual toddlers, controlling the cognate status of the prime. This repository contains:

* **Data**: Raw gaze data from Oxford and Barcelona labs
* **Documents**: Notes, questionnaires, and templates
* **Julia**: Julia scripts for frequentest multilevel models
* **R**: R scripts for processing and analysing data, and for generating graphs
* **Rmd**: reproducible reports and manuscripts
* **Stimuli**: audio files, images presented to participants, and trial lists

To reproduce the data processing and analysis pipeline you may need to clone this repository to your local machine and set up some packages. If you are unfamiliar with Git, you can download this repository two ways by clicking the green "Code" button and then clicking "Download ZIP". Unzip the downloaded file and will be ready!

Then you should install and set up the package [renv](https://rstudio.github.io/renv/articles/renv.html). By running `renv::init()`, you will create a new environment in which to run the scripts, which includes all the packages needed in the same versions we used. This may require installing several packages, which can take a while.

```r
install.packages("renv") # in case you have not installed it previously
renv::init()
```

Finally, you will need to access some password-protected data. This password is not hard-coded in the scripts, but rather saved locally in the authors' machines. We suggest you to get in touch [gonzalo.garciadecastro@upf.edu](mailto:gonzalo.garciadecastro@upf.edu) to obtain the password, and then use the [keyring](https://github.com/r-lib/keyring) package to save it securely in your local machine. Once you have obtained the password, run the following code:

```r
keyring(user = "gonzalo.garciadecastro@upf.edu")
```

Then, enter the password in the dialogue box that will pop up in you RStudio Session. You will (should) now be able to run the all the scripts without interruptions!
