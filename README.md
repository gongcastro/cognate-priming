# Cognate Priming

Phonological priming paradigm in Spanish-English and Spanish-Catalan bilingual toddlers, controlling the cognate status of the prime. This repository contains the following sub-folders:

* **Data**: Raw gaze data from Oxford and Barcelona labs
* **Documents**: Notes, questionnaires, and templates
* **Matlab**: Matlab code used to present stimuli and take eye-tracking samples in the experiment.
* **R**: R scripts for processing and analysing data, and for generating graphs
* **Rmd**: reproducible reports and manuscripts
* **Stimuli**: audio files, images presented to participants, and trial lists


# How to set up this repository locally

1) Download and install [Git](https://git-scm.com/downloads) with default settings.
2) Clone this repository locally. You can do this in two ways: (a) using your console, navigate to the folder you want to download the repository into, and run `git clone https://github.com/bilingual-project/cognate-priming.git` (if the repository is private at the time you try to clone it, you may have to enter your GitHub credentials), (b) clicking the green button "Code" in this page (upper-right corner), clicking "Download ZIP", and unzipping the downloaded repository.
3) Open and R session.
4) Install the [renv](https://rstudio.github.io/renv/articles/renv.html) R package running `r install.packages("renv")`.
4) Install the [targets](https://books.ropensci.org/targets/) package running  `r install.packages("targets")`.
5) Run `r renv::restore()`. This will create an environment for the project that contains the necessary R packages (and package dependencies) you need, in version you need. 
6) Run `targets::tar_make()`. This command will run the scripts in the appropriate order (as specified in the [_targets.R](https://github.com/bilingual-project/cognate-priming/blob/master/_targets.R) script) and create the objects, which will be stored in the [targets/](https://github.com/bilingual-project/cognate-priming/tree/master/_targets). The final output of this command will be the renderisation of the [Rmd/report.Rmd](https://github.com/bilingual-project/cognate-priming/blob/master/Rmd/report.Rmd) file into and HTML file (Rmd/report.html) with the laboratory notes and all the information about the study.

If you want to inspect specific objects into the Global Environment of your R session, you can run `r tar_load()`, specifying the object you want to import (e.g., `r tar_load(model_fits)`). You can also visualise the dependencies across files and scripts running `r tar_visnetwork()`.


# Password-protected data

Some data are password protected. This password is not hard-coded in the scripts, but rather saved locally in the authors' machines. We suggest you to get in touch [gonzalo.garciadecastro@upf.edu](mailto:gonzalo.garciadecastro@upf.edu) to obtain the password, and then use the [keyring](https://github.com/r-lib/keyring) package to save it securely in your local machine. Once you have obtained the password, run the following code:

```r
keyring::keyring(user = "gonzalo.garciadecastro@upf.edu")
```

Then, enter the password in the dialogue box that will pop up in you RStudio Session. You will (should) now be able to run the all the scripts without interruptions!

# Troubleshooting

When running `r renv::restore()` for the first time, you may encounter an error indicating that the package [multilex](https://github.com/gongcastro/multilex) is not available. To fix this, install it manually by running:

```r
renv::install("formr") # a dependency of multilex, only available in GitHub
renv::install("multilex")
```

This may take a while, but hopefully will fix the issue.
