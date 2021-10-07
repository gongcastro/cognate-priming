# Cognate Priming [[report](https://github.com/bilingual-project/cognate-priming/blob/single/Rmd/report.md)]

Phonological priming paradigm in Spanish-English and Spanish-Catalan bilingual toddlers, controlling the cognate status of the prime. This repository contains the following sub-folders:

* **Data**: Raw gaze data from Oxford and Barcelona labs
* **Documents**: Notes, questionnaires, and templates
* **Matlab**: Matlab code used to present stimuli and take eye-tracking samples in the experiment.
* **R**: R scripts for processing and analysing data, and for generating graphs
* **Rmd**: reproducible reports and manuscripts
* **Stimuli**: audio files, images presented to participants, and trial lists


# How to set up this repository locally

*NOTE*: We recommend using RStudio (as opposed to base R, the console, and any other IDE). 

1) Download and install [Git](https://git-scm.com/downloads) with default settings.
2) Clone this repository locally. You can do this in two ways:

* Using your console, navigate to the folder you want to download the repository into, and run:

```console
git clone https://github.com/bilingual-project/cognate-priming.git
```
If the repository is private at the time you try to clone it, you may have to enter your GitHub credentials)

* Clicking the green button "Code" in this page (upper-right corner), clicking "Download ZIP", and unzipping the downloaded repository.

3) Download the Barcelona gaze data CSV files from this [Google Drive folder](https://drive.google.com/drive/folders/1SHiZlR2kM3RlwkngCqhCSAtZbL10JicN?usp=sharing), and move the files under Data/Gaze/Barcelona.
4) Download the Oxford gaze data CSV files from this [Google Drive folder](https://drive.google.com/drive/folders/1SHiZlR2kM3RlwkngCqhCSAtZbL10JicN?usp=sharing), and move the files under Data/Gaze/Oxford.
5) Open and R session.
6) Install the [renv](https://rstudio.github.io/renv/articles/renv.html) R package running `install.packages("renv")`.
7) Install the [targets](https://books.ropensci.org/targets/) package running  `install.packages("targets")`.
8) Run `renv::restore()`. This will create an environment for the project that contains the necessary R packages (and package dependencies) you need, in version you need. 
9) Run `targets::tar_make()`. This command will run the scripts in the appropriate order (as specified in the [_targets.R](https://github.com/bilingual-project/cognate-priming/blob/master/_targets.R) script) and create the objects, which will be stored in the [targets/](https://github.com/bilingual-project/cognate-priming/tree/master/_targets). The final output of this command will be the rendering of the [Rmd/report.Rmd](https://github.com/bilingual-project/cognate-priming/blob/master/Rmd/report.Rmd) file into and HTML file (Rmd/report.html) with the laboratory notes and all the information about the study.

If you want to inspect specific objects into the Global Environment of your R session, you can run `tar_load()`, specifying the object you want to import (e.g., `tar_load(model_fits)`). You can also visualise the dependencies across files and scripts running `tar_visnetwork()`.


# Password-protected data

Some data (in the multilex package) are password protected. This password is not hard-coded in the scripts, but rather saved locally in the authors' machines. We suggest you to get in touch [gonzalo.garciadecastro@upf.edu](mailto:gonzalo.garciadecastro@upf.edu) to obtain the password, and then use the [keyring](https://github.com/r-lib/keyring) package to save it securely in your local machine. Once you have obtained the password, run the following code:

```r
keyring::keyring(user = "gonzalo.garciadecastro@upf.edu")
```

Then, enter the password in the dialogue box that will pop up in you RStudio Session. You will (should) now be able to run the all the scripts without interruptions!

# Troubleshooting

When running `r renv::restore()` for the first time, you may encounter an error indicating that the package [multilex](https://github.com/gongcastro/multilex) is not available. To fix this:

1) If you are using Ubuntu (as opposed to Windows or Mac), you may have to run the following lines in your (bash) console first, in order to be able to install some R necessary R packages later (e.g., [openssl](https://github.com/jeroen/openssl), [Cairo](https://github.com/s-u/Cairo)).

```console
sudo apt-get install libssl-dev
sudo apt-get install libxt-dev
sudo apt-get install libmysqlclient-dev
sudo apt-get install libsodium-dev
sudo apt-get install libcairo2-dev
sudo apt-get update
```

Finally you may have to install the [childesr](https://github.com/langcog/childesr), [formr](https://github.com/rubenarslan/formr), and [multilex](https://github.com/gongcastro/multilex) packages manually from your R console. for some reason, renv assumes that these packages are available in CRAN (formr and multilex are not), and therefore `renv::restore()` may fail to install them.

```r
renv::install("lancog/childesr", "rubenarslan/formr", "gongcastro/multilex")
```

This may take a while, but hopefully will fix the issue. Please get in contact in case you need any help setting up this repository. We did our best facilitating others' use of our code, but our familiarity with the tools above mentioned is limited.

