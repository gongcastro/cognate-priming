# select image
FROM rocker/rstudio:4.1.0

ARG GH_PAT

# install system dependencies
RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y git
RUN apt-get install -y gnupg
RUN apt-get install -y software-properties-common
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key C99B11DEB97541F0
RUN apt-add-repository https://cli.github.com/packages
RUN apt update
RUN apt install gh
RUN apt-get install -y libxt-dev
RUN apt-get install -y libmysqlclient-dev
RUN apt-get install -y libsodium-dev
RUN apt-get install -y libcairo2-dev
RUN apt-get update

# authenticate GitHub (PAT is passed as secret from docke-compose.yaml)
# clone GitHub repository
RUN cd /home/
RUN git config --global url."https://'${GH_PAT}'':@github.com/".insteadOf "https://github.com/"
RUN git clone https://github.com/bilingual-project/cognate-priming.git
RUN cd /home/cognate-priming

# copy data
COPY ./Data/* /home/cognate-priming/

# install R dependencies from RStudio package manager (2021-08-30 version)
RUN R -e "install.packages(c('renv', 'targets'), repos = 'https://packagemanager.rstudio.com/cran/__linux__/focal/2021-08-30')"
RUN R -e "renv::install('lancog/childesr', 'rubenarslan/formr', 'gongcastro/multilex')"
RUN R -e "renv::restore()"

# add keyring 
RUN R -e "keyring::keyring_create()"
RUN R -e "keyring::key_set_with_value('multilex', '${ML_USER}', '${ML_PASSWORD}')"

# expose port
EXPOSE 8787

# run custom function run() to update targets
CMD R -e "run()"
