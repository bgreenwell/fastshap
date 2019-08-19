FROM rocker/binder:3.6.0
LABEL maintainer='Brandon Greenwell'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}
RUN wget https://github.com/bgreenwell/fastshap/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-08-19/')); devtools::install_deps()"
