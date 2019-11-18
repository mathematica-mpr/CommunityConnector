FROM rocker/tidyverse:3.6.1 as base

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libgit2-dev \
    libpng-dev \
    libudunits2-dev \
    libgdal-dev

RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'forcats', 'ggplot2', 'shinytest', 'yaml', 'shinyWidgets', 'Rcpp', 'viridis', 'sf', 'leaflet', 'aws.s3', 'aws.ec2metadata'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('git2r', type='source', configure.vars='autobrew=yes')"
RUN R -e "devtools::install_github('rstudio/renv')"
RUN R -e "devtools::install_version('plotly', version = '4.9.0', repos='http://cran.rstudio.com/')"
#RUN R -e "shinytest::installDependencies()"





EXPOSE 8787