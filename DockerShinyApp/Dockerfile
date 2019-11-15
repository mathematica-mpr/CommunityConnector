FROM rocker/rstudio:3.6.1 as base

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libgit2-dev \
    libpng-dev \
    libudunits2-dev \
    libgdal-dev

RUN R -e "install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'devtools', 'DT', 'forcats', 'ggplot2', 'plotly', 'shinytest'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('yaml', 'shinyWidgets'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('Rcpp'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('fmsb'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('git2r', type='source', configure.vars='autobrew=yes')"
RUN R -e "install.packages(c('devtools'), repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('rstudio/renv')"
RUN R -e "devtools::install_github('rstudio/r2d3')"
RUN R -e "install.packages(c('DT'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinytest'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('maps'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('usmap'), repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_version('plotly', version = '4.9.0', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('viridis'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('sf'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('leaflet'), repos='http://cran.rstudio.com/')"
#RUN R -e "shinytest::installDependencies()"





EXPOSE 8787