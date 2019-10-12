FROM rocker/rstudio:3.6.1 as base

RUN R -e "install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'devtools', 'DT', 'forcats', 'ggplot2', 'plotly', 'shinytest'), repos='http://cran.rstudio.com/')"

EXPOSE 8787

CMD ["/init"]
#COPY app /srv/shiny-server/

#CMD ["/usr/bin/shiny-server.sh"]




  
