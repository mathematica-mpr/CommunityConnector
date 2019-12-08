FROM rocker/shiny-verse:3.6.1 AS base

RUN apt update \
  && apt install -y --no-install-recommends \
    libxml2-dev \
    libgit2-dev \
    libpng-dev \
    libudunits2-dev \
    libgdal-dev \
    curl \
    awscli \
    && R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'forcats', 'ggplot2', 'shinytest', 'yaml', 'shinyWidgets', 'Rcpp', 'viridis', 'sf', 'leaflet', 'aws.s3', 'aws.ec2metadata'), repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('git2r', type='source', configure.vars='autobrew=yes')" \
    && R -e "devtools::install_github('rstudio/renv')" \
    && R -e "devtools::install_version('plotly', version = '4.9.0', repos='http://cran.rstudio.com/')" \
#RUN R -e "shinytest::installDependencies()"
    && R -e "devtools::install_version('shinyBS', repos='http://cran.rstudio.com/')" \
    && R -e "install.packages(c('aws.signature', 'aws.ec2metadata', 'aws.s3'), repos = c(cloudyr = 'http://cloudyr.github.io/drat', getOption('repos')))" \
    && R -e "install.packages(c('shinycssloaders'), repos='http://cran.rstudio.com/')" \
    && R -e "install.packages(c('shinyjs'), repos='http://cran.rstudio.com/')"

RUN printf 'run_as shiny;\n\
server {\n\
  listen 3838;\n\
  location / {\n\
    app_dir /srv/shiny-server/CommunityConnectorDockerShinyApp;\n\
    log_dir /var/log/shiny-server;\n\
  }\n\
}\n' > /etc/shiny-server/shiny-server.conf \
    && rm -r /srv/shiny-server/sample-apps

COPY app/ /srv/shiny-server/CommunityConnectorDockerShinyApp/

WORKDIR /root

RUN printf '#!/bin/sh\n\
mkdir -p /var/log/shiny-server\n\
chown shiny.shiny /var/log/shiny-server\n\
if [ "$APPLICATION_LOGS_TO_STDOUT" != "false" ];\n\
then\n\
    # push the "real" application logs to stdout with xtail in detached mode\n\
    exec xtail /var/log/shiny-server/ &\n\
fi\n\
echo "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI=$AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" > /home/shiny/.Renviron\n\
chown shiny:shiny /home/shiny/.Renviron\n\
# start shiny server\n\
exec shiny-server 2>&1\n' > start.sh \
  && chmod +x start.sh

CMD ["/root/start.sh"]
