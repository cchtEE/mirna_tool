## reference: https://github.com/isglobal-brge/exposomeShiny

FROM rocker/shiny-verse:4.1.3

LABEL maintainer="USER lawarde.ankita1@gmail.com"
# system libraries of general use

RUN apt-get update && apt-get install -y \
sudo \
pandoc \
pandoc-citeproc \
libnode-dev \
curl \
libjpeg-dev \
libudunits2-dev \
vim

# copy shiny-server.sh to image

COPY shiny-server.sh /usr/bin/

# copy shiny server config to image

COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf


# copy installer

COPY installer.R /srv/shiny-server/

# copy the contents of app folder to image
RUN mkdir /root/app
COPY mirna /root/shiny_save

# install R packages

RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('shiny', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('shinydashboard', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('shinyBS', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('shinyjs', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('shinythemes', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('BiocManager', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('RCurl', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('R.utils', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('knitr', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('testthat', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('rmarkdown', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('ggrepel', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('highcharter', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('magrittr', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('survival', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('rlang', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('RColorBrewer', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('igraph', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('downloader', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('visNetwork', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('DT', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('ggplot2', repos = 'https://cloud.r-project.org')"
RUN R -e "devtools::install_github('kassambara/survminer', build_vignettes = FALSE)"
RUN R -e "BiocManager::install('enrichplot')"
RUN R -e "BiocManager::install('clusterProfiler')"
RUN R -e "BiocManager::install('hpar')"

RUN apt-get update && apt-get install -y \
libbz2-dev \
liblzma-dev

# select port

EXPOSE 80
EXPOSE 3838

# allow permission for user ‘shiny’ to run

RUN sudo chown -R shiny:shiny /srv/shiny-server

# install linux programs to enable conversion of ms dos file to unix file

# RUN apt-get update && apt-get install -y dos2unix

# Change access permissions to shiny-server.sh - did not need this for my purposes

#RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"] 

# run app

#CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 3838)"]
#CMD ["/usr/bin/shiny-server.sh"]
CMD ["R", "-e", "shiny::runApp('/root/shiny_save', host = '0.0.0.0', port = 3838)"]

