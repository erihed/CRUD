# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# expose port
EXPOSE 3838
  
LABEL io.openshift.expose-services="8080:http"

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

COPY Rprofile.site /etc/R
RUN install2.r --error --skipinstalled \
shiny \
shinythemes \
shinyWidgets \
shinyFeedback \
shinyjs \
thematic \
unixodbc \
DBI \
odbc \
DT \
dplyr \
ggplot2 \
validate \
uuid \
pool \
plotly


# copy necessary files
## app folder
RUN mkdir -p /srv/shiny-server/soker
COPY docker.Rproj /srv/shiny-server/soker
COPY server.R /srv/shiny-server/soker
COPY ui.R /srv/shiny-server/soker
COPY renv.lock /srv/shiny-server/soker
COPY server.R /srv/shiny-server/soker
COPY renv  /srv/shiny-server/soker/renv
# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

RUN chown -R shiny /srv/shiny-server/
RUN chown -R shiny /var/lib/shiny-server/

# Run as a non-root user
USER shiny


export IMAGE="analythium/covidapp-shiny:shiny"
export FILE="Dockerfile.shiny"
DOCKER_BUILDKIT=1 docker build --no-cache -f $FILE -t $IMAGE .
docker run -p 8080:3838 $IMAGE
