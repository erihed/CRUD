FROM dukegcb/openshift-shiny-verse:4.1.2
RUN install2.r here \
shiny \
shinythemes \
shinyWidgets \
shinyFeedback \
shinyjs \
thematic \
odbc \
DBI \
odbc \
DT \
dplyr \
ggplot2 \
validate \
uuid \
pool \
plotly
ADD ./DigitalLogbook /srv/code
