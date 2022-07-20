FROM dukegcb/openshift-shiny-verse:4.1.2
RUN install2.r here
ADD ./src /srv/code
