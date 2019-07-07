FROM rocker/shiny-verse:3.5.1

RUN apt-get update && apt-get install libssl-dev libcurl4-openssl-dev libv8-3.14-dev -y &&\
  mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install libraries
RUN install2.r jsonlite ggplot2 anytime httr gtools

# copy the app to the image COPY shinyapps /srv/shiny-server/
COPY appdir /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
