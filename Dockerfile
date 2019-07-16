FROM debian:buster

#RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 648ACFD622F3D138
#RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 04EE7237B7D453EC

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y --no-install-recommends \
#RUN apt-get install -y \
#  make \
#  gcc \
#  libc++-dev \
  r-cran-shiny \
  r-cran-dplyr \
  r-cran-httr \
  r-cran-ggplot2

COPY app.R /srv/app.R
#COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
#COPY shiny-server.sh /usr/bin/shiny-server.sh
#RUN chmod 755 /usr/bin/shiny-server.sh
#RUN adduser shiny

EXPOSE 3838
#CMD ["/usr/bin/shiny-server.sh"]
CMD /usr/bin/R -e "shiny::runApp('/srv/app.R', port = 3838, host = '0.0.0.0')"
