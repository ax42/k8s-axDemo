FROM debian:buster

RUN useradd docker \
      && mkdir /home/docker \
      && chown docker:docker /home/docker \
      && addgroup docker staff

RUN apt-get update --allow-releaseinfo-change

RUN apt-get update -y \
  && apt-get install -y --no-install-recommends \
  ed \
  less \
  locales \
  vim-tiny \
  wget \
  curl \
  ca-certificates \
  fonts-texgyre \
  && rm -rf /var/lib/apt/lists/*v

RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

ENV R_BASE_VERSION 3.5.2
#RUN echo "deb http://http.debian.net/debian buster main" > /etc/apt/sources.list.d/debian-unstable.list \
#        && echo 'APT::Default-Release "buster";' > /etc/apt/apt.conf.d/default


## Now install R and littler, and create a link for littler in /usr/local/bin
RUN apt-get update -y \
    && apt-get install -y --no-install-recommends \
        littler \
	r-cran-littler \
	r-cran-plumber \
        r-base=${R_BASE_VERSION}-* \
#        r-base-dev=${R_BASE_VERSION}-* \
#        r-recommended=${R_BASE_VERSION}-* \
    && ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r \
    && ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
#    && ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
 #   && ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
   && install.r docopt \
   && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
   && rm -rf /var/lib/apt/lists/*

#CMD ["R"]
RUN install2.r plumber

WORKDIR /payload/
COPY [".","./"]

EXPOSE 8001
#ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
#CMD ["/payload/plumber.R"]
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/payload/plumber.R'); pr$run(host='0.0.0.0', port=8001, swagger = TRUE)"]
#CMD ["/payload/plumber.R"]

#CMD ["hourlyCost-api.R"]
