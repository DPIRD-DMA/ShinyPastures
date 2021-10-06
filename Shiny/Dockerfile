FROM rocker/shiny-verse:4.0.5
LABEL maintainer="Kenyon Ng <kenyon.ng@dpird.wa.gov.au>"
# geospatial rocker

# RUN sudo apt-get update && sudo apt-get install -y --no-install-recommends \
#     software-properties-common

# RUN add-apt-repository --yes "ppa:edd/r-4.0" \
#     && apt-get install -y --no-install-recommends \
#     sudo \
#     r-cran-bspm \
#     && echo "bspm::enable()" >> /etc/R/Rprofile.site \
#     && echo "options(bspm.sudo=TRUE)" >> /etc/R/Rprofile.site \
#     && echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/90local-no-recommends \
#     && echo "docker ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/local-docker-user \
#     && chmod 0440 /etc/sudoers.d/local-docker-user \
#     && chgrp 1000 /usr/local/lib/R/site-library \
#     && install2.r remotes

# RUN apt-get update \
#   && apt-get install -y --no-install-recommends \
#     lbzip2 \
#     libfftw3-dev \
#     libgdal-dev \
#     libgeos-dev \
#     libgsl0-dev \
#     libgl1-mesa-dev \
#     libglu1-mesa-dev \
#     libhdf4-alt-dev \
#     libhdf5-dev \
#     libjq-dev \
#     liblwgeom-dev \
#     libpq-dev \
#     libproj-dev \
#     libprotobuf-dev \
#     libnetcdf-dev \
#     libsqlite3-dev \
#     libssl-dev \
#     libudunits2-dev \
#     netcdf-bin \
#     postgis \
#     protobuf-compiler \
#     sqlite3 \
#     tk-dev \
#     unixodbc-dev

RUN sudo apt-get update && sudo apt-get install -y --no-install-recommends \
    libnetcdf-dev \
    libgeos-dev \
    libproj-dev \
    libgdal-dev

RUN install2.r --error \
    remotes \
    raster \
    shinydashboard \
    leaflet \
    rgdal \
    shinyWidgets \
    shinycssloaders \
    shinyjs \
    plotly \
    rgeos \
    feather \
    ncdf4 \
    plyr
    # dqshiny \

RUN installGithub.r daqana/dqshiny

# Install all the requried R packages using install2r.r from 'littler'. Throw an
# error if installation fails.
# RUN install2.r --error \
#     plotly 
    # markdown

# remove all the default dummy apps
RUN rm -rf /srv/shiny-server/*

# make the default user 'shiny' the owner of the files
COPY --chown=shiny . /srv/shiny-server

EXPOSE 3838


