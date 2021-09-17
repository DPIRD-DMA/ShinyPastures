FROM rocker/shiny-verse:4.1.0
LABEL maintainer="Karen Holmes <karen.holmes@dpird.wa.gov.au>"
# geospatial rocker
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    liblwgeom-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev

RUN install2.r --error \
    RColorBrewer \
    RandomFields \
    RNetCDF \
    classInt \
    deldir \
    gstat \
    hdf5r \
    lidR \
    mapdata \
    maptools \
    mapview \
    ncdf4 \
    proj4 \
    raster \
    rgdal \
    rgeos \
    rlas \
    sf \
    sp \
    spacetime \
    spatstat \
    spdep \
    geoR \
    geosphere 

# Install all the requried R packages using install2r.r from 'littler'. Throw an
# error if installation fails.
RUN install2.r --error \
    --deps TRUE \
    plotly \
    plyr \
    shinydashboard \
    leaflet \
    ggplot2 \
    shinyWidgets \
    shinycssloaders \
    shinyjs
  
    # markdown

# remove all the default dummy apps
RUN rm -rf /srv/shiny-server/*

# make the default user 'shiny' the owner of the files
COPY --chown=shiny . /srv/shiny-server

EXPOSE 3838


