# 1. Use R 4.5.1 (Matches Rocker's Ubuntu 24.04 based image)
FROM rocker/r-ver:4.5.1

# 2. Install system dependencies
# Added libharfbuzz-dev, libfribidi-dev, and libfreetype6-dev for 'ragg'
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libicu-dev \
    libglpk-dev \
    zlib1g-dev \
    gfortran \
    pandoc \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# 3. Setup Environment for Speed and Persistence
ENV RENV_PATHS_LIBRARY=/opt/renv/library
# Using the 'noble' (Ubuntu 24.04) binary repo for speed
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/noble/latest"
ENV RENV_DOWNLOAD_METHOD="libcurl"

WORKDIR /project

# 4. Setup and Restore
# Copy only the lockfile first to maximize Docker layer caching
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R

RUN mkdir -p /opt/renv/library
RUN R -e "options(Ncpus = parallel::detectCores()); renv::restore(library='/opt/renv/library')"

# 5. Copy Application Files
COPY *.Rmd ./
COPY _site.ym[l] ./ 

# 6. Flexible Execution
ENTRYPOINT ["sh", "-c", "R -e \"rmarkdown::render_site(config_file = '$1')\"", "--"]

# Default argument
CMD ["_site.yml"]
