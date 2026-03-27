# 1. Use R 4.5.1
FROM rocker/r-ver:4.5.1

# 2. Install system dependencies (including cmake, nlopt, curl, and certificates)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev libssl-dev libcurl4-openssl-dev libpng-dev \
    libjpeg-dev libtiff-dev libfreetype6-dev libharfbuzz-dev \
    libfribidi-dev libicu-dev libglpk-dev zlib1g-dev \
    gfortran pandoc pkg-config \
    cmake libnlopt-dev curl ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# 3. Setup Environment
ENV R_LIBS_USER=/opt/renv/library
ENV RENV_PATHS_LIBRARY=/opt/renv/library
ENV R_PROFILE_USER=/dev/null
# Force renv to use Linux binaries for speed
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/noble/latest"
# Force renv to install binaries where available
ENV RENV_CONFIG_INSTALL_TYPE=binary

WORKDIR /project

# 4. Restore Packages (Cached Layer)
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R

# Bootstrap: Install renv from CRAN binaries
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest')); install.packages('renv')"

RUN mkdir -p /opt/renv/library

# Restore packages to /opt/renv/library (binaries used via env variable)
RUN R -e "renv::restore(library='/opt/renv/library')"

# 5. Copy all application files
COPY . .

# 6. Execution Logic
# ENTRYPOINT ["R", "--vanilla", "-e", "bookdown::render_book(\"index.Rmd\")"]
