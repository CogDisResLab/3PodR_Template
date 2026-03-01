# 1. Use R 4.5.1 to match your Mac
FROM rocker/r-ver:4.5.1

# 2. Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libpng-dev libjpeg-dev zlib1g-dev libicu-dev \
    libglpk-dev gfortran pandoc \
    && rm -rf /var/lib/apt/lists/*

# 3. Setup Environment for Speed and Persistence
# We move the library to /opt/renv so it isn't overwritten when you mount /project
ENV RENV_PATHS_LIBRARY=/opt/renv/library
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/noble/latest"
ENV RENV_DOWNLOAD_METHOD="libcurl"

WORKDIR /project

# 4. Setup and Restore
# We restore specifically to the /opt path
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
RUN mkdir -p /opt/renv/library
RUN R -e "options(Ncpus = parallel::detectCores()); renv::restore(library='/opt/renv/library')"

# 5. Copy Application Files
COPY *.Rmd ./
COPY _site.ym[l] ./ 

# 6. Flexible Execution
# This allows you to pass a custom yaml filename as an argument
ENTRYPOINT ["sh", "-c", "R -e \"rmarkdown::render_site(config_file = '$1')\"", "--"]

# Default argument if you don't provide one
CMD ["_site.yml"]
