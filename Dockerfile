# 1. Use R 4.5.1 to match your Mac
FROM rocker/r-ver:4.5.1

# 2. Install system dependencies
# Note: Ubuntu 24.04 (Noble) uses libicu74, matching R 4.5 binaries
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libpng-dev libjpeg-dev zlib1g-dev libicu-dev \
    libglpk-dev gfortran pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /project

# 3. Use NOBLE (Ubuntu 24.04) binaries for R 4.5
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/noble/latest"
ENV RENV_DOWNLOAD_METHOD="libcurl"

# 4. Setup and Restore
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
RUN mkdir -p renv/library data
RUN R -e "options(Ncpus = parallel::detectCores()); renv::restore()"

COPY *.Rmd ./
COPY _site.ym[l] ./ 

CMD ["R", "-e", "rmarkdown::render_site()"]
