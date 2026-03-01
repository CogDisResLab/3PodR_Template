# 1. Use R 4.4.2 which is based on Ubuntu 22.04 (Jammy)
# This provides libicu70 required by your stringi/Posit binaries.
FROM rocker/r-ver:4.4.2

# 2. Install system dependencies + Pandoc
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libpng-dev \
    libjpeg-dev \
    zlib1g-dev \
    libicu-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /project

# 3. FORCE BINARIES & PARALLELISM
# The repository URL for 'jammy' now aligns with the OS version of this image.
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/jammy/latest"
ENV RENV_DOWNLOAD_METHOD="libcurl"
ENV RENV_PATHS_LIBRARY="renv/library"

# 4. Setup renv files
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
RUN mkdir -p renv/library data

# 5. Restore using all available CPU cores
# This should now pull AMD64 binaries instantly on GitHub Actions.
RUN R -e "options(Ncpus = parallel::detectCores()); renv::restore()"

# 6. Copy project assets
COPY *.Rmd ./
COPY _site.ym[l] ./ 

# 7. Render
CMD ["R", "-e", "rmarkdown::render_site()"]
