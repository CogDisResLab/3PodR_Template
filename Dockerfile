# 1. Use R 4.4.2 (Ubuntu 22.04 Jammy)
FROM rocker/r-ver:4.4.2

# 2. Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libpng-dev libjpeg-dev zlib1g-dev libicu-dev \
    glpk-utils libglpk-dev \ 
    pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /project

# 3. FORCE STABLE REPOS
# We add the Bioconductor repo override here to fix the 'fgsea' 3.21 error
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/jammy/latest"
ENV RENV_BIOCONDUCTOR_VERSION="3.20"
ENV RENV_DOWNLOAD_METHOD="libcurl"
ENV RENV_PATHS_LIBRARY="renv/library"

# 4. Setup renv
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
RUN mkdir -p renv/library data

# 5. Restore
# We detect cores to speed up the compilation of Bioc packages
RUN R -e "options(Ncpus = parallel::detectCores()); renv::restore()"

COPY *.Rmd ./
COPY _site.ym[l] ./ 

CMD ["R", "-e", "rmarkdown::render_site()"]
