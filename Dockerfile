# 1. Use r-ver (Works on Mac ARM64 and GitHub AMD64)
FROM rocker/r-ver:latest

# 2. Install system dependencies + Pandoc
# Added image libraries (libpng, libjpeg) and string processing (libicu)
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
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/jammy/latest"
ENV RENV_DOWNLOAD_METHOD="libcurl"
ENV RENV_PATHS_LIBRARY="renv/library"

# 4. Setup renv files
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
# Pre-create the library folder
RUN mkdir -p renv/library data

# 5. Restore using all available CPU cores
# This will now find the 'png.h' header it needs for the 'png' package
RUN R -e "options(Ncpus = parallel::detectCores()); renv::restore()"

# 6. Copy project assets
COPY *.Rmd ./
COPY _site.ym[l] ./ 

# 7. Render
CMD ["R", "-e", "rmarkdown::render_site()"]
