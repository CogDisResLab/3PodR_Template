# 1. Use the Jammy-based image to match Posit's Ubuntu 22.04 binaries
FROM rocker/r-ver:4.4.2-jammy

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
# These binaries now match the OS (Jammy) perfectly, preventing 'stringi' errors
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/jammy/latest"
ENV RENV_DOWNLOAD_METHOD="libcurl"
ENV RENV_PATHS_LIBRARY="renv/library"

# 4. Setup renv files
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
RUN mkdir -p renv/library data

# 5. Restore using all available CPU cores
RUN R -e "options(Ncpus = parallel::detectCores()); renv::restore()"

# 6. Copy project assets (Ensure configuration.yml is NOT copied if you want to mount it at runtime)
COPY *.Rmd ./
COPY _site.ym[l] ./ 
# Optional: COPY configuration.yml configuration.yml (only if you want a baked-in default)

# 7. Render
CMD ["R", "-e", "rmarkdown::render_site()"]
