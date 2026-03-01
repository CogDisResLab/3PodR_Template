# 1. Use R 4.5.1
FROM rocker/r-ver:4.5.1

# 2. Install system dependencies (restoring cmake and adding nlopt)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev libssl-dev libcurl4-openssl-dev libpng-dev \
    libjpeg-dev libtiff-dev libfreetype6-dev libharfbuzz-dev \
    libfribidi-dev libicu-dev libglpk-dev zlib1g-dev \
    gfortran pandoc pkg-config \
    cmake libnlopt-dev \
    && rm -rf /var/lib/apt/lists/*

# 3. Setup Environment
ENV R_LIBS_USER=/opt/renv/library
ENV RENV_PATHS_LIBRARY=/opt/renv/library
ENV R_PROFILE_USER=/dev/null 
# Force renv to use the Linux binaries for speed
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/noble/latest"

WORKDIR /project

# 4. Restore Packages (Cached Layer)
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R

# Bootstrap: Install renv from binaries
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest')); install.packages('renv')"

RUN mkdir -p /opt/renv/library
# Now renv::restore will work with the binary override
RUN R -e "renv::restore(library='/opt/renv/library')"

# 5. Copy ALL Application Files
COPY . .

# 6. Execution Logic
ENTRYPOINT ["sh", "-c", "ln -sf /project/$1 /project/configuration.yml && R --vanilla -e 'bookdown::render_book(\"index.Rmd\")'", "--"]

CMD ["configuration.yml"]
