# 1. Use R 4.5.1
FROM rocker/r-ver:4.5.1

# 2. Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev libssl-dev libcurl4-openssl-dev libpng-dev \
    libjpeg-dev libtiff-dev libfreetype6-dev libharfbuzz-dev \
    libfribidi-dev libicu-dev libglpk-dev zlib1g-dev \
    gfortran pandoc pkg-config \
    && rm -rf /var/lib/apt/lists/*

# 3. Setup Environment
ENV R_LIBS_USER=/opt/renv/library
ENV RENV_PATHS_LIBRARY=/opt/renv/library
# This prevents your local .Rprofile from causing loops during build
ENV R_PROFILE_USER=/dev/null 
WORKDIR /project

# 4. Restore Packages (Cached Layer)
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R

# Bootstrap: Install renv from binaries for speed
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest')); install.packages('renv')"

RUN mkdir -p /opt/renv/library
# Now renv::restore will work
RUN R -e "renv::restore(library='/opt/renv/library')"

# 5. Copy ALL Application Files
# This ensures 3PodR.R, chapters/, R/, style.css, etc. are included
COPY . .

# 6. Execution Logic
# We force the symlink and run render_book. 
# Using index.Rmd ensures bookdown merges your chapters correctly.
ENTRYPOINT ["sh", "-c", "ln -sf /project/$1 /project/configuration.yml && R --vanilla -e 'bookdown::render_book(\"index.Rmd\")'", "--"]

CMD ["configuration.yml"]
