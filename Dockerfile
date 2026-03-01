# 1. Use r-ver for native Apple Silicon (ARM64) support
FROM rocker/r-ver:latest

# 2. Install system dependencies + Pandoc (Critical for RMarkdown)
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /project

# 3. Setup renv files
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
RUN mkdir -p renv library data

# 4. SPEED BOOST: 
# - Use 'jammy' binaries (pre-compiled for the container's Ubuntu base)
# - Use all CPU cores (Ncpus) to install 200+ packages in parallel
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest'), Ncpus = parallel::detectCores()); renv::restore()"

# 5. Copy your Rmd files and site configuration
# Use a wildcard for _site.yml so it doesn't crash if the file is missing
COPY *.Rmd ./
COPY _site.ym[l] ./ 

# 6. Render the site
CMD ["R", "-e", "rmarkdown::render_site()"]
