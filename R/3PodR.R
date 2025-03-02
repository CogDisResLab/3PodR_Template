# Load libraries and set options
suppressPackageStartupMessages({
  library(magrittr)
  library(tidyverse)
  library(purrr)
  library(HGNChelper)
  library(ggpubr)
  library(ggrepel)
  library(pheatmap)
  library(knitr)
  library(BioPathNet)
  library(drugfindR)
  library(jsonlite)
  library(httr)
  library(enrichR)
  library(factoextra)
  library(babelgene)
  library(ggVennDiagram)
  library(PAVER)
  library(ggupset)
  library(DT)
  library(circlize)
  library(randomcoloR)
  library(ComplexHeatmap)
})

suppressMessages(
  options(
    readr.show_col_types = FALSE,
    timeout = 999,
    rlib_name_repair_verbosity = "quiet"
  )
)

set.seed(123)

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = FALSE,
  out.width = "100%",
  out.height = "100%",
  fig.align = "center",
  dpi = 300 # TODO: 600, or use vector plots?
)

# Load functions used throughout the report
list.files(here::here("R/functions"), full.names = TRUE) %>%
  purrr::walk(source)

# Setup report state
global_state <- yaml::read_yaml(file = here::here("extdata/_variables.yml"),
                                readLines.warn = FALSE)

# This is the HGNCHelper precomputed mapping file
global_state$humanmap <- here::here(file.path("extdata", "assets", global_state$humanmap)) %>%
  readr::read_csv()

#These are gene annotations for each species
global_state$hgnc <- here::here(file.path("extdata", "assets", global_state$hgnc)) %>%
  readr::read_csv() %>%
  dplyr::select(Symbol = symbol, Name = name)

global_state$mgi <- here::here(file.path("extdata", "assets", global_state$mgi)) %>%
  readr::read_csv() %>%
  dplyr::select(Symbol = `Marker Symbol`, Name = `Marker Name`)

global_state$rgd <- here::here(file.path("extdata", "assets", global_state$rgd)) %>%
  readr::read_csv() %>%
  dplyr::select(Symbol = SYMBOL, Name = NAME)

global_state$map <- dplyr::case_when(
  global_state$species == "human" ~ list(global_state$hgnc),
  global_state$species == "mouse" ~ list(global_state$mgi),
  global_state$species == "rat" ~ list(global_state$rgd)
) %>%
  purrr::pluck(1)

#This is the scraped LINCS FDA data
global_state$lincs_fda <- file.path("extdata", "assets", global_state$lincs_fda) %>%
  here::here() %>%
  read_csv()

#This is the GMT file
global_state$gmt <- file.path("extdata", "assets", global_state$gmt) %>%
  here::here()

#Theses are files for PAVER
global_state$embeddings <- file.path("extdata", "assets", global_state$embeddings) %>%
  here::here() %>%
  readr::read_rds()

global_state$term2name <- file.path("extdata", "assets", global_state$term2name) %>%
  here::here() %>%
  read_rds()

#Read counts and design if specified
if (!is_empty(global_state$design) &
    !is_empty(global_state$counts)) {
  global_state$design <- file.path("extdata", global_state$design) %>%
    here::here() %>%
    readr::read_csv()
  
  global_state$counts <- file.path("extdata", global_state$counts) %>%
    here::here() %>%
    read_counts()
  
  if (global_state$species == "human") {
    #Correct HGNC symbols of the counts if human data
    global_state$counts %<>% fix_hgnc(global_state$humanmap)
  }
  
  global_state$using_counts <- TRUE
  
} else {
  global_state$using_counts <- FALSE
}

# Load input DEG data
global_state$data <- global_state$data %>%
  map( ~ {
    x <- .
    
    x <- utils::modifyList(x, list(
      name  = paste0(x$group1, " vs ", x$group2),
      data  = read_deg(paste0("extdata/", x$file)),
      results = list()
    ))
    
    if (global_state$species == "human") {
      x$data <- fix_hgnc(x$data, global_state$humanmap)
    }
    
    # Prepare BioPathNet data using DEG statistics
    x$bpn <- BioPathNet::prepare_data(x$data$Symbol, x$data$log2FoldChange, x$data$pvalue)
    
    x
  })

global_state$data <- purrr::set_names(global_state$data, purrr::map_chr(global_state$data, "name"))

#Create global results store
global_state$results <- list()