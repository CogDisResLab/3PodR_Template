#Input: Vector of Gene symbols
#Output: Tibble of enrichR results
do_enrichr <- function(X) {
  dbs = c("GO_Biological_Process_2023", 
          "GO_Molecular_Function_2023", 
          "GO_Cellular_Component_2023")
  
  columns = c("Biological_Process",
              "Molecular_Function",
              "Cellular_Component")
  
  quiet(X %>%
          enrichr(databases = dbs) %>% 
          map2(columns, ~ mutate(.x, namespace = .y)) %>%
          bind_rows %>%
          filter(Adjusted.P.value <= .05) %>%
          extract(Term, "GOID", "(GO:\\d+)", remove = FALSE) %>%
          extract(Term, "Term", "(.*?)\\(GO:\\d+\\)"))
}