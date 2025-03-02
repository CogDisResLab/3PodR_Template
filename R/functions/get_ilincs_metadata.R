#Input: Vector of LINCS signature ids from drugFindr
#Output: Tibble of iLINCS signature metadata
get_ilincs_metadata <- function(X) {
  url <- "https://www.ilincs.org/api/SignatureMeta/findMany"
  body <- list(signatures = toJSON(X))
  
  metadata <- POST(url, body = body, encode = "json") %>%
    content(as = "text") %>%
    fromJSON() %>%
    pluck("data") %>%
    select(TargetSignature = signatureid, tissue, integratedMoas, GeneTargets) #where(~ any(!is.na(.x)))
}