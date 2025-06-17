#Input: a list of DEG tables
combined_deg_heatmap <- function(df,
                                 num_genes = 50,
                                 useFDR = TRUE,
                                 alpha = 0.05) {
  if (useFDR) {
    df <- df %>%
      purrr::map(~ dplyr::mutate(., pvalue = p.adjust(pvalue, method = "fdr")))
  }

  # Identify top DEGs in each dataset
  top_degs <- df %>%
    purrr::map(
      ~ dplyr::filter(., pvalue <= alpha) %>%
        dplyr::arrange(dplyr::desc(abs(log2FoldChange))) %>%
        dplyr::slice_head(n = num_genes) %>%
        dplyr::pull(Symbol)
    ) %>%
    purrr::flatten_chr() %>%
    unique()

  mat <- df %>%
    purrr::map( ~ dplyr::filter(., Symbol %in% top_degs) %>%
                  dplyr::select(Symbol, log2FoldChange)) %>%
    dplyr::bind_rows(.id = "Group") %>%
    tidyr::pivot_wider(names_from = Group, values_from = log2FoldChange) %>%
    tidyr::drop_na() %>%
    tibble::column_to_rownames(var = "Symbol") %>%
    as.matrix()

  # --- ADD THIS CHECK ---
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    message("No common top DEGs found across datasets or matrix is empty. Skipping heatmap generation.")
    return(NULL) # Or handle as appropriate, e.g., return an empty plot object
  }
  # --- END OF ADDITION ---

  min_val = min(mat, na.rm = T)
  max_val = max(mat, na.rm = T)

  # Ensure that if all values are 0, min_val and max_val are set appropriately
  # This avoids issues where min_val = Inf and max_val = -Inf or similar for all-zero matrices
  # If all values are 0, min_val and max_val would be 0, and the colorRamp2 would be c(0,0,0) which is problematic.
  # Better to set a small range around 0 in such cases or provide a clearer message.
  if (min_val == max_val) {
      if (min_val == 0) {
          min_val = -0.5 # or some small negative value
          max_val = 0.5  # or some small positive value
      } else { # if min_val = max_val != 0 (e.g., all values are 5)
          min_val = min_val - abs(min_val * 0.1)
          max_val = max_val + abs(max_val * 0.1)
      }
  }


  lgd = ComplexHeatmap::Legend(
    title = "log2FC",
    col_fun = circlize::colorRamp2(c(min_val, 0, max_val), c("blue", "white", "red")),
    at = c(min_val, 0, max_val),
    labels = c(round(min_val, 2), 0, round(max_val, 2)),
    direction = "vertical",
    labels_gp = grid::gpar(fontsize = 7),
    title_gp = grid::gpar(fontsize = 7)
  )

  ht = ComplexHeatmap::Heatmap(
    mat,
    column_names_rot = 0,
    column_names_gp = grid::gpar(fontsize = 9),
    column_names_centered = TRUE,
    row_names_gp = grid::gpar(fontsize = 5),
    show_row_names = TRUE,
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    show_heatmap_legend = FALSE
  )

  ComplexHeatmap::draw(
    ht,
    padding = grid::unit(c(0, 0, 0, 0), "mm"),
    heatmap_legend_list = lgd,
    heatmap_legend_side = "right"
  )

}

