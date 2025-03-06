volcano_plot <- function(X,
                         showFDRLine = TRUE,
                         alpha = 0.05) {
  # Use FDR-adjusted or raw p-values as the significance measure
  if (showFDRLine) {
    X <- dplyr::mutate(X, p_sig = stats::p.adjust(pvalue, method = "fdr"))
    # Find the row with adjusted p-value closest to alpha
    closest_row <- X[which.min(abs(X$p_sig - alpha)), ]
    closest_pvalue <- closest_row$pvalue
  } else {
    X <- dplyr::mutate(X, p_sig = pvalue)
  }
  
  # Create labels with counts for each significance group
  ns <- paste0("NS (", sum(X$p_sig > alpha), ")")
  up <- paste0("Up (", sum(X$p_sig <= alpha &
                             X$log2FoldChange > 0), ")")
  down <- paste0("Down (", sum(X$p_sig <= alpha &
                                 X$log2FoldChange < 0), ")")
  
  # Assign significance labels based on the threshold
  X <- dplyr::mutate(
    X,
    Significant = dplyr::case_when(
      p_sig > alpha ~ "NS",
      p_sig <= alpha & log2FoldChange >= 0 ~ "Up",
      p_sig <= alpha & log2FoldChange < 0 ~ "Down"
    )
  )
  X$Significant <- factor(X$Significant, levels = c("Down", "NS", "Up"))
  
  # Select the top 10 points with the highest absolute log2 fold change among significant genes
  
  top10 <- dplyr::pull(dplyr::slice_head(dplyr::arrange(
    dplyr::filter(X, p_sig <= alpha), dplyr::desc(abs(log2FoldChange))
  ), n = 10), Symbol)
  
  # Label top 10 points for the plot
  
  X <- dplyr::mutate(X,
                     top10label = dplyr::if_else(Symbol %in% top10, Symbol, NA_character_))
  
  # Build the volcano plot
  
  p <- ggplot2::ggplot(X,
                       ggplot2::aes(
                         x = log2FoldChange,
                         y = -log10(pvalue),
                         col = Significant,
                         label = top10label
                       )) +
    ggplot2::geom_point(size = 1) +
    ggplot2::scale_color_manual(labels = c(down, ns, up),
                                values = c("blue", "black", "red")) +
    ggrepel::geom_text_repel(ggplot2::aes(fontface = "bold"),
                             size = 9 / ggplot2::.pt,
                             na.rm = TRUE) +
    ggplot2::geom_hline(
      linetype = "dotted",
      yintercept = -log10(alpha),
      col = "black"
    ) +
    ggprism::theme_prism(base_size = 9) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0
      )),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0
      )),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.spacing.y = grid::unit(-1, "mm"),
      legend.spacing.x = grid::unit(-1, "mm"),
      legend.box.spacing = grid::unit(-1, "mm"),
      legend.key.spacing.x = grid::unit(-1, "mm"),
      legend.key.spacing.y = grid::unit(-1, "mm"),
      legend.box.margin = ggplot2::margin(),
      legend.box = "vertical",
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0, 1.5, 0, 0), "mm"),
      panel.spacing = grid::unit(c(0, 0, 0, 0), "mm"),
      legend.margin = ggplot2::margin(0),
      legend.text = ggplot2::element_text(
        face = "bold",
        size = 9,
        margin = ggplot2::margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0
        )
      )
    )
  
  # If using FDR, add an additional horizontal line at the p-value corresponding to the FDR threshold
  if (showFDRLine) {
    p <- p + ggplot2::geom_hline(
      linetype = "dotted",
      yintercept = -log10(closest_pvalue),
      col = "black"
    )
  }
  
  return(p)
}