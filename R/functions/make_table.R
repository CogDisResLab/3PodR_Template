make_table <- function(X, caption=NULL) {
  DT::datatable(
    X,
    rownames = FALSE,
    caption = htmltools::tags$caption(
      style = 'text-align: left;',
      caption
    ),
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      paging = TRUE,
      fixedHeader = TRUE,
      pageLength = 10
    )
  )
}