#' @title Plot Moran's I across distance thresholds
#' @description Plots Moran's I values and p-values across distance thresholds. The x axis represents distance thresholds, the y axis shows Moran's I, and point size indicates significance.
#' @param model Data frame with columns `distance.threshold`, `moran.i`, `moran.i.null`, and `p.value`, as returned by [moran_multithreshold()].
#' @param point.color Character vector of colors for the gradient scale. Default: viridis-like palette.
#' @param line.color Character string, color of connecting lines. Default: `"gray30"`.
#' @param verbose Logical. If `TRUE`, the resulting plot is printed. Default: `TRUE`.
#' @return A ggplot object.
#' @seealso [moran()], [moran_multithreshold()]
#' @noRd
plot_moran <- function(
  model = NULL,
  point.color = c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"),
  line.color = "gray30",
  verbose = TRUE
) {
  #declaring variables
  distance.threshold <- NULL
  moran.i <- NULL
  moran.i.null <- NULL
  p.value.binary <- NULL

  x <- model

  #adding binary p.value
  x$p.value.binary <- "< 0.05"
  x[x$p.value >= 0.05, "p.value.binary"] <- ">= 0.05"
  x$p.value.binary <- factor(
    x$p.value.binary,
    levels = c("< 0.05", ">= 0.05")
  )

  p1 <- ggplot2::ggplot(data = x) +
    ggplot2::aes(
      x = distance.threshold,
      y = moran.i,
      fill = moran.i
    )

  if (nrow(x) > 1) {
    p1 <- p1 + ggplot2::geom_line(linewidth = 1, color = line.color)
  }

  p1 <- p1 +
    ggplot2::geom_point(
      ggplot2::aes(size = p.value.binary),
      pch = 21
    ) +
    ggplot2::scale_fill_gradientn(colors = point.color) +
    ggplot2::geom_hline(
      yintercept = x$moran.i.null[1],
      col = line.color,
      linetype = "dashed"
    ) +
    ggplot2::scale_size_manual(
      breaks = c("< 0.05", ">= 0.05"),
      values = c(2.5, 5),
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(breaks = x$distance.threshold) +
    ggplot2::xlab("Distance thresholds") +
    ggplot2::ylab("Moran's I") +
    ggplot2::ggtitle("Multiscale Moran's I") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(size = "Moran's I p-value") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::guides(fill = "none")

  if (verbose) {
    suppressWarnings(print(p1))
  }

  p1
}
