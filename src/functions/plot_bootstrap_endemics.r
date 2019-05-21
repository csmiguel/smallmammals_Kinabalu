plot_bootstrap_endemics <- function(prop_endemics, permutations, y_lab, mt){
  cols <- c("blue", "orange")
  plot(1, type = "n", las = 1,
    xlim = c(500, 3300),
    ylim = c(0, max(unlist(n_end))),
    ylab = y_lab, xlab = "Elevation (m)")

  for (i in seq_along(mt)){
    nn <- names(prop_endemics[[mt[i]]])
    pp <- permutations[[mt[i]]][c(1, 3), ]
    polygon(c(nn, rev(nn)), c(pp[1, ], rev(pp[2, ])),
    col = adjustcolor(cols[i], alpha = 0.2), border = NA)
    lines(nn, prop_endemics[[mt[i]]], col = cols[i],
      type = "o", pch = c(1, 2)[i])
  }
  legend("bottomright", col = cols, lty = 1, pch = 1,
  legend = paste("Mt.", mt), bty = "n")
}
