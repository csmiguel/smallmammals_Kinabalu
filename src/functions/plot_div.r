plot_rangemodel <- function(rm_output = rm_output, cols){
  lines(rm_output$Sample.Site,
    rm_output$Computed.Mean.Richness, col = col, lwd = 1)
  polygon(x = c(rm_output$Sample.Site, rev(rm_output$Sample.Site)),
        y = c(rm_output$Computed.Upper.95.CI, rev(rm_output$Computed.Lower.95.CI)),
        col = adjustcolor(col, alpha = 0.2), border = NA)
      }
lowess_line <- function(mountain = mountain, index = index, ...){
  assertthat::assert_that(mountain %in% c("Kinabalu", "Tambuyukon"),
    msg = "check mountain name in lowess line function")
  col <- cols[grep(mountain, mt)]
  lines(lowess(div$Elevation[div$Location == mountain],
  div[index][, 1][div$Location == mountain]), col = col, lwd = 2,
  lty = grep(mountain, mt) + 1)
}

plot_rangemodelr <- function(RM_list, mtt = c("Kinabalu", "Tambuyukon"), ...){
  rm_output <- RM_list[[mtt]]
  #lines(rm_output$class, rm_output$mean, col = col, lwd = 1)
  polygon(x = c(rm_output$class, rev(rm_output$class)),
        y = c(rm_output$q975, rev(rm_output$q025)),
        col = adjustcolor(
          cols_confidence[grep(mtt, mt)],
          alpha = 0.2), border = NA)
      }
plot_pred <- function(mountain = mountain, m = model){
  assertthat::assert_that(mountain %in% c("Kinabalu", "Tambuyukon"),
    msg = "check mountain name in lowess line function")
  model <- paste0("pred_", m)
  if(mountain == "Tambuyukon") {col <- cols[2]; p_pch = 17}
  if(mountain == "Kinabalu") {col <- cols[1]; p_pch = 16}
  points(x = div[["Elevation"]][div$Location == mountain],
  y = div[[model]][div$Location == mountain], col = col, pch = p_pch)
}
