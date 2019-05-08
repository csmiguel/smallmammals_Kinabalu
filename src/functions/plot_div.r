plot_rangemodel <- function(rm_output = rm_output, col = col){
  lines(rm_output$Sample.Site,
    rm_output$Computed.Mean.Richness, col = col, lwd = 1)
  polygon(x = c(rm_output$Sample.Site, rev(rmk$Sample.Site)),
        y = c(rm_output$Computed.Upper.95.CI, rev(rmk$Computed.Lower.95.CI)),
        col = adjustcolor(col, alpha = 0.2), border = NA)
      }
lowess_line <- function(mountain = mountain, index = index){
  assertthat::assert_that(mountain %in% c("Kinabalu", "Tambuyukon"),
    msg = "check mountain name in lowess line function")
  if(mountain == "Tambuyukon") col <- "orange"
  if(mountain == "Kinabalu") col <- "blue"
  lines(lowess(div$Elevation[div$Location == mountain],
  div[index][, 1][div$Location == mountain]), col = col, lwd = 1, lty = 3)
}
