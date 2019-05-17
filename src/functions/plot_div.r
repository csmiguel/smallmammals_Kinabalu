plot_rangemodel <- function(rm_output = rm_output, col = col){
  lines(rm_output$Sample.Site,
    rm_output$Computed.Mean.Richness, col = col, lwd = 1)
  polygon(x = c(rm_output$Sample.Site, rev(rm_output$Sample.Site)),
        y = c(rm_output$Computed.Upper.95.CI, rev(rm_output$Computed.Lower.95.CI)),
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

plot_rangemodelr <- function(RM_list, mt = c("Kinabalu", "Tambuyukon"),
  col = col){
  rm_output <- RM_list[[mt]]
  lines(rm_output$class, rm_output$mean, col = col, lwd = 1)
  polygon(x = c(rm_output$class, rev(rm_output$class)),
        y = c(rm_output$q975, rev(rm_output$q025)),
        col = adjustcolor(col, alpha = 0.2), border = NA)
      }
