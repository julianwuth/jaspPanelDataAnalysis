#' @import jaspBase
#' @export
.createPlmPlot <- function(jaspResults, dataset, options, ready){

  plmPlot <- createJaspPlot(title = "Panel Plot",  width = 160, height = 320)

  plmPlot$dependOn(c("time", "id", "dependent", "plot"))

  jaspResults[["plmPlot"]] <- plmPlot

  if (!ready)
    return()

  .plmFillPlotDescriptives(plmPlot, dataset, options)

  return()
}

.plmFillPlotDescriptives <- function(plmPlot, dataset, options){
  dataset <- dataset
  dataset[[options$time]] <- as.numeric(dataset[[options$time]])
  rangeTime <- range(dataset[[options$time]])

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(rangeTime)

  aes <- ggplot2::aes

  plot <- ggplot2::ggplot(dataset, aes(dataset[[options$time]], dataset[[options$dependent]], color = dataset[[options$id]]))+ggplot2::geom_line()

  plot <- plot +
    jaspGraphs::scale_x_continuous(options$time, breaks = xBreaks) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe(sides = 'bl')

  plmPlot$plotObject <- plot

  return()
}
