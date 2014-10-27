# Storing plotting objects with the different tabs
# params are a named list (this is how to access plots later again too)
# common params used
# type - the type of the plot 
# single vs multiplot
# zoomable - whether it can in theory be zoomed
# dataTable - whether it can show a datatable
plots.storeInfo<-function(pn, info, reset=FALSE) {
  if (reset)
    plotinfo<-list()
  else
    plotinfo<-pn.getSelectedPlotTabParam(pn, params="plotinfo")
  for (name  in names(info))
    plotinfo[name]<-info[name]
  pn.setSelectedPlotTabParam(pn, list(plotinfo=plotinfo)) # save plot parameter
}

plots.getInfo<-function(pn) return(pn.getSelectedPlotTabParam(pn, params="plotinfo"))

#####################
# PLOTTING HANDLERS #
#####################

# Plotting mass spectra handler
dataset.peakSpecPlotHandler<-function(plotsNotebook, dataTable, datasetTable) {
  # store the infos about the plot
  plotinfo<-list(
    type="MASS-SPECTRUM", 
    filePath=NULL,
    RT="")
  
  if(!is.empty(selRT<-svalue(dataTable))) { 
    datasetID<-svalue(datasetTable)
    plotinfo$RT<-selRT
    file<-file.path("spectra", paste("spec_dataset_", datasetID, "_RT_", selRT, ".RData", sep=""))
    if (!is.na(file.info(file)$size) ) { # file exists!
      plotinfo$filePath<-file
      plotinfo$zoomable<-TRUE
      plotinfo$zoomHandler<-"plot.peakSpectrum"
    }
  }
  
  # go plot
  plotinfo<-plot.peakSpectrum(plotinfo)
  plots.storeInfo(plotsNotebook, reset=TRUE, plotinfo)
}

# Plot data from single data set
dataset.plotDSQHandler<-function(plotsNotebook, datasetID, datasetName, datasetType, data, varI, showRTs){
  
  plotinfo<-list(
    type="DSQDATA",
    multiPlot=FALSE,
    datasetID=datasetID,
    datasetName=datasetName, 
    datasetType=datasetType)
  
  # prepare data with labeling information
  if (nrow(data) > 0) {
    data$Label<-""
    data$Color<-NA
    notfound<-which(!is.na(data$ID) & is.na(data$Area))
    assigned<-which(!is.na(data$ID) & !is.na(data$Area))
    unassigned<-which(is.na(data$ID))
    if (!is.empty(notfound)) {
      data[notfound, c("Label", "Color")]<-list(data$Name[notfound], "red")
      if (showRTs)
        data$Label[notfound]<-paste(data$Label[notfound], "\n(", round(data$RT[notfound],2), ")", sep="")
    }
    if (!is.empty(assigned)) {
      data[assigned, c("Label", "Color")]<-list(data$Name[assigned], "black")
      if (showRTs)
        data$Label[assigned]<-paste(data$Label[assigned], "\n(", round(data$RT[assigned],2), ")", sep="")
    }
    if (!is.empty(unassigned)) {
      data[unassigned, c("Label", "Color")]<-list("", "blue")
      if (showRTs)
        data$Label[unassigned]<-round(data$RT[unassigned],2)
    }
  } else
    data<-NULL
  
  # case 1: chromatogram
  if ( varI == 1 ) {
    # note you're in a chrom
    plotinfo$plotType<-"CHROM"
    plotinfo$plotName<-plotinfo$datasetName # just go for this naming
    plotinfo$filePath<-NULL # none assigned yet
    
    filename<-paste("chrom_dataset_", datasetID, "_", datasetType, ".RData", sep="")
    file=file.path("chroms", filename)
    
    if (!is.na(file.info(file)$size) ) { # file exists!
      # store additional information
      plotinfo$filePath<-file
      plotinfo$dataView<-data.frame(Compound = data$Label, data[c("RT", "StartRT", "EndRT", "Height", "Area")])
      plotinfo$peakLabels<-data 
      plotinfo$zoomable<-TRUE
      plotinfo$zoomHandler<-"plot.DSQChrom"
      
      # check if you're replotting the same file as before (and keep zooming if that's the case)
      previousPlot<-plots.getInfo(plotsNotebook)
      if (!is.null(previousPlot) && identical(previousPlot$filePath, plotinfo$filePath)) # exact same chromatogram, replot w/ zoom
        plotinfo<-c(plotinfo, previousPlot[c("xlim", "ylim", "xlimInit", "ylimInit")])
    } else {
      plotinfo$errorInfo<-file # save file path for the error information
    }
    
    # go plot
    plotinfo<-plot.DSQChrom(plotinfo)
    
    # case 2: raw area/height
  } else if ( varI == 2 ) { 
    # note you're in a chrom
    plotinfo$plotType<-"HEIGHT-AREA"
    plotinfo$data<-data
    plotinfo$dataView<-data.frame(Compound = data[c("Label")], data[c("RT", "StartRT", "EndRT", "Height", "Area")])
    
    # fill in empty rows data with 0s
    if (!is.empty(emptyRows<-which(is.na(plotinfo$data$Area)))) 
      plotinfo$data[emptyRows,c("Area", "Height")] <- list(0, 0)
    
    # plot
    plotinfo<-plot.DSQData(plotinfo)
  } 
  
  # store plotting information
  plots.storeInfo(plotsNotebook, reset=TRUE, plotinfo)
}

# plot single dataset with Delta plus data
dataset.plotDPHandler<-function(plotsNotebook, datasetID, datasetName, data, varI, plotRefs=TRUE, plotNonRefs=TRUE) {
  
  plotinfo<-c(list(
    type="DPDATA",
    multiPlot=FALSE,
    datasetID=datasetID,
    data=NULL,
    datasetName=datasetName),
    lapply(datasets.getDPlusPlottingOptions()[varI,], function(x) x))
  
  if ( (plotRefs || plotNonRefs) && nrow(data) > 0) { # there is data
    data<-subset(data, RefPeak%in%c(plotRefs, !plotNonRefs)) # select selected peaks
    if (!is.empty(sel<-which(data$RefPeak==TRUE & !is.na(data$Name))))
      data[sel, "Name"]<-paste("*", data[sel,"Name"]) # mark reference peaks
    
    if (!is.empty(sel<-which(data$RefPeak==TRUE & is.na(data$Name))))
      data[sel, "Name"]<-"*" # mark reference peaks
    plotinfo$data<-data
    plotinfo$data$rArea2<-plotinfo$data$rArea2/1000
  }
  
  # plot and store plotting information
  plotinfo<-plot.DPData(plotinfo)
  plots.storeInfo(plotsNotebook, reset=TRUE, plotinfo)
}

# dataset plotting options
datasets.getDSQPlottingOptions<-function(normPeakID = NULL) {
  # get variables and labels matrix (varaible name, type is DSQ=1, DP=2; varI is DSQvarI and DPvarI; normI is DSQnormI  plottype="bar"/"point", )
  vars<-data.frame(variable=character(), plottype=character(), normalized=logical(), label=character(), stringsAsFactors=FALSE)
  vars[nrow(vars)+1,]<-c("Chrom", "multiplot", TRUE, "Chromatograms")
  vars[nrow(vars)+1,]<-c("Area", "bar", FALSE, "Raw Area Data")
  vars[nrow(vars)+1,]<-c("AreaRelT", "bar", TRUE, "Normalized to total (all peaks)")
  vars[nrow(vars)+1,]<-c("AreaRelA", "bar", TRUE, "Normalized to total (active peaks)")
  vars[nrow(vars)+1,]<-c("AreaRelP", "bar", TRUE, "Normalized to selected peak")
  if (!is.null(normPeakID))
    vars[nrow(vars),"label"]<-paste("Normalized to", g.compounds[which(g.compounds$ID==normPeakID),"Name"]) # adjust name of the normalizing peak
  vars[nrow(vars)+1,]<-c("Amount", "bar", FALSE, "Amount [ug]")
  vars[nrow(vars)+1,]<-c("Conc", "bar", FALSE, "Concentration [ug/mg sample]")
  return (vars)
}

# Plot the current data selection from all datasets
datasets.plotDSQHandler<-function(plotsNotebook, datasetIDs, plotVarI, normPeakID=NULL){
  
  plotinfo<-list(
    type="DSQ",
    multiPlot=TRUE,
    datasetIDs=datasetIDs, 
    data=NULL)
  
  # get data and plot options info
  allData<-datasets.aggregateData(datasetIDs, normPeakID=normPeakID)
  plotoptions<-datasets.getDSQPlottingOptions(normPeakID = normPeakID)
  
  if (!is.null(allData)) { # some aggregated data
    relevantData<-allData[which(allData$variable%in%plotoptions$variable), ]
    g.currentData<<-relevantData
    
    # multiple chromatograms
    if (plotoptions$variable[plotVarI] == "Chrom") {
      
      # LOOK more into how to best make this grid work, I didn't really test if regular plot command works with this
      # NOTE: doesn't look like it so might have to reimplement the chrom plotting for this purpose (no zoom possible then here, have two parallel functions?)
      
      #       layout <- matrix(seq(1,  length(datasetIDs)),
      #                        ncol = 1, nrow = length(datasetIDs))
      #       grid.newpage()
      #       pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      #       
      #       for (i in 1:length(datasetIDs)) {
      #         # Get the i,j matrix positions of the regions that contain this subplot
      #         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      #         id<-datasetIDs[i]
      #         data<-g.datasets$Data[[which(g.datasets$ID==id)]]
      #         #print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
      #         dataset.plotDSQHandler(plotsNotebook, datasetID, "datasetName", "datasetType", data, 1, TRUE)
      #       }
      gmessage("sorry not implemented yet")
      #WORKHERE - implement me
    } else if (plotoptions$variable[plotVarI] == "AreaRelP" && is.empty(normPeakID)) { # supposed to normalize to selected peak but it's not possible
      gmessage("Please select a peak to normalize to from the compound list.")
    }  else {
      # case just the relevant variable
      compoundIs<-which(!duplicated(relevantData$CName))
      plotinfo$orderedNames<-relevantData$CName[compoundIs][order(relevantData$Order[compoundIs])]
      plotinfo$orderedDatasets<-relevantData$Dataset[which(!duplicated(relevantData$DatasetID))]
      plotinfo$dataLabel<-plotoptions$label[plotVarI]
      plotinfo$plotType<-plotoptions$plottype[plotVarI]
      plotinfo$isNormalized<-plotoptions$normalized[plotVarI]
      plotinfo$data<-dcast(subset(relevantData, variable==plotoptions[plotVarI, "variable"]), DatasetID + Dataset + Datatype + Weight + WaterdD ~ CName)
      plotinfo$dataView<-data.frame(Data=plotinfo$dataLabel, plotinfo$data) # viewable data
      
      # plot
      plotinfo<-plot.aggregatedData(plotinfo)
      
      # store plotting information
      plots.storeInfo(plotsNotebook, reset=TRUE, plotinfo)
    }
  } else
    gmessage("No data in any of the selected datasets.")
}

# dataset plotting options
datasets.getDPlusPlottingOptions<-function() {
  # FIXME: consider implementing graphing options for "relative to selected peak" areas (already implemented in the data aggregation functions)
  # --> relative to total peaks is tricky because it requires all reference peaks to be guaranteed to be implemented correctly
  vars<-data.frame(
    variable=c("rArea2", "Amp2", "dD_VSMOW", "rR3H2v2H2"),
    plottype=c("bar", "bar", "point", "point"), 
    normalized=FALSE, 
    label=c("Area2 [mV s]", "Amplitude 2 [mV]", "dD vs SMOW [permil]", "rRatio (area 3 / area 2)"), stringsAsFactors=FALSE)
  return (vars)
}

# plotting handler for aggregated DPlus data
datasets.plotDPlusHandler<-function(plotsNotebook, datasetIDs, plotVarI, normPeakID=NULL){
  plotinfo<-list(
    type="DPLUS",
    multiPlot=TRUE,
    datasetIDs=datasetIDs, 
    data=NULL)
  
  # get data and plot options info
  allData<-datasets.aggregateData(datasetIDs, normPeakID=normPeakID)
  plotoptions<-datasets.getDPlusPlottingOptions()
  
  if (!is.null(allData)) { # some aggregated data
    relevantData<-allData[which(allData$variable%in%c(plotoptions$variable,"NormalizedArea")), ] #FIXME: hardcoded normalized data
    g.currentData<<-relevantData
    
    # case just the relevant variable
    compoundIs<-which(!duplicated(relevantData$CName))
    plotinfo$orderedNames<-relevantData$CName[compoundIs][order(relevantData$Order[compoundIs])]
    plotinfo$orderedDatasets<-relevantData$Dataset[which(!duplicated(relevantData$DatasetID))]
    plotinfo$dataLabel<-plotoptions$label[plotVarI]
    plotinfo$plotType<-plotoptions$plottype[plotVarI]
    plotinfo$isNormalized<-plotoptions$normalized[plotVarI]
    plotinfo$data<-dcast(subset(relevantData, variable==plotoptions[plotVarI, "variable"]), DatasetID + Dataset + Datatype + Weight + WaterdD ~ CName)
    plotinfo$dataView<-data.frame(Data=plotinfo$dataLabel, plotinfo$data) # viewable data
    
    # plot
    plotinfo<-plot.aggregatedData(plotinfo)
    
    # store plotting information
    plots.storeInfo(plotsNotebook, reset=TRUE, plotinfo)
  } else
    gmessage("No data in any of the selected datasets.")
}

#####################
# Interactive Plots #
# i.e. zoomHandler  #
#####################

# General zoom handler for any zoomable plots in the notebook
# plotinfo of the plot needs defined
# - zoomable = TRUE
# - zoomHandler = plotting function that returns a plotinfo object
plots.zoomHandler<-function(plotsNotebook, h) {
  plotinfo<-plots.getInfo(plotsNotebook)
  if (!is.null(plotinfo$zoomable) && plotinfo$zoomable==TRUE) { # can zoom
    if (!is.null(plotinfo$clicked) && plotinfo$clicked==TRUE) {
      plotinfo$xlim<-c(min(plotinfo$x1, h$x), max(plotinfo$x1, h$x))
      plotinfo$ylim<-c(min(plotinfo$y1, h$y), max(plotinfo$y1, h$y))
      plotinfo$clicked<-FALSE
      
      # RUN zoom Handler, i.e. the appropriate plotting function
      plotinfo<-do.call(plotinfo$zoomHandler, args=list(plotinfo=plotinfo))
    } else {
      plotinfo$x1<-h$x
      plotinfo$y1<-h$y
      plotinfo$clicked<-TRUE
    }
    plots.storeInfo(plotsNotebook, plotinfo, reset=TRUE)
  }
}

# general unzoom handler for any zoomable plots in the notebook
# - zoomable = TRUE
# - zoomHandler = plotting function that returns a plotinfo object
# - xlimInit, ylimInit
plots.unzoomHandler<-function(plotsNotebook) {
  plotinfo<-plots.getInfo(plotsNotebook)
  if (!is.null(plotinfo$zoomable) && plotinfo$zoomable==TRUE) { # can zoom
    plotinfo$xlim<-plotinfo$xlimInit
    plotinfo$ylim<-plotinfo$ylimInit
    plotinfo<-do.call(plotinfo$zoomHandler, args=list(plotinfo=plotinfo))
    plots.storeInfo(plotsNotebook, plotinfo, reset=TRUE)
  }
}

# reset zoom handlers
plots.resetZoomHandler<-function(plotsNotebook){
  plotinfo<-plots.getInfo(plotsNotebook)
  if (!is.null(plotinfo$zoomable) && plotinfo$zoomable==TRUE) { # can zoom
    plotinfo$clicked<-FALSE
    plots.storeInfo(plotsNotebook, plotinfo, reset=TRUE)
  }
}

###################
# ACTUAL PLOTTING #
###################

# Make mass spectrum plot from plotinfo
# uses 
#   - plotinfo$filePath 
# return plotinfo (but at this stage without additions)
plot.peakSpectrum<-function(plotinfo) {
  
  if (!is.null(plotinfo$filePath) && !is.na(file.info(plotinfo$filePath)$size) ) {
    # load chromatogram info from the file
    load(plotinfo$filePath)
    data<-spectrum$data
    
    # panels - DEPRECATED?
    #data$panel<-3
    #massrange<-ceiling((max(data$Mass)-min(data$Mass))/3)
    #data[which(data$Mass< (min(data$Mass) + 2*massrange)),"panel"]<-2
    #data[which(data$Mass< (min(data$Mass) + massrange)),"panel"]<-1
    
    #cutoff<-0.25
    #FIXME: implement a better strategy for labeling the different peaks (hightest in a cluster each)
    #data[which(data$panel==1 & data$Intensity>cutoff*max(subset(data, panel==1, selec="Intensity"))),"label"]<-TRUE
    #data[which(data$panel==2 & data$Intensity>cutoff*max(subset(data, panel==2, selec="Intensity"))),"label"]<-TRUE
    #data[which(data$panel==3 & data$Intensity>cutoff*max(subset(data, panel==3, selec="Intensity"))),"label"]<-TRUE
    # ggplot
    #p<-ggplot(data, aes(Mass, Intensity/max(Intensity), label=round(Mass))) + 
    #  facet_wrap(~panel, scales="free", ncol=1) + 
    #  geom_bar(stat="identity", width=1) + 
    #  scale_y_continuous(label=percent) + 
    #  theme_bw() + labs(x="Mass [m/z]", y="Relative Abundance", title=paste("Spectrum for peak", plotinfo$RT,"\nfrom file:", spectrum$fileName ,"\nRT range (TIC trace):",spectrum$RTrange[1],"-",spectrum$RTrange[2])) +
    #  geom_text(data=subset(data, label==TRUE), angle=90, size=4, hjust=-0.2, vjust=0.5, colour="red") + 
    #  opts(strip.background = theme_blank(), strip.text.x = theme_blank())
    
    ### plot spectrum
    data$rIntensity<-data$Intensity/max(data$Intensity)*100
    
    # plot limits
    if (is.null(plotinfo$ylim))
      plotinfo$ylim<-c(0, 110)
    plotinfo$ylim[1]<-0 # always zoomed at base
    if (is.null(plotinfo$xlim))
      plotinfo$xlim<-c(min(data$Mass), max(data$Mass))
    
    # store initial limits
    if (is.null(plotinfo$ylimInit))
      plotinfo$ylimInit<-plotinfo$ylim
    if (is.null(plotinfo$xlimInit))
      plotinfo$xlimInit<-plotinfo$xlim  
    
    # make labels
    data$label<-FALSE
    labelintervals<-30
    massinterval<-(plotinfo$xlim[2] - plotinfo$xlim[1])/labelintervals
    for (i in 1:labelintervals) {
      peaks<-which(data$Mass > (plotinfo$xlim[1] + (i-1)*massinterval) & data$Mass <= (plotinfo$xlim[1] + i*massinterval))
      if (!is.empty(peaks)) 
        data[peaks[which(data[peaks,"Intensity"]==max(data[peaks,"Intensity"]))],"label"]<-TRUE
    }
    
    # plotting data
    width<-0.8
    data$xmin<-data$Mass-width/2
    data$xmax<-data$Mass+width/2
    data$ymin<-0
    data$ymax<-data$rIntensity
    labels<-subset(data, label==TRUE)
    
    # actual plot
    plot(plotinfo$xlim, plotinfo$ylim, type = "n",
         main = paste("Spectrum for peak", plotinfo$RT,"\nfrom:", spectrum$fileName ,"\nRT range (TIC trace):",spectrum$RTrange[1],"-",spectrum$RTrange[2]), 
         xlab="Mass [m/z]", ylab="Relative Abundance [%]", xaxs="i", yaxs="i")
    rect(data$xmin, data$ymin, data$xmax, data$ymax, col="grey")
    text(labels$Mass, labels$rIntensity, labels=round(labels$Mass, 1), col="red", srt=90, adj=c(-0.2,0.5), cex=0.9)
    
    #print(p)
    #spectrum<-list(data=data, fileName=gsub("\t","",cb[1,1]), filter=gsub("\t","",cb[2,1]), scanNr=scanNr, RTrange=RTrange, sbNr=sbNr, sbRange1=sb1, sbRange2=sb2)
  } else {
    plot.new()
    text(0.5, 0.5, labels=paste("Spectrum for peak", plotinfo$RT,"\n\nNo MS available."))
  }
  return(plotinfo)
} 

# Make chromatogram plot from plot info
# NOTE: can be used as a standalone function to output a chromatogram from a file!
# uses 
#   - plotinfo$filePath 
#   - plotinfo$peakLabels (need at least RT and Label)
#   - plotinfo$plotName
#   - plotinfo$xlim<-c(xmin, xmax)
#   - plotinfo$ylim<-c(ymin, ymax)
# ... arguments are passed on to the plotting function (e.g. labels or other params for plotting layout)
# returns plotinfo with additional information stored in it (e.g. for zoom handlers)
plot.DSQChrom<-function(plotinfo){
  
  if (!is.null(plotinfo$filePath)) {
    # plot the chromatogram file
    load(plotinfo$filePath)
    peaks<-chrom$peaks[c("apexRT","startRT","endRT","startBG","endBG")]
    names(peaks)<-c("apexX","startX","endX","startY","endY")
    
    # convert to percentage (FIXME: give option?)
    maxS<-max(chrom$data$signal)
    chrom$data$percent<-chrom$data$signal/maxS*100
    peaks$startY<-peaks$startY/maxS*100
    peaks$endY<-peaks$endY/maxS*100
    
    # plot limits
    if (is.null(plotinfo$ylim))
      if (!is.null(plotinfo$peakLabels)) # give extra space if no limits are provided and there are peak labels
        plotinfo$ylim<-c(min(chrom$data$percent), 1.15*max(chrom$data$percent)) # set max to allow enough space for labels
    else
      plotinfo$ylim<-c(min(chrom$data$percent), max(chrom$data$percent))
    if (is.null(plotinfo$xlim))
      plotinfo$xlim<-c(min(chrom$data$time), max(chrom$data$time))
    
    # store initial limits
    if (is.null(plotinfo$ylimInit))
      plotinfo$ylimInit<-plotinfo$ylim
    if (is.null(plotinfo$xlimInit))
      plotinfo$xlimInit<-plotinfo$xlim  
    
    spectrum.plot(chrom$data$time, chrom$data$percent, peaks=peaks, xlim=plotinfo$xlim, ylim=plotinfo$ylim, xlab="Time [min]", ylab="Intensity [%]", peakDelimiters=FALSE, main=plotinfo$plotName) # borrowed from CHROMPARSER
    
    # label peaks
    if (!is.null(plotinfo$peakLabels) && nrow(plotinfo$peakLabels) > 0) {
      if (is.null(plotinfo$peakLabels$Index)) #indices not identified yet --> find them
        plotinfo$peakLabels$Index<-apply(plotinfo$peakLabels["RT"],1,function(x) {diff<-abs(chrom$data$time-x[1]); which(diff==min(diff))[1]}) # get indices of peaks
      text(plotinfo$peakLabels$RT, chrom$data$percent[plotinfo$peakLabels$Index], labels=plotinfo$peakLabels$Label, col=as.character(plotinfo$peakLabels$Color), srt=90, adj=c(-0.2,0.5), cex=0.9)
    }
    
  } else {
    plot.new()
    cat("\nFile", plotinfo$errorInfo, "not found.")
    text(0.5, 0.5, labels=paste("No chromtogram found for this data set:\n\n", plotinfo$errorInfo, "\n\ncould not be located.", sep=""))
  }
  return (plotinfo)
}

# plot area/height sketch for plot
plot.DSQData<-function(plotinfo) {
  
  if ( !is.null(nrow(plotinfo$data)) ) { 
    
    # x limits are 10% of relevant run time added before and after first/last peak
    xlim<-c(
      min(plotinfo$data$RT) - 0.1*(max(plotinfo$data$RT) - min(plotinfo$data$RT)),
      max(plotinfo$data$RT) + 0.1*(max(plotinfo$data$RT) - min(plotinfo$data$RT)))
    
    heightData<-data.frame(RT=c(plotinfo$data$StartRT, plotinfo$data$RT, plotinfo$data$EndRT),  
                           Trace=c(rep(0, times=nrow(plotinfo$data)), plotinfo$data$Height, rep(0,times=nrow(plotinfo$data))))
    
    areaData<-data.frame(
      xmin=plotinfo$data$StartRT, xmax=plotinfo$data$EndRT, ymin=0, ymax=plotinfo$data$Area)
    
    # plotting Height
    heightPlot <- ggplot(heightData, aes(RT, Trace)) + 
      geom_area(fill="gray70",size=0) + 
      geom_line(colour="black") + 
      coord_cartesian(ylim=c(0,max(1.5*heightData$Trace, na.rm=TRUE)), xlim=xlim) + 
      geom_text(data=plotinfo$data, 
                aes(x=RT, y=Height, label=Label, colour=Color), angle=90, size=4, hjust=-0.2, vjust=0.5) +
      labs(x="", y="Peak Height", title=paste(plotinfo$datasetName, " (", plotinfo$datasetType, ")", sep="")) + 
      theme_bw() + theme(legend.position="none")  +
      scale_colour_manual(values=c("black", "blue", "red"))
    
    # plotting Area
    areaPlot <- ggplot(areaData, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) + 
      geom_rect() +
      coord_cartesian(ylim=c(0,max(1.5*areaData$ymax, na.rm=TRUE)), xlim=xlim) +
      geom_text(data=plotinfo$data, 
                aes(x=RT, y=Area, label=Label, colour=Color, xmin=RT, xmax=RT, ymin=Height, ymax=Height), angle=90, size=4, hjust=-0.2, vjust=0.5) +
      labs(x="Retention time [min]", y="Peak Area") + 
      theme_bw() + theme(legend.position="none")  +
      scale_colour_manual(values=c("black", "blue", "red"))
    
    g.currentPlot<<-list(height=heightPlot, area=areaPlot)
    
    # make plot
    multiplot(heightPlot, areaPlot)      
  } else {
    plot.new()
    text(0.5, 0.5, labels=paste("No ", plotinfo$datasetType, " peaklist in this dataset.", sep=""))
  }
  return(plotinfo)
}

# plot DPlus data 
plot.DPData<-function(plotinfo) {
  
  if (!is.null(plotinfo$data) && nrow(plotinfo$data) > 0 ){ # something to plot
    
    xlim<-c(
      min(plotinfo$data$RT) - 0.1*(max(plotinfo$data$RT) - min(plotinfo$data$RT)),
      max(plotinfo$data$RT) + 0.1*(max(plotinfo$data$RT) - min(plotinfo$data$RT)))
    
    #plot
    p<-ggplot(plotinfo$data, aes_string(x="RT", y=plotinfo$variable, label="Name")) + theme_bw() + labs(x="Retention time [sec]", y=plotinfo$label, title=plotinfo$datasetName) 
    
    # bars and points
    if (plotinfo$plottype == "point") {
      ymin<-min(plotinfo$data[plotinfo$variable], na.rm=TRUE)
      ymax<-max(plotinfo$data[plotinfo$variable], na.rm=TRUE)
      ylim<-c(ymin - 0.05*(ymax - ymin), ymax + 0.15*(ymax - ymin))
      p<-p + geom_point(size=5, shape=21, fill="gray", colour="black") 
    } else if (plotinfo$plottype == "bar") {
      ylim<-c(0, 1.15*max(max(plotinfo$data[plotinfo$variable], na.rm=TRUE)))
      p<-p + geom_bar(stat="identity", colour="black", fill="gray") 
    }
    
    #labels
    if (length(unique(plotinfo$data$Name)) > 1 || !is.na(unique(plotinfo$data$Name))) # add labels if there are any
      p <- p + geom_text(angle=90, size=4, hjust=-0.2, vjust=0.5)
    
    # print plot
    g.currentPlot <<- p + coord_cartesian(ylim=ylim, xlim=xlim)
    print(g.currentPlot)
  } else {
    plot.new()
    text(0.5, 0.5, labels=paste("No data. Make sure to import the peaklist and\nselect which peaks (refs and/or non-refs) to plot.", sep=""))
  }
  
  return(plotinfo)
}

# plot aggregated data
plot.aggregatedData<-function(plotinfo) {
  # plot the data for the selected variable (remelt the cast to introduce 0 for the NAs in case of bargraphs)
  plotdf<-melt(plotinfo$data, id=c("DatasetID", "Dataset", "Datatype", "Weight", "WaterdD"))
  
  # figure out data treatment based on plottype
  if (plotinfo$plotType=="bar" && !is.empty(which(is.na(plotdf$value)))) # for bars, replace nonexisting valus with 0
    plotdf[which(is.na(plotdf$value)),]$value<-0
  else if (plotinfo$plotType=="point") # for points, remove NA values
    plotdf<-subset(plotdf, !is.na(value))
  
  # order datasets properly
  plotdf$Dataset<-ordered(plotdf$Dataset, levels=plotinfo$orderedDatasets)
  plotdf$variable<-ordered(plotdf$variable, levels=plotinfo$orderedNames)
  
  # make plot
  if (plotinfo$plotType=="bar") {
    g.currentPlot<<-ggplot(plotdf,aes(variable, value, fill=factor(Dataset))) +
      geom_bar(position="dodge", stat="identity", colour="black") + coord_cartesian(ylim=c(0,max(1.01*plotdf$value, na.rm=TRUE)))
  } else if (plotinfo$plotType=="point") {
    g.currentPlot<<-ggplot(plotdf,aes(variable, value, fill=Dataset)) +
      geom_point(shape=21, size=4) + scale_y_continuous(labels=percent) #FIXME
  } else
    stop(paste("plot type", plotinfo$plotType, "not supported."))
  
  # save as current plot
  g.currentPlot<<-g.currentPlot +  
    labs(x="", y=plotinfo$dataLabel) + theme_bw() + 
    theme(legend.position="bottom", axis.text.x = element_text(angle = 60, hjust = 1)) + 
    guides(fill = guide_legend(title.hjust=0.5, title.position = "top", nrow=ceiling(1/2*length(unique(plotdf$Dataset))))) 
  
  
  # FIXME: abusing the plotting here ? sk fixme
  # Note: maybe actually assemble the plotting code as string entirely and then execute it, this way can let people copy the code for it in order to modify?
  
  # when normalized, convert to percent values
  if (plotinfo$isNormalized) 
    g.currentPlot<<-g.currentPlot + scale_y_continuous(labels=percent)
  print(g.currentPlot)
  
  return(plotinfo)
}

#####################################
# Graphing functions          			#
# These are not all mine, sources   #
# indicated where taken from else-  #
# where                             #
#####################################

##############################
# overwrite funtions         #
# fixme: removed as soon as  #
# fixed in package           #
##############################
gglocator<-function (n = 1, object = last_plot(), message = FALSE, xexpand = c(0.05, 0), yexpand = c(0.05, 0)) {
  if (n > 1) {
    df <- NULL
    for (k in 1:n) {
      df <- rbind(df, gglocator(object = object, message = message, 
                                xexpand = xexpand, yexpand = yexpand))
    }
    return(df)
  }
  x <- grid.ls(print = message)[[1]]
  #x <- x[grep("panel-", grid.ls(print = message)[[1]])] #SK CHANGE
  x <- grid.ls()[[1]][grep("panel", grid.ls(print = message)[[1]])][1] #SK CHANGE
  seekViewport(x)
  loc <- as.numeric(grid.locator("npc"))
  xrng <- with(object, range(data[, deparse(mapping$x)]))
  yrng <- with(object, range(data[, deparse(mapping$y)]))
  xrng <- scales::expand_range(range = xrng, mul = xexpand[1], 
                               add = xexpand[2])
  yrng <- scales::expand_range(range = yrng, mul = yexpand[1], 
                               add = yexpand[2])
  point <- data.frame(xrng[1] + loc[1] * diff(xrng), yrng[1] + 
                        loc[2] * diff(yrng))
  names(point) <- with(object, c(deparse(mapping$x), deparse(mapping$y)))
  point
}


####### useful constants #########
#color-blind friendly palettes:
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Preferred shapes for filling (they can take a fill colour)
prefShapes<-21:25
# e.g.: #ggplot(data.frame(x=1:5, y=10:14, colour=1:5), aes(x=x, y=y, fill=factor(colour), shape=factor(colour))) + geom_point(colour="black", size=5) + scale_shape_manual(values=21:25)

# simple histogram whisker plot combination function 
# data=supplied as a simple vector
plot.whiskerhist<-function(data, xlab, filename="whiskerhist.pdf") {
  maxF=max(hist(data)$counts)
  pdf(filename)
  h<-hist(data,main='',xlab=xlab,ylim=c(0,1.5*maxF))
  boxplot(data,horizontal=TRUE,at=1.125*maxF,add=TRUE,axes=FALSE)
  stripchart(data,add=TRUE,at=1.375*maxF)
  axis(3)
  dev.off()
}

# error bars
plot.addErrorBars<-function(q, err, y, color="black", width=1, linetype=1) {
  if (is.na(color) && is.na(linetype))
    q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width)
  else if (is.na(linetype))
    q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width,color=color)
  else if (is.na(color))
    q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width,linetype=linetype)
  else
    q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width,color=color,linetype=linetype)
  return(q)
}

# scales
plot.addAxes<-function(q, xlim, xbreaks, ylim, ybreaks) {
  if(length(xlim)==2 && length(ylim)==2) { 
    q<-q+coord_cartesian(xlim=xlim, ylim=ylim)
    q<-q+scale_y_continuous(breaks=ybreaks)+scale_x_continuous(breaks=xbreaks)
  } else if(length(ylim)==2) {
    q<-q+coord_cartesian(ylim=ylim)
    q<-q+scale_y_continuous(breaks=ybreaks)
  } else if (length(xlim)==2) {
    q<-q+coord_cartesian(xlim=xlim)
    q<-q+scale_x_continuous(breaks=xbreaks)
  }
  return (q)	
}

# simple background
plot.addSimpleBgrd<-function(q)
  return (q + opts(panel.background = theme_rect(colour = 'black', size = 1.5, linetype='solid'), panel.grid.major = theme_line(colour = 'gray', size = 0.5, linetype = 'solid')))

# standard labels
plot.addLabels<-function(q,main,xlab,ylab,colLegend=NULL,shapeLegend=NULL, lineLegend=NULL, fillLegend=NULL) 
  return (q + opts(title = main) + labs(y = ylab, x = xlab, colour=colLegend, shape=shapeLegend, linetype=lineLegend, fill=fillLegend))

########################
# complex graphs funcs #
########################

# q = a ggplot object which has 
#	- its colour aesthetic set (even if it's =factor(ID) and all colours passed in the colours argument are the same)
# 	- its size aesthetic set to the same distinguisher as the color aesthestic (e.g. some ID, use factor(ID) if discrete)
#	- no geom_line OR geom_line(size=x) called BEFORE geom_point() ;(size has to be fixed in geom_line attribute)
# colors = color_palette used, e.g. c("black", "blue", "red", "dark green")
# order = the order (relative to the colors provided) in which the graphs should appear, e.g. c(4,1,3,2)
# autosave = TRUE: save each plot automatically, FALSE: ask whether to save it
# filename = base filename (number in the animation sequence is added at the end), omit ".pdf"
# ... = additional arguments are all passed on to ggsave (e.g. width, height, dpi)
ggplot.animate<-function(q, order, colors, psize=2.5, autosave=FALSE, filename="animate", ...){
  if (max(order)>length(colors) || min(order) < 1) {
    print("ERROR: order includes indices that are not available. If you provide 4 colours, only provide order numbers 1 through 4, e.g. c(1,4,2,3).")
    return()
  }
  animateSizes<-rep(0,times=length(colors))
  animateColors<-rep("white", times=length(colors))
  for(i in 0:length(order)) {
    if (i>0){
      animateSizes[order[i]]<-psize
      animateColors[order[i]]<-colors[order[i]]
    }
    print(q + scale_size_manual(values=animateSizes, legend=FALSE) + scale_colour_manual(values=animateColors))
    if (autosave==TRUE) {
      save <- "y"
    } else {
      save <- ask(paste("Would you like to save this plot (#", i, ")? Type y for yes, enter for no.", sep=""))
    }
    if (save=="y") {
      name<-paste(filename,"_",i,".pdf",sep="")
      print(paste("Saving plot #", i, " as ", name, sep=""))
      ggsave(filename=name,...)
    }
  }
}

##### BORROWED MULTIPLOT FUCTION #####
# from R cookbook
# Multiple plot function
# NOTE: the layout structure is extremely useful!!
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



####################
# simplicity funcs #
# for + syntax use #
# TODO: figure out how to use multiple ggplot commands (i.e. the + syntax)
# TODO: upgrade to new ggplo2 syntax (a lot of commands are now deprecated)
####################


# make black and white theme (call before calling any other theme vars, sucha s nogrid, adjustX, etc.)
gg.bw<-function() theme_bw()

# removing grid (call after gg.bw, otherwise gg.bw recreates the lines)
gg.nogrid<-function() return(opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank()))

# add title
gg.title<-function(title) opts(title=title)

# adjust x axis position
gg.adjustX<-function(vjust=-0.5, hjust=0, size=12) opts(axis.title.x = theme_text(vjust=vjust, hjust=0.5+hjust, size=size))

# adjust y axis position (note to self: maybe allow adjusting face="bold"?)
gg.adjustY<-function(vjust=0.2, hjust=0, size=12, angle=90) opts(axis.title.y = theme_text(vjust=vjust, hjust=0.5+hjust, angle=angle, size=size))

# change margins
# recommmend using this together with gg.noLegend, otherwise need to adjust the right parameter substantially
gg.noMargins<-function(ylabel=TRUE, xlabel=TRUE, top=0.2, right=-0.5, bottom=-1, left=-1) {
  if (ylabel==TRUE) left<-left+1
  if (xlabel==TRUE) bottom<-bottom+1
  opts(plot.margin = unit(c(top,right,bottom,left), "lines"))
}

# remove legend
gg.noLegend<-function() opts(legend.position="none")




