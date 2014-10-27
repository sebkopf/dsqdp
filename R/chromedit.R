####################################
# code for Chromatograph processor #
# Copyright 2013 Sebastian Kopf    #
# seb.kopf@gmail.com               #
####################################

# USAGE:
# cpinstance<-CP.launch()
# CP.load(cpinstance, spectrum, [time])
# example: CP.load(CP.launch(width=1, noise=5000, smooth=10), chrom$signal, chrom$time)

##################
# user interface #
##################

# IMPLEMENT ME
# - swithcing from datapoints to mins in solvent peak delay should automatically adjust the range of the spin butotn
# - this should also all happn with smoothing, signal, noise, etc.
# - reimplememt smoothing witdh slightly better --> actually don't use it for preselection but for differentiating neighboring peaks
# --> downside: will be a lot slower

# launch the chromparser
# returns the gui object instance that is important (to be passed on to CP.load to load a specific dataset)
# object is a list with #win, $wxmin, $wxmax, $wymin, $wymax, $gobj and all the peak finding parameter widgets
# return is a function that gets the argument (data, peaks, params, returnargs) passed to it
# default parameters are the valid ones for FID 
CP.launch<-function(container=NULL, returnfunc=NULL, returnargs=NULL, wintitle="Chromatograph Parser", 
                    solvdelay=10, smooth=60, noise=1000, width=30, hcutoff=1, acutoff=0) {
  library(psych)
  library(gWidgets)
  library(zoo)
  options("guiToolkit"="RGtk2")
  
  #CP objects
  cpgui<-list()
  
  # window and frames
  cpgui$win<-gwindow(wintitle, cont=container, width=1024, height=768, visible = FALSE)
  gw<-ggroup(con=cpgui$win, horizontal=FALSE)
  topgrp<-ggroup(cont=gw)
  navframe<-gframe("Navigation", cont=topgrp, expand=FALSE)
  peakframe<-gframe("Peak Detection", cont=topgrp, expand=FALSE)
  opframe<-gframe("Operation", cont=topgrp, expand=FALSE)
  graphframe<-gframe("Graph", container=gw, expand=TRUE)
  
  # navigation panel
  navgrp<-glayout(container=navframe, spacing=10)
  navgrp[1,2]<-(cpgui$wymax<-gedit(0, con=navgrp, width=6, coerce.with=as.numeric))
  navgrp[1,6]<-gbutton("gtk-zoom-in", con=navgrp, handler=function(h,...) { tag(cpgui$gobj, "bestFit")<-FALSE; CP.zoomInOut(cpgui, zoom=50) })  
  navgrp[2,1]<-(cpgui$wxmin<-gedit(0, con=navgrp, width=6, coerce.with=as.numeric))
  navgrp[2,2]<-gbutton("gtk-zoom-100", con=navgrp, handler=function(h,...) { tag(cpgui$gobj, "bestFit")<-FALSE; CP.navReset(cpgui); CP.plot(cpgui) })
  navgrp[2,3]<-(cpgui$wxmax<-gedit(0, con=navgrp, width=6, coerce.with=as.numeric))
  navgrp[2,4]<-glabel("     ", con=navgrp)
  navgrp[2,5]<-gbutton("gtk-go-back", con=navgrp, handler=function(h,...) { CP.moveInterval(cpgui, move=-20, fitMax=tag(cpgui$gobj, "bestFit")) })
  navgrp[2,6]<-gbutton("gtk-zoom-fit", con=navgrp, handler=function(h,...) { tag(cpgui$gobj, "bestFit")<-TRUE; CP.zoomBestFit(cpgui) })
  navgrp[2,7]<-gbutton("gtk-go-forward", con=navgrp, handler=function(h,...) { CP.moveInterval(cpgui, move=20, fitMax=tag(cpgui$gobj, "bestFit")) })
  navgrp[3,2]<-(cpgui$wymin<-gedit(0, con=navgrp, width=6, coerce.with=as.numeric))
  navgrp[3,6]<-gbutton("gtk-zoom-out", con=navgrp, width=6, handler=function(h,...) { tag(cpgui$gobj, "bestFit")<-FALSE; CP.zoomInOut(cpgui, zoom=200) })
  
  # operation panel
  cpgui$op<-gradio(c("Navigation", "Add peak", "Edit peak", "Delete peak"), cont=opframe, handler=function(h,...){ tag(cpgui$gobj, "editPeak")<-NULL; CP.plot(cpgui) })
  
  # peak recognition
  peakgrp<-glayout(container=peakframe, spacing=10)
  peakgrp[1,1]<-glabel("Solvent peak delay: ", con=peakgrp)
  peakgrp[1,2]<-(cpgui$solv<-gspinbutton(from=0, to=100000, by=1, value=solvdelay, con=peakgrp, handler=function(h,...) { CP.solventDelayHandler(cpgui); if (CP.confirmPeakDetect(cpgui)) CP.peakdetectHandler(cpgui) else CP.plot(cpgui) }))
  peakgrp[1,3]<-(cpgui$solvUnit<-gcombobox(c("mins", "datapoints"), active=1, con=peakgrp))
  peakgrp[2,1]<-glabel("Smoothing width: ", con=peakgrp)
  peakgrp[2,2]<-(cpgui$smooth<-gspinbutton(from=1, to=1000, by=10, value=smooth, con=peakgrp, handler=function(h,...) { if (CP.confirmPeakDetect(cpgui)) CP.peakdetectHandler(cpgui) }))
  peakgrp[2,3]<-glabel("Signal width: ", con=peakgrp)
  peakgrp[2,4]<-(cpgui$width<-gspinbutton(from=1, to=1000, by=5, value=width, con=peakgrp, handler=function(h,...) { if (CP.confirmPeakDetect(cpgui)) CP.peakdetectHandler(cpgui) }))
  peakgrp[3,1]<-glabel("Noise intensity: ", con=peakgrp)
  peakgrp[3,2]<-(cpgui$noise<-gspinbutton(from=0, to=100000, by=100, value=noise, con=peakgrp, handler=function(h,...) { if (CP.confirmPeakDetect(cpgui)) CP.peakdetectHandler(cpgui) }))
  peakgrp[4,1]<-glabel("Height cutoff [%]:", con=peakgrp)
  peakgrp[4,2]<-(cpgui$hcutoff<-gspinbutton(from=0, to=100, by=0.5, value=hcutoff, con=peakgrp, digits=1, handler=function(h,...){ if (CP.confirmPeakDetect(cpgui)) CP.cutoffHandler(cpgui) }))
  peakgrp[4,3]<-glabel("Area cutoff [%]:", con=peakgrp)
  peakgrp[4,4]<-(cpgui$acutoff<-gspinbutton(from=0, to=100, by=0.5, value=acutoff, con=peakgrp, digits=1, handler=function(h,...){ if (CP.confirmPeakDetect(cpgui)) CP.cutoffHandler(cpgui) }))
  peakgrp[5,1:2]<-glabel("Show peak delimiters:", cont=peakgrp)
  peakgrp[5,3]<-(cpgui$showPeaks<-gcheckbox("", checked=TRUE, cont=peakgrp, handler=function(h,...){CP.plot(cpgui)}))
  
  # operations
  dlgButtonsGrp<-ggroup(con=opframe, horizontal=FALSE)
  gbutton("Save", con=dlgButtonsGrp, handler=function(h,...) { dev.set(which=dev.prev()); CP.save(cpgui, returnfunc=returnfunc, returnargs=returnargs); dispose(cpgui$win) })
  gbutton("Cancel", con=dlgButtonsGrp, handler=function(h,...) { dev.set(which=dev.prev()); dispose(cpgui$win) })
  
  # graphics object (this will have all the information attached)
  cpgui$gobj<-ggraphics(cont=graphframe)
  tag(cpgui$gobj, "doPlot")<-FALSE # info on whether to plot or not (to control handlers firing when assigning values directly)
  tag(cpgui$gobj, "bestFit")<-FALSE # info on whether to best fit should be used when moving interval
  tag(cpgui$gobj, "addPeak")<-NULL # add peak info list (x, y) - just the coordinate of the first selected point
  tag(cpgui$gobj, "editPeak")<-NULL # edit peak info list (peak=data.frame with peak info and which side of peak selected)
  #addHandlerKeystroke(cpgui$gobj, handler=function(h, key, ...) {print(h)}) #FIXME: doesn't work!! :(
  
  # event handlers for the coordinates (FIXME might be possible to just indicate them in the edit fields directly - do I want that though for pretty code?)
  addHandlerChanged(cpgui$wymax, handler=function(h,...){ CP.plot(cpgui)})
  addHandlerChanged(cpgui$wymin, handler=function(h,...){ CP.plot(cpgui)})
  addHandlerChanged(cpgui$wxmax, handler=function(h,...){ CP.plot(cpgui)})
  addHandlerChanged(cpgui$wxmin, handler=function(h,...){ CP.plot(cpgui)})
  
  # event handlers for the graph
  addHandlerClicked(cpgui$gobj, handler=function(h,...) {
    switch(svalue(cpgui$op,index=TRUE), 
           1==CP.zoomHandler(h, cpgui),# navigation
           2==CP.addPeak(h, cpgui), # add peak
           3==CP.editPeak(h, cpgui), # edit peak
           4==CP.deletePeak(h, cpgui))}) # delete peak
  # FIXME: modify right click handler to do things depending on what is selected in operation (right click in nav resets, right click in edit mode deselects peak and right click in add cancels currently added peak)
  addHandlerRightclick(cpgui$gobj, handler=function(h,...) { 
    switch(svalue(cpgui$op,index=TRUE), 
           1==CP.navReset(cpgui),# navigation
           2==(tag(cpgui$gobj, "addPeak")<-NULL), # add peak
           3==(tag(cpgui$gobj, "editPeak")<-NULL)); CP.plot(cpgui)}) # edit peak
  addHandlerMouseMotion(cpgui$gobj,  handler=function(h,...) {tag(cpgui$gobj, "addPeak")<-NULL; tag(cpgui$gobj, "editPeak")<-NULL; tag(cpgui$gobj,"clicked")<-FALSE}) # reset clicking event when returning to graph
  
  return(cpgui)
}

# laod data into the chromotograph parser
# cpgui - the gui object
# signal = numerical vector of data points
# time [optional] = numerical vector of time coordinates, has to be the same length as signal!!
# peaks [optional] = already defined peaks, this will prevent the peak detect handler from firing right away
CP.load<-function(cpgui, signal, time=NULL, peaks=NULL) {
  tag(cpgui$gobj,"clicked") <- FALSE
  tag(cpgui$gobj,"data")<-data.frame(signal=signal)
  if (!is.null(time)) { #time data is given
    tag(cpgui$gobj,"data")$time<-time
    CP.paramsSet(cpgui, "solvUnit", values=c("mins", "datapoints"), value="mins")
  } else # no time data given
    CP.paramsSet(cpgui, "solvUnit", values=c("datapoints"), value="datapoints")
  
  # save initial copy of data
  tag(cpgui$gobj,"alldata")<-tag(cpgui$gobj,"data")
  
  # set solvent peak delay
  CP.solventDelayHandler(cpgui)
  
  # reset navigation panel
  CP.navReset(cpgui)
  
  tag(cpgui$gobj, "doPlot")<-FALSE # avoid handler triggered plotting
  
  # set peaks if any are defined or find new ones if there are none
  if (!is.null(peaks))
    tag(cpgui$gobj,"allpeaks")<-tag(cpgui$gobj,"peaks")<-peaks
  else 
    CP.peakdetectHandler(cpgui)
  
  # attach plotting event to focus handler (enables even modal dialog loading)
  visHandler <- addHandlerFocus(cpgui$win, handler=function(...) {
    # plot the spectrum on window load
    tag(cpgui$gobj, "doPlot")<-TRUE # avoid handler triggered plotting
    CP.plot(cpgui)
    blockHandler(cpgui$win, ID=visHandler)
  })
  visible(cpgui$win, TRUE)
}

####################
# CP save handlers #
####################

# function to save the peaks with callback function
CP.save<-function(cpgui, returnfunc=NULL, returnargs=NULL) {
  data<-tag(cpgui$gobj,"data")
  peaks<-tag(cpgui$gobj,"peaks")
  
  # IMPORTANT: here the modified peak areas and heights are recalculated!
  peaks[which(peaks$modified==TRUE),]<-spectrum.peakcalcs(data$signal, peaks[which(peaks$modified==TRUE),], time=data$time) 
  
  params<-list(solv=svalue(cpgui$solv), solvUnit=svalue(cpgui$solvUnit), smooth=svalue(cpgui$smooth), width=svalue(cpgui$width), delta=svalue(cpgui$noise), hcutoff=svalue(cpgui$hcutoff), acutoff=svalue(cpgui$acutoff))    
  
  if (!is.null(returnfunc))
    do.call(returnfunc, list(data=data, peaks=peaks, params=params, returnargs=returnargs)) # call the return function with the parameters
}

###########################
# peak detection handlers #
###########################

# Find out if you really want to run peak detection again
CP.confirmPeakDetect<-function(cpgui) {
  return(is.null(tag(cpgui$gobj,"peaks")) || length(which(tag(cpgui$gobj,"peaks")$modified==TRUE)) == 0 || 
           gconfirm("There are peaks that have been added/edited. Rerunning the peak detection algorithm will overwrite any manual changes. Do you wish to proceed?"))
}

# function to set all the peak detection parameters
# param = paramter name ("solv", "solvUnit", "smooth", "width", "noise", "hcutoff", "acutoff")
# values = list of possible values, e.g. seq(2,50, by=2)
# value = selected value (otherwise will try to select what was previously selected)
# for each one, expects a list such that list(range=seq(2,50, by=2), value=5)
CP.paramsSet<-function(cpgui, param, values=NULL, value=NULL) {
  tag(cpgui$gobj, "doPlot")<-FALSE # avoid handler triggered plotting
  if (is.null(value))
    value<-svalue(cpgui[[param]])
  if (!is.null(values))
    cpgui[[param]][]<-values
  svalue(cpgui[[param]])<-value
  tag(cpgui$gobj, "doPlot")<-TRUE # avoid handler triggered plotting
}

# function for changing the solvent delay
# FIXME for this one and the other peak detection triggers --> ask user if they really want to do it if there are custom defined peaks in the peaklist
CP.solventDelayHandler<-function(cpgui) {
  tag(cpgui$gobj, "editPeak")<-NULL
  if (svalue(cpgui$solvUnit)=="mins")  # go by time
    index<-which(tag(cpgui$gobj,"alldata")$time>svalue(cpgui$solv))
  else
    index<-as.integer(svalue(cpgui$solv)+1):nrow(tag(cpgui$gobj,"alldata"))
  tag(cpgui$gobj,"data")<-tag(cpgui$gobj,"alldata")[index,]
}

# wrapper for triggering peak detection based on the input parameters
CP.peakdetectHandler<-function(cpgui) {
  # find peaks
  tag(cpgui$gobj, "addPeak")<-NULL
  tag(cpgui$gobj, "editPeak")<-NULL
  tag(cpgui$gobj,"allpeaks")<-tag(cpgui$gobj,"peaks")<-spectrum.peakdetect(tag(cpgui$gobj,"data")$signal, time=tag(cpgui$gobj,"data")$time, smooth=svalue(cpgui$smooth), width=svalue(cpgui$width), delta=svalue(cpgui$noise))
  CP.cutoffHandler(cpgui)
}

# function for changing height and area cutoffs
CP.cutoffHandler<-function(cpgui) {
  tag(cpgui$gobj, "addPeak")<-NULL
  tag(cpgui$gobj, "editPeak")<-NULL
  
  maxHeight<-max(tag(cpgui$gobj,"allpeaks")[which(!is.na(tag(cpgui$gobj,"allpeaks")$height)),"height"])
  maxArea<-max(tag(cpgui$gobj,"allpeaks")[which(!is.na(tag(cpgui$gobj,"allpeaks")$area)),"area"])
  tag(cpgui$gobj,"peaks")<-tag(cpgui$gobj,"allpeaks")[which(tag(cpgui$gobj,"allpeaks")$height>svalue(cpgui$hcutoff)/100*maxHeight & tag(cpgui$gobj,"allpeaks")$area>svalue(cpgui$acutoff)/100*maxArea),]
  print("Peaks above cutoff:")
  print(tag(cpgui$gobj,"peaks"))
  CP.plot(cpgui)
}

##############################
# peak modification handlers #
##############################

# edit a peak
# changes the previously selected point (well, or closest clicked one) to the new click point and deselects the point
# FIXME: implement this for non-time signal cases!! (i.e. when there's only index, not time signal - should work but double check
CP.editPeak<-function(h, cpgui) {
  peakI<-NULL
  if (!is.null(tag(cpgui$gobj, "editPeak"))) { # already have a peak selected --> change the side that was selected
    peak<-tag(cpgui$gobj, "editPeak")$peak
    peaks<-tag(cpgui$gobj,"peaks")
    newI<-CP.findIndexFromRT(cpgui, h$x)
    sideName<-if (tag(cpgui$gobj, "editPeak")$sideSelected=="end") "EI" else "SI"
    if ((sideName=="SI" && newI<peak$PI) || (sideName=="EI" && newI>peak$PI) ) { # safety precaution to make sure peak does not move outside the apex
      peaks[which(peaks$PI==peak$PI),sideName]<-newI
      if (!is.null(tag(cpgui$gobj, "data")$time)) # time is given, update retention time
        peaks[which(peaks$PI==peak$PI),paste(tag(cpgui$gobj, "editPeak")$sideSelected,"RT", sep="")]<-tag(cpgui$gobj, "data")$time[newI]
      peaks[which(peaks$PI==peak$PI),paste(tag(cpgui$gobj, "editPeak")$sideSelected,"BG", sep="")]<-h$y
      peaks[which(peaks$PI==peak$PI),"modified"]<-TRUE
      tag(cpgui$gobj,"peaks")<-peaks
      peakI<-peak$PI
    }
  }
  
  # replot
  CP.plot(cpgui)
  
  # find selected peak
  if (!is.null(peakI) || !is.empty(peakI<-CP.findPeak(cpgui, h$x))) { # peak already selected or click was inside a peak
    peak<-tag(cpgui$gobj,"peaks")[which(tag(cpgui$gobj,"peaks")$PI==peakI),]
    tag(cpgui$gobj, "editPeak")<-list(peak=peak) # record edit peak
    if (!is.null(tag(cpgui$gobj, "data")$time)) { # time is given --> highlight segments based on retention time
      tag(cpgui$gobj, "editPeak")$sideSelected<-if (peak$apexRT<=h$x) "end" else "start"# record which side was selected
      segments(peak$startRT,peak$startBG,peak$endRT,peak$endBG, col="red", lwd=4)
      if (peak$apexRT<=h$x) # selecting 2nd half of peak
        points(peak$endRT, peak$endBG, col="red", type="p", lwd=10)
      else
        points(peak$startRT, peak$startBG, col="red", type="p", lwd=10)
    }
  } else
    tag(cpgui$gobj, "editPeak")<-NULL # clibk outside a peak
}

# add peak
CP.addPeak<-function(h, cpgui) {
  if (!is.null(tag(cpgui$gobj, "addPeak"))) { # already have selected first point
    peaks<-tag(cpgui$gobj,"peaks")
    row.names(peaks)<-NULL #reset row names to sequential
    
    # add new peak row
    newrow<-nrow(peaks)+1
    newI1<-CP.findIndexFromRT(cpgui, tag(cpgui$gobj,"addPeak")$x1)
    newI2<-CP.findIndexFromRT(cpgui, h$x)
    peaks[newrow,c("modified", "SI", "EI")]<-c(TRUE, min(newI1, newI2), max(newI1, newI2)) # assign starting and ending index
    peaks[newrow,c("fullSI", "fullEI")]<-peaks[newrow,c("SI", "EI")]
    peaks[newrow,"PI"]<-peaks[newrow,"SI"]-1+which(tag(cpgui$gobj, "data")$signal[peaks[newrow,"SI"]:peaks[newrow,"EI"]]==max(tag(cpgui$gobj, "data")$signal[peaks[newrow,"SI"]:peaks[newrow,"EI"]]))[1] # peak index (maximum in the signal start to end range) #index=5
    
    if (!is.null(tag(cpgui$gobj, "data")$time)) # time is given, update retention time
      peaks[newrow,c("startRT", "endRT", "apexRT")]<-c(tag(cpgui$gobj, "data")$time[peaks[newrow,"SI"]], tag(cpgui$gobj, "data")$time[peaks[newrow,"EI"]], tag(cpgui$gobj, "data")$time[peaks[newrow,"PI"]])
    
    if (peaks[newrow,c("SI")]==newI1) #assign starting and ending background
      peaks[newrow,c("startBG", "endBG")]<-c(tag(cpgui$gobj,"addPeak")$y1, h$y)
    else
      peaks[newrow,c("startBG", "endBG")]<-c(h$y, tag(cpgui$gobj,"addPeak")$y1)
    
    tag(cpgui$gobj,"peaks")<-peaks
    tag(cpgui$gobj, "addPeak")<-NULL
    
    # replot
    CP.plot(cpgui)
  } else { # select first point
    if (!is.empty(peakI<-CP.findPeak(cpgui, h$x))){ # found a peak at selection point
      CP.deletePeak(h, cpgui, confirmmsg="There is already a peak here. Do you want to delete the existing peak?")
    } else { # nothing here yet, can adda peak!
      tag(cpgui$gobj, "addPeak")<-list(x1=h$x, y1=h$y)
      points(h$x, h$y, col="red", type="p", lwd=10)     
    }
  }
}

# delete peak
CP.deletePeak<-function(h, cpgui, confirmmsg="Do you really want to delete this peak?") {
  if (!is.empty(peakI<-CP.findPeak(cpgui, h$x))){ # found a peak
    peak<-tag(cpgui$gobj,"peaks")[which(tag(cpgui$gobj,"peaks")$PI==peakI),]
    if (!is.null(tag(cpgui$gobj, "data")$time)) # time domain
      segments(peak$startRT,peak$startBG,peak$endRT,peak$endBG, col="red", lwd=4) # highlight selected peak
    if (gconfirm(confirmmsg, cont=cpgui$win))
      tag(cpgui$gobj,"peaks")<-tag(cpgui$gobj,"peaks")[which(tag(cpgui$gobj,"peaks")$PI!=peakI),] # delete peak
    CP.plot(cpgui)
  }
}

# find apex index of peak that belongs to the x coordinate (return NULL if no peak associated with this)
CP.findPeak<-function(cpgui, x) {
  if (!is.null(tag(cpgui$gobj, "data")$time)) {# time signal
    xs<-tag(cpgui$gobj, "data")$time
    peaks<-tag(cpgui$gobj,"peaks")[c("startRT","endRT", "PI")]
  } else { # no time signal
    xs<-1:nrow(tag(cpgui$gobj,"data")) 
    peaks<-tag(cpgui$gobj,"peaks")[c("SI","EI","PI")]
  }
  return(peaks[which(peaks[[1]]<=x & peaks[[2]]>=x),"PI"])
}

# find index from RT (or if index passed in, index is returned)
CP.findIndexFromRT<-function(cpgui, RT) {
  if (!is.null(tag(cpgui$gobj, "data")$time)) {# time signal
    diff<-abs(tag(cpgui$gobj, "data")$time-RT)
    return (which(diff==min(diff))) 
  } else # no time signal
    return (RT) # this is index
}

#######################
# navigation handlers #
#######################

# set the navigation panel (can set each one idependently)
CP.navSet<-function(cpgui, xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL) {
  tag(cpgui$gobj, "doPlot")<-FALSE # avoid handler triggered plotting
  if (!is.null(xmin))
    svalue(cpgui$wxmin)<-xmin
  if (!is.null(xmax))
    svalue(cpgui$wxmax)<-xmax
  if (!is.null(ymin))
    svalue(cpgui$wymin)<-ymin
  if (!is.null(ymax))
    svalue(cpgui$wymax)<-ymax
  tag(cpgui$gobj, "doPlot")<-TRUE
}

# reset the navigation panel to the min and maxes
CP.navReset<-function(cpgui) {
  tag(cpgui$gobj,"clicked")<-FALSE
  CP.navSet(cpgui, ymin=min(tag(cpgui$gobj,"data")$signal), ymax=max(tag(cpgui$gobj,"data")$signal), xmin=1, xmax=length(tag(cpgui$gobj,"data")$signal))
  if (!is.null(tag(cpgui$gobj,"data")$time))  #time data is given
    CP.navSet(cpgui, xmax=round(max(tag(cpgui$gobj,"data")$time)), xmin=round(min(tag(cpgui$gobj,"data")$time)))
}

# move viewing interval
# move is the percent (+ or -) of moving the size of the current inverval
CP.moveInterval<-function(cpgui, move=0, fitMin=TRUE, fitMax=TRUE) {
  xmin<-svalue(cpgui$wxmin)
  xmax<-svalue(cpgui$wxmax)
  CP.navSet(cpgui, xmin=(xmin+move/100*(xmax-xmin)), xmax=(xmax+move/100*(xmax-xmin)))
  CP.zoomBestFit(cpgui, fitMin=fitMin, fitMax=fitMax)
}

# zoom in and out (does not change zoom on minimum!)
# zoom is percent zoom level
CP.zoomInOut<-function(cpgui, zoom=100) {
  CP.navSet(cpgui, ymax=round(svalue(cpgui$wymin) + zoom/100*(svalue(cpgui$wymax) - svalue(cpgui$wymin))))
  CP.plot(cpgui)
}

# zoom to best fit for min and max signal
CP.zoomBestFit<-function(cpgui, fitMin=TRUE, fitMax=TRUE) {
  xmin<-svalue(cpgui$wxmin)
  xmax<-svalue(cpgui$wxmax)
  interval<-round(xmin):round(xmax)
  if (!is.null(tag(cpgui$gobj,"data")$time)) # there is a time signal
    interval<-which(tag(cpgui$gobj,"data")$time>=xmin & tag(cpgui$gobj,"data")$time<=xmax)
  if (fitMin)
    CP.navSet(cpgui, ymin=min(tag(cpgui$gobj,"data")$signal[interval]))
  if (fitMax)
    CP.navSet(cpgui, ymax=max(tag(cpgui$gobj,"data")$signal[interval]))
  CP.plot(cpgui)
}

# handler for zooming
CP.zoomHandler<-function(h, cpgui){
  if (tag(cpgui$gobj, "clicked")) {
    CP.navSet(cpgui, xmin=min(tag(cpgui$gobj,"x"), h$x), xmax=max(tag(cpgui$gobj,"x"), h$x))
    tag(cpgui$gobj, "bestFit")<-FALSE; # reset best fit handling
    CP.plot(cpgui)
    tag(cpgui$gobj,"clicked")<-FALSE
  } else {
    tag(cpgui$gobj,"x")<-h$x # save both x and y
    tag(cpgui$gobj,"y")<-h$y
    tag(cpgui$gobj,"clicked")<-TRUE
  }
}

############
# plotting #
############

# wrapper plotting functionf or the chromatograph processor (pulling all information out of the GUI objects)
CP.plot<-function(cpgui) {
  if (tag(cpgui$gobj, "doPlot")) {
    data<-tag(cpgui$gobj,"data")
    y<-data$signal # plot the signal
    xlim=c(svalue(cpgui$wxmin), svalue(cpgui$wxmax)) # x min and max
    ylim=c(svalue(cpgui$wymin), svalue(cpgui$wymax)) # y min and max
    if (!is.null(data$time)) { # there is a time signal --> convert to time signal
      x<-data$time
      xlab<-"Time [mins]"
      peaks<-tag(cpgui$gobj,"peaks")[c("apexRT","startRT","endRT","startBG","endBG")]
    } else { # no time signal
      x<-1:nrow(data) # plot against index by default
      xlab<-"Data point"
      peaks<-tag(cpgui$gobj,"peaks")[c("PI","SI","EI","startBG","endBG")]
    }
    names(peaks)<-c("apexX","startX","endX","startY","endY")
    spectrum.plot(x, y, peaks=peaks, xlim=xlim, ylim=ylim, xlab=xlab, ylab="Intensity", peakDelimiters=svalue(cpgui$showPeaks))
  }
}

# plotting function for a chromatogram
# outputs in the currently active plotting device
# (can also be used indpendently from the user interface)
# ... arguments passed on to plot (e.g. main or other plotting parameters)
spectrum.plot<-function(x, y, peaks=NULL, xlim=NULL, ylim=NULL, xlab="x", ylab="y", peakDelimiters=TRUE, ...){
  if (is.null(xlim)) xlim<-c(min(x), max(x))
  if (is.null(ylim)) ylim<-c(min(y), max(y))
  plot(0,0, type="l", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...)
  if (!is.null(peaks)) { # plot the peaks on top of the chromatogram
    if (peakDelimiters) { # plot the vertical lines
      abline(v=peaks$apexX, col="red", lty=2)
      abline(v=peaks$startX, col="blue", lty=2)
      abline(v=peaks$endX, col="blue", lty=2)
    }
    segments(peaks$startX,peaks$startY,peaks$endX,peaks$endY, col="red")
  }
  lines(x,y)
}

###############
# computation #
###############

# function to find peaks (can also be used independently from the user interface)
# signal
# time signal (same indices as signal for integrating area correctly)
# smooth = across how many data points the signal is smoothed (unit: data points, minimum is 1)
# delta = what to consider the proper steepness to consider a peak (unit: signal change over smoothing range)
# width = minimum distance between between neighboring peaks to be combined into one peak
# return: list of all the peaks with indices (not the time signals), if time is given, then areas are in units of intensity*time (convert afterwards from min to s if necessary!!)
spectrum.peakdetect<-function(signal, time=NULL, smooth=60, width = 10, delta = 500) {
  
  #test dataset
  #signal<-c(5,1,2,6,5,4,3,2,1,5,5,4,5,6,17,15,8,7,9,11,18,21,25,29,23,4,2,3,2,3,4,6,9,12,6,3,2,1,5,12)
  change<-diff(signal, lag=smooth)
  posI<-which(change>delta)+floor(smooth/2) #indices of steps that are larger than noise and positive (split smoothing interval so that indices represent roughly the center of the interval)
  negI<-which(change<(-1)*delta)+floor((smooth+1)/2) #indices of steps that are smaller than noise and negative
  
  startI<-c(posI[1],posI[which(diff(posI)>width)+1]) #indices of large steps that are at the beginning of peaks
  endI<-c(negI[which(diff(negI)>width)],negI[length(negI)]) #indices of large steps that are the end of peaks
  startChange<-change[startI-floor(smooth/2)] #the actual change intervals associated with the position
  endChange<-change[endI-floor((smooth+1)/2)] #the actual change intervals associate witht the positions
  
  # figure out all the peak limits from the collected indices
  plims<-data.frame(idx=c(endI, startI), change=c(endChange, startChange))
  plims<-plims[order(plims$idx),] # reorder by index
  plims$pstart<-plims$change>0 # record which ones are starts and ends of peaks
  
  # consecutive ends or beginnings of peaks not alternated with the opposite (end vs start of peak) are collapsed to just the last start and first end
  dups<-which(diff(plims$pstart)==0) # consecutive duplicates (start-start and end-end transitions)
  if (length(dups)>0) { # if there were any duplicates found
    startExcl<-dups[which(plims$pstart[dups])] # start-start transitions to exclude (+1 to only take first peak start, +0 to only take last peak start)
    endExcl<-dups[which(!plims$pstart[dups])]+1 # end-end transitions to exclude (+1 to only take first peak end, +0 to only take last peak end)
    plims<-plims[c(-startExcl,-endExcl),] # remove the consecutive duplicates
  }
  
  # figure out which peaks are overlapping within the width interval and store for peak processing
  plims$overlap<-c(diff(plims$idx)<=width,FALSE)
  plims$overlap[nrow(plims)-1]<-FALSE # make sure last peak doesn't try to overlap
  
  # collect and process identified peaks
  peaks<-data.frame(SI=integer(), EI=integer(), fullSI=integer(), fullEI=integer())
  overlap<-FALSE
  peakgrp<-NULL
  for (i in 1:nrow(plims)) {
    index<-plims$idx[i] 
    if (plims$pstart[i]) { # start of a peak
      if (!overlap) # if no overlap, start new peakgrp
        peakgrp<-data.frame(SI=index, EI=NA, fullSI=index, fullEI=NA)
      else # if overlap, continue exisiting peak group
        peakgrp<-rbind(peakgrp,data.frame(SI=peakgrp[nrow(peakgrp),"EI"],EI= NA,fullSI= NA,fullEI= NA))
    } else if (!is.null(peakgrp)) { # end of a peak and peakgrp defined
      if (plims$overlap[i]) { # there is overlap with the next peak
        overlapS<-signal[index:plims$idx[i+1]] # get signal between the peaks
        peakgrp[nrow(peakgrp),"EI"]<-(index-1+which(overlapS==min(overlapS))[1]) # find absolute local minimum between the peaks
        overlap<-TRUE
      } else { # no overlap --> close peak grp
        peakgrp[nrow(peakgrp),"EI"]<-index
        peakgrp$fullSI<-peakgrp[1,"SI"]
        peakgrp$fullEI<-peakgrp[nrow(peakgrp),"EI"]
        peaks<-rbind(peaks,peakgrp)
        peakgrp<-NULL
        overlap<-FALSE
      }
    }
  }
  
  # peak optimization and additional information
  peaks$PI<-apply(peaks,1,function(x) {x[1]-1+which(signal[x[1]:x[2]]==max(signal[x[1]:x[2]]))[1]}) # peak index (maximum in the signal start to end range) #index=5
  peaks$SI<-apply(peaks,1,function(x) {x[1]-1+which(signal[x[1]:x[5]]==min(signal[x[1]:x[5]]))[1]}) # adjust to absolut minimum in the range from peak start to peak apex #index=1
  peaks$EI<-apply(peaks,1,function(x) {x[5]-1+which(signal[x[5]:x[2]]==min(signal[x[5]:x[2]]))[1]}) # adjust to absolut minimum in the range from peak apex to peak end #index=2
  peaks$diffES<-(peaks$SI - c(0,peaks$EI[-nrow(peaks)])) #calculate differences in indices between peaks start to end (a value of 0 means the peak is border the previous peak)
  peaks$diffSE<-(c(peaks$SI[-1],0) - peaks$EI) # calculate differences in indices between peaks end to start (a value of 0 means the peak is border the next peak)
  peaks$fullSI<-apply(peaks,1,function(x) {if(x[6]!=0) x[1] else min(x[1], x[3]-1+which(signal[x[3]:x[5]]==min(signal[x[3]:x[5]]))[1])}) # adjust to absolut minimum in the range from peak group start to peak apex #index=3
  peaks$fullEI<-apply(peaks,1,function(x) {if(x[7]!=0) x[2] else max(x[2], x[5]-1+which(signal[x[5]:x[4]]==min(signal[x[5]:x[4]]))[1])}) # adjust to absolut minimum in the range from peak apex to peak group end #index=4
  peaks$startBG<-apply(peaks,1,function(x) {if(x[6]!=0) signal[x[1]] else min(signal[x[1]], signal[x[3]]+(x[1]-x[3])/(x[4]-x[3])*(signal[x[4]]-signal[x[3]]))}) # BG at start of peak (extrapolated if necessary) #index=6
  peaks$endBG<-apply(peaks,1,function(x) {if(x[7]!=0) signal[x[2]] else min(signal[x[2]], signal[x[3]]+(x[2]-x[3])/(x[4]-x[3])*(signal[x[4]]-signal[x[3]]))}) # BG at end of peak (extrapolated if necessary) #index=7
  peaks<-spectrum.peakcalcs(signal, peaks, time=time) # calculate height and area
  
  # comput retention times if time domain is given
  if (!is.null(time)) 
    peaks[, c("startRT","endRT","apexRT")]<-(apply(peaks[,c("SI", "EI", "PI")],2,function(col) {time[col]})) # convert indices to time domain
  else
    peaks[, c("startRT","endRT","apexRT")]<-NA
  
  peaks$modified<-FALSE # introduce flag for peak modification
  return(peaks)
}

# calculate height and area of the peaks
spectrum.peakcalcs<-function(signal, peaks, time=NULL) {
  peaks$apexBG<-apply(peaks[,c("SI", "EI", "fullSI", "fullEI", "PI")],1,function(x) {signal[x[3]]+(x[5]-x[3])/(x[4]-x[3])*(signal[x[4]]-signal[x[3]])})# BG at peak center (PI), calculated from extrapolation #index=8
  peaks$height<-signal[peaks$PI]-peaks$apexBG# peak height (BG subtracted) #index=9
  
  # compute peak area
  if (!is.null(time)) {
    peaks$areaBG<-apply(peaks[,c("SI", "EI", "startBG", "endBG")],1,function(x) {(time[x[2]]-time[x[1]])*(x[3]+x[4])/2})# BG of peak area using the time interval #index=10
    peaks$area<-apply(peaks,1,function(x) { if (x[1]<x[2]) sum(diff(time[x[1]:x[2]])*rollmean(as.numeric(signal[x[1]:x[2]]),2))}) - peaks$areaBG # peak area using the time interval (G subtracted) #index=11
  } else {
    peaks$areaBG<-apply(peaks[,c("SI", "EI", "startBG", "endBG")],1,function(x) {(x[2]-x[1])*(x[3]+x[4])/2})# BG of peak area using indices #index=10
    peaks$area<-apply(peaks,1,function(x) { if (x[1]<x[2]) sum(diff(x[1]:x[2])*rollmean(signal[x[1]:x[2]],2))}) - peaks$areaBG # peak area using indices (G subtracted) #index=11
  }
  
  return (peaks)
}

#################
# UTILITY FUNCS #
#################
is.empty<-function(variable)
  return (is.na(variable) || is.null(variable) || length(variable) == 0)


