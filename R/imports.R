############################
# DSQ DATA IMPORT          #
# (Pasting from Excalibur) #
############################

### Pasting MASS SPECTRUM
# associate mass spectrum with peak
excaliburImport.pasteMS<-function(datasetsTable, dataTable) {
  # read clipboard
  cb<-read.clipboard.csv(stringsAsFactors=FALSE, header=TRUE, sep=";", comment.char="$") # read clipboard (avoid commenting out the scan)
  
  # INFO: note - nominal mass is just a sum of all the others
  if (names(cb)[1]!="SPECTRUM...MS") { # very rudiemtnary check that it's the right thing
    gmessage("The clipboard does not seem to contain an Xcalibur mass spectrum. Right click on a mass spectrum --> Export --> Clipboard (Exact Mass).")
    stop("can't process clipboard")
  }
  
  if(!is.empty(selRT<-svalue(dataTable))) { 
    scan<-str_match(cb[3,1],"^(.[^:]*):\\s([0-9]+)-?([0-9]*)")
    scanNr<-as.integer(scan[3:4])
    if (is.na(scanNr[2])) scanNr[2]<-scanNr[1]
    
    rts<-str_match(cb[4,1],"^(.[^:]*):\\s([0-9\\.]+)-?([0-9\\.]*)")
    RTrange<-as.numeric(rts[3:4])
    if (is.na(RTrange[2])) RTrange[2]<-RTrange[1]
    
    sbNr<-NA # number of background data points
    sb1<-NA # range of substracted background 1
    sb2<-NA # range of substracted background 2
    
    if (!is.empty(sbI<-grep("SB",cb[3:10,]))) {
      bgs<-str_match(cb[sbI+2,1],"^(.[^:]*):\\s([0-9]+)\\s\\s([0-9\\.]+)-?([0-9\\.]*)(\\s\\s\\,\\s([0-9\\.]+)-?([0-9\\.]*))?")
      sbNr<-as.integer(bgs[3])
      sb1<-as.numeric(bgs[4:5])
      sb2<-as.numeric(bgs[7:8])
    }
    
    data<-read.clipboard.tab(stringsAsFactors=FALSE, header=TRUE, skip=( grep("Mass", cb[3:10,]) +2 ))
    datasetID<-svalue(datasetsTable)
    filename<-paste("spec_dataset_", datasetID, "_RT_", selRT, ".RData", sep="")
    spectrum<-list(data=data, fileName=gsub("\t","",cb[1,1]), filter=gsub("\t","",cb[2,1]), scanNr=scanNr, RTrange=RTrange, sbNr=sbNr, sbRange1=sb1, sbRange2=sb2)
    
    save(spectrum, file=file.path("spectra", filename))
    return (TRUE)
  }  else {
    gmessage("Please select a peak to associate this mass spectrum with.")
    return (FALSE)
  }
}

### Pasting peak list
# information on DSQ peak list --> very simple, it just has apexRT, start&endRt, the height and area (total and in %)
excaliburImport.pastePeakList <- function(datasetInfoWidgets){
  cb<-read.clipboard.csv(stringsAsFactors=FALSE) # read clipboard
  if (names(cb)!="PEAK.LIST") { # very rudiemtnary check that it's the right thing
    gmessage("The clipboard does not seem to contain a Xcalibur peaklist. Right click on a chromatogram --> Export --> Clipboard (Peak List).")
    stop("can't process clipboard")
  }
  
  if(svalue(datasetInfoWidgets$Excfile)=="" || gconfirm(paste("Do you really want to overwrite the previous data import? ", svalue(datasetInfoWidgets$Excfile)))) { 
    # parse data from clipboard
    data<-data.frame(RT=numeric(), ID=integer(), Name=character(), StartRT=numeric(), EndRT=numeric(), Height=numeric(), Area=numeric(), stringsAsFactors=FALSE)
    for (i in 5:nrow(cb))
      data[nrow(data)+1,c(-2,-3)]<-as.numeric(strsplit(cb$PEAK.LIST[i], "\\\t")[[1]])[c(1:3,6,4)]
    
    # load the clipboard data into the dlg
    widgets.load(datasetInfoWidgets[c("Excfile","Data")], list(Excfile=gsub("\t","",cb$PEAK.LIST[1]), Data=data))  
  }
}

### Paste whole excalibur chromatogram
#FIXME: better to try to catch exceptions on the read clipboard!
excaliburImport.pasteChromatogram<-function(datasetsTable, datasetInfoWidgets, dsAutoPlotHandler){ 
  cb<-read.clipboard.csv(stringsAsFactors=FALSE, header=TRUE) # read clipboard
  if (names(cb)!="CHROMATOGRAM") { # very rudiemtnary check that it's the right thing
    gmessage("The clipboard does not seem to contain a Xcalibur chromatogram. Right click on a chromatogram --> Export --> Clipboard (Chromatogram).")
    stop("can't process clipboard")
  }
  
  if(svalue(datasetInfoWidgets$Excfile)=="" || gconfirm(paste("Do you really want to overwrite the previous data import? ", svalue(datasetInfoWidgets$Excfile)))) { 
    
    # load the filename into the dlg
    fileName<-gsub("\t","",cb$CHROMATOGRAM[1])
    widgets.load(datasetInfoWidgets["Excfile"], list(Excfile=fileName))  
    
    # parse data from clipboard
    cb<-read.clipboard.tab(stringsAsFactors=FALSE, header=TRUE, skip=3) # read clipboard starting with the time and intensity and going from there
    
    # load the clipboard data into the chromatogram browser
    if (svalue(datasetInfoWidgets$Datatype) == "FID") { #FID
      params<-list(smooth=60, noise=1000, width=30) # default params for FID trace
    } else { #TIC
      params<-list(smooth=10, noise=50000, width=1) # default params for TIC trace
    }
    cpinstance<-CP.launch(container=NULL, returnfunc=datasets.cpReturnHandler, 
                          returnargs=list(widgets=datasetInfoWidgets, datasetsTbl=datasetsTable, dsAutoPlotHandler=dsAutoPlotHandler),
                          wintitle=fileName, smooth=params$smooth, noise=params$noise, width=params$width)
    CP.load(cpinstance, cb$Intensity, cb$Time)
  }
}

# Reprocess chromatogram if it is available for this data set
# 
excaliburImport.reprocessChromatogram<-function(datasetsTable, datasetInfoWidgets, dsAutoPlotHandler) { 
  # check if file exists
  datasetID<-svalue(datasetsTable)
  datasetType<-svalue(datasetInfoWidgets$Datatype)
  filename<-paste("chrom_dataset_", datasetID, "_", datasetType, ".RData", sep="")
  file=file.path("chroms", filename)
  if (!is.na(file.info(file)$size) ) { # file exists --> go load it
    load(file)
    cpgui<-CP.launch(container=NULL, returnfunc=datasets.cpReturnHandler, 
                     returnargs=list(widgets=datasetInfoWidgets, datasetsTbl=datasetsTable, dsAutoPlotHandler=dsAutoPlotHandler,
                                     parsePeaks=subset(datasetInfoWidgets$Data[], !is.na(ID), select=c("RT", "ID", "Name"))),
                     wintitle=filename, 
                     solvdelay=chrom$params$solv, smooth=chrom$params$smooth, 
                     width=chrom$params$width, noise=chrom$params$delta, 
                     hcutoff=chrom$params$hcutoff, acutoff=chrom$params$acutoff)
    CP.load(cpgui, chrom$data$signal, chrom$data$time, peaks=chrom$peaks)
  } else
    gmessage(paste("No chromtogram found for this data set (", file, " could not be located). If this DSQDP datafile was copied from another workspace, make sure to copy the chroms folder as well if you want to access the chromatograms.", sep=""))
}

# Process what the chromatogram browser returns
datasets.cpReturnHandler<-function(data, peaks, params, returnargs) { 
  peaklist<-data.frame(RT=numeric(), ID=integer(), Name=character(), StartRT=numeric(), EndRT=numeric(), Height=numeric(), Area=numeric(), stringsAsFactors=FALSE)
  peaklist[1:nrow(peaks), c("RT","StartRT","EndRT","Height","Area")]<-round(peaks[c("apexRT","startRT","endRT")], digits=2)
  peaklist[1:nrow(peaks), c("Height","Area")]<-peaks[c("height","area")]
  peaklist$Area<-peaklist$Area*60 # convert from intensity*min to intensity*s units (that's what excalibur gives)
  if (!is.null(returnargs$parsePeaks))
    peaklist<-data.matchPeaks(peaklist, returnargs$parsePeaks)
  widgets.load(
    returnargs$widgets[c("Data", "Notes")], 
    list(
      Data=peaklist, # load peaks
      Notes=paste("PEAK DETECTION paramters:\nSolvent peak delay:", params$solv,params$solvUnit,"\nSmoothing:", params$smooth, "/ Signal width:", params$width, "/ Noise:", params$delta, "\nHeight cutoff:", params$hcutoff,"% / Area cutoff:", params$acutoff, "\n", svalue(returnargs$widgets$Notes)) # load notes
    ))
  
  # save chromatogram
  datasetID<-svalue(returnargs$datasetsTbl)
  datasetType<-svalue(returnargs$widgets$Datatype)
  filename<-paste("chrom_dataset_", datasetID, "_", datasetType, ".RData", sep="")
  chrom<-list(data=data, peaks=peaks, params=params)
  save(chrom, file=file.path("chroms", filename))
  
  # autoplot if appropriate
  do.call(returnargs$dsAutoPlotHandler, list())
}


########################
# ISODAT DATA IMPORT   #
# (Pasting from Isoat) #
########################

# Add data from peak list in csv file
isodat.pasteCSVFile<-function(datasetInfoWidgets){
  f=gfile(type="open")
  if (!is.na(f)){
    # load data from file
    csv<-read.csv(file=f, head=TRUE, sep=",", stringsAsFactors=FALSE) # read clipboard when pasted from excel
    #FIXME: catch exception here!!!
    if (length(names(csv)) < 2 || names(csv)[1]!="Filename" || names(csv)[2]!="Peak.Nr.") { # very rudiemtnary check that it's the right thing
      gmessage("The CSV file does not seem to contain an Isodat peaklist. Right click on a peak list in Isodat and choose Export ASCII to generate the CSV file for import here.")
      stop("can't process csv file")
    }
    isodat.processPeakList(csv, datasetInfoWidgets)
  }
}

# Add data from peak list
isodat.pastePeakList<-function(datasetInfoWidgets){
  error<-function(e) {
    gmessage("Failed to read clipboard. Please make sure a peaklist is in the clipboard.") 
    stop("can't process clipboard")
  }
  
  tryCatch(
    cb<-read.clipboard.tab(stringsAsFactors=FALSE),
    error=error, warning=error )# read clipboard when pasted from excel
  
  essentialCols<-c("Filename", "Peak.Nr.", "Ref..Name", "Start.s.", "Rt.s.", "End.s.", "Ampl..2.mV.", "Ampl..3.mV.", "BGD.2.mV.", "BGD.3.mV.", "rArea.2.mVs.", "rArea.3.mVs.", "rR.3H2.2H2", "d.2H.1H.per.mil.vs..VSMOW")
  
  if (length(names(cb)) < length(essentialCols)) {
    gmessage(paste(c("The clipboard does not seem to contain the minimal number of essential columns for an Isodat peaklist:", essentialCols), collapse="\n"))
    stop("can't process clipboard")
  }
  
  if (names(cb)[1]!="Filename" || names(cb)[2]!="Peak.Nr.") { # very rudiemtnary check that it's the right thing
    # try fit the peaks to proper headers
    cb <- read.clipboard.tab(stringsAsFactors=FALSE, header=FALSE)
    names(cb)[1:length(essentialCols)] <- essentialCols
    
    # let user choose to import or not
    dlg <- gbasicdialog(title="Check Isodat pasting", expand=TRUE, handler=function(h,...) {
      isodat.processPeakList(cb, datasetInfoWidgets)
    })
    size(dlg)<-c(1600, 800)
    gtable(cb, cont=dlg)
    
    visible(dlg)<-TRUE
  } else 
    isodat.processPeakList(cb, datasetInfoWidgets)
}

# process isodat peaklist
# peaklist - the data frame with the original peak list data (either from clipboard or from csv file)
# information on Delta Plus peak list
# Filename - this is the filename the data is from (this is stored in the complete data set)
# Peak Nr - this is the number of the detected peak, a * after the number means it is a recognized reference peak (only that it is a reference peak is stored)
# Component - can be defined in the method (not used, this assignment is was DSQDP is for)
# Master Peak - if this peak is added by hand, this is the nearest peak, defined to serve as a reference
# Ref  Name - reference name --> imported but not used
# Start (s) - start retention time --> imported
# Rt (s) - apex RT --> imported (and stored both in s and in min as RT.s and RT.min)
# End (s) - end retention time --> imported
# Width (s) - just the difference of End - Start (not imported)
# Ampl  2 (mV) - amplitude of signal 2 at apex RT (imported, ideally with more significant digits)
# Ampl  3 (mV) - amplitude of signal 3 at apex RT (imported, ideally with more significant digits)
# BGD 2 (mV) - background of signal 2 at apex RT (imported, ideally with more significant digits)
# BGD 3 (mV) - background of signal 3 at apex RT (imported, ideally with more significant digits)
# Area All (Vs) - Area 2 + Area 3 (not imported)
# Area 2 (Vs) - area of peak in signal 2 (not imported)
# Area 3 (Vs) - area of peak in signal 2 (not imported), useless because of low sig. digits
# rArea All (mVs) - rArea 2 + rArea3 (not imported)
# rArea 2 (mVs) - 1000*Area2 --> imported, ideally with more sig. digits
# rArea 3 (mVs) - 10^6*Area3 --> imported but still not enough significant digit to calculate everything from
# R 3H2/2H2 - Area2/Area3 --> useless because of low sig. digits
# rR 3H2/2H2 - rArea2/rArea3 --> imported, this is the most useful actual data in terms of sig. digits, but could still use 1 or 2 more for downstream calcs
# rd 3H2/2H2 (per mil) vs  methane ref --> data vs. ref peak (not imported, could be calculated sufficiently well with sig. digits of rR)
# this is calculated by the usual formula, here: (rR 3H2/2H2 / rR_ref - 1)*1000
# where rR_ref is the linear extrapolation of the rR 3H2/2H2 value from the two reference peaks bracketing the given peak
# i.e. rR_ref = rR_CH4before + (rR_CH4after - rR_CH4before)/(RT_CH4after - RT_CH4before) * (RT_peak - RT_CH4before)
# FIXME: check how this is extrapolated for peaks not bracketed by ref peaks
# d 3H2/2H2 (per mil) vs VSMOW --> data vs. VSMOW (not imported, could be calculated precisely from calculated rR 3H2/2H2)
# (R/Rvsmow - 1)*1000, where R/Rvsmow = R/Rref * Rref/Rvsmow = {([rd 3H2/2H2]/1000 + 1) * ([d 3H2/2H2]CH4vsSMOW/1000 + 1) - 1} * 1000
# DeltaDelta 3H2/2H2 (a???) - if the peak is modified/another peak added, this is the difference between the peak and its master peak (not really a useful measure, not imported)
# R 2H/1H - actual D/H ratio of the peak (not imported, could be calculated precisely from d 2H/1H vs SVMOW)
# R = (dD/1000 + 1) * R_VSMOW, where VSMOW = 155.76 ppm (this is the value isodat seems to use)
# d 2H/1H (per mil) vs  VSMOW - the actual dD vs VSMOW (imported but could technically be calculated from d 3H2/2H2 vs SMOW, should be the exact same pretty much!)
# AT% 2H/1H (%) - excess atom percent D (not 100% sure how this is calculated), not imported
# this should be a simple calculation of R/(1+R) but somehow this doesn't actually add up, they calculate not really atom% but some funky excess number
isodat.processPeakList<-function(peaklist, datasetInfoWidgets) {
  #FIXME: yes no dialog!
  if(svalue(datasetInfoWidgets$IsodatFile)=="" || gconfirm(paste("Do you really want to overwrite the previous import of D+ data?", svalue(datasetInfoWidgets$IsodatFile)))) { 
    data<-data.frame(RT=numeric(), ID=integer(), Name=character(), StartRT=numeric(), EndRT=numeric(), 
                     dD_VSMOW=numeric(), PeakNr=integer(), Modified=character(), RefPeak=logical(), RefName=character(),
                     Amp2=numeric(), Amp3=numeric(), BGD2=numeric(), BGD3=numeric(), 
                     rArea2=numeric(), rArea3=numeric(), rR3H2v2H2=numeric(), Filename=character(), stringsAsFactors=FALSE)
    
    peakNr<-str_match(peaklist[["Peak.Nr."]],"^([0-9]+)([\\*\\+]?)$")
    data[1:nrow(peaklist),"PeakNr"]<-peakNr[,2] # peak number
    data[1:nrow(peaklist),"RefPeak"]<-(peakNr[,3]=="*") # whether it is a reference peak
    data[1:nrow(peaklist),"Modified"]<-sapply(peakNr[,3], FUN=function(x) { if (x=="+") "Added" else "No" }) # whether peak is added/edited/etc.
    data[1:nrow(peaklist),c("RefName", "Filename")]<-peaklist[c("Ref..Name", "Filename")]
    data[1:nrow(peaklist),c("RT", "StartRT", "EndRT")]<-peaklist[c("Rt.s.","Start.s.","End.s.")] # rentention times
    data[1:nrow(peaklist),c("Amp2", "Amp3", "BGD2", "BGD3")]<-peaklist[c("Ampl..2.mV.", "Ampl..3.mV.", "BGD.2.mV.", "BGD.3.mV.")] # heights
    data[1:nrow(peaklist),c("rArea2", "rArea3", "rR3H2v2H2")]<-peaklist[c("rArea.2.mVs.", "rArea.3.mVs.", "rR.3H2.2H2")] # areas
    data[1:nrow(peaklist),c("dD_VSMOW")]<-peaklist[c("d.2H.1H.per.mil.vs..VSMOW")] # isotope data
    
    # load the clipboard data into the widgets
    widgets.load(datasetInfoWidgets[c("IsodatFile","Dplus")], list(IsodatFile=paste(unique(peaklist$Filename), collapse="\n"), Dplus=data))  # FIXME: implement using direct widgets load! not this archaic table.dlg.load
  }
}

