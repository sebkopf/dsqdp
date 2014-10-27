###################################
# code for installing DSQDP        #
# Copyright 2013 Sebastian Kopf    #
# seb.kopf@gmail.com               #
####################################

# installation function 
# param: installation path
DSQDP.install<-function() {
  local({# set mirror to berkely
    r <- getOption("repos")
    r["CRAN"] <- "http://cran.cnr.berkeley.edu/"
    options(repos = r)
  }) 
  cat("Installing required packages.\n")
  install.packages("ggplot2", depen=TRUE) # for advanced plotting, includes reshape2 and plyr, is quick
  install.packages("plyr", depen=TRUE)
  install.packages("zoo", depen=TRUE) # for rolling means calculations
  install.packages("psych", depen=TRUE) # for reading the from the clipboard, fairly quick
  install.packages("sqldf", depen=TRUE) # for running sql statements on data frames, quick
  install.packages("gWidgets", depen=TRUE) # for user interfaces, quick
  update.packages(ask=FALSE)
  cat("All required packages installed.\n")
}


###################
### GLOBAL VARS ###
###################
# update data sets by setting (or overwriting) an existing dataset field
DSQDP.addDataSetField<-function(field, defaultVal) {
  g.datasets[[field]]<<-rep(defaultVal, times=length(g.datasets$ID))
}

# remove an existing data set field
DSQDP.removeDataSetField<-function(field) {
  g.datasets[[field]]<<-NULL
}

# package DSQDP
DSQDP.package<-function(filename="DSQDP.R", dir="/Users/SKOPF/Dropbox/Tools/software/R/DSQDPdev") {
  funcsdir<-"/Users/SKOPF/Dropbox/Tools/software/R/funcs"
  funcsfiles<-c("SKGUILIB.R", "SKDATALIB.R", "SKUTILLIB.R", "SKPLOTLIB.R")
  dsqdpdir<-dir
  dsqdpfiles<-c("DSQDP.main.R", "DSQDP.plots.R", "DSQDP.compounds.R", "DSQDP.peaklists.R", "DSQDP.imports.R", "DSQDP.datasets.R")
  files<-c(file.path(dsqdpdir, dsqdpfiles), file.path(funcsdir, funcsfiles))
  
  header<-paste(
    "##############################\n",
    "# PACKAGED DSQ DataProcessor #\n",
    "# ", format(Sys.time(), "%Y-%m-%d"), " by SKOPF        #\n",
    "##############################\n\n", sep="")
  for (i in 1:length(files)) 
    files[i]<-readChar(files[i], file.info(files[i])$size)
  cat(header, files, file=file.path(dir, filename), sep="\n\n\n")
}

# start DSQDP - will ask for working directory
# exitR - whether to exit R upon closing of the DSQDP data processor
# sourcecodeDir - the directory that holds the source code (absolute path), by default will try to source the code in the selected working directory
#   --> if you have a single sourcebase somewhere, supply absolute path
# cleanWD - whether to clean up the current working directory before starting DSQDP (remove all variabes and functions before reloading the data)
DSQDP<-function(exitR=TRUE, sourcecodeDir=".", cleanWD=TRUE) {
  print(sourcecodeDir)
  library(gWidgets)
  options("guiToolkit"="RGtk2")
  f=gfile("Select your DSQDP workspace folder.", type="selectdir")
  if (!is.na(f)){
    save.image() # save previous workspace
    if(cleanWD)
      rm(envir=.GlobalEnv, list=ls(envir=.GlobalEnv)[which(!(ls(envir=.GlobalEnv)%in%c("DSQDP.install", "DSQDP", "DSQDPdev", "DSQDPactive", ls())))]) # clean previous workspace (except functions in this file and variables in this function)
    setwd(f)
    cat("\nDSQDP workspace loaded:", getwd(), "\n")
    source(file.path(sourcecodeDir,"CHROMPARSE.R"))
    source(file.path(sourcecodeDir,"DSQDP.R"))
    cat("\nDSQDP source files loaded.\n")
    if (is.na(file.info("backups")$size)) dir.create("backups") # data backups folder
    if (is.na(file.info("chroms")$size)) dir.create("chroms") # chromatograms folder
    if (is.na(file.info("spectra")$size)) dir.create("spectra") # mass spectra
    if (is.na(file.info("graphs")$size)) dir.create("graphs") # graphs folder
    if (is.na(file.info("exports")$size)) dir.create("exports") # exports folder
    
    # load data in workspace (DSQDP.RDATA)
    if (!is.na(file.info("DSQDP.RDATA")$size)) { # try to load the DSQDP dataset that's in the directory
      load("DSQDP.RDATA", .GlobalEnv)
      cat("\nWorkspace DSQDP.RDATA dataset loaded.\n")
    } else if (!is.na(file.info(file.path(sourcecodeDir,"DSQDP.RDATA"))$size)) { # failing that, try to load from sourcecode directory
      load(file.path(sourcecodeDir,"DSQDP.RDATA"), .GlobalEnv)
      gmessage(paste("Welcome to starting DSQDP for the first time in this workspace. To reload the data stored in this workspace in the future, please go back into this exact same workspace directory on startup: ", getwd()))
      cat("\nInitial DSQDP.RDATA dataset loaded.\n")
    } else {
      gmessage(paste("There does not seem to be a DSQDP.RDATA file in this workspace or the sourcecode directory", sourcecodeDir, "to initialize the program with. Please copy a DSQDP.RDATA file into the workspace or sourcecode directory."))
      stop("no DSQDP.RDATA file available to initialize with")
    }
    
    if (g.currentVersion > DSQDP.getVersion()$version) { # make sure codebase is up for the task of processing this data
      gmessage(paste("The data in this instance of DSQDP was generated in version", g.currentVersion, "but the codebase is only version", DSQDP.getVersion()$version,"- please update the codebase before running DSQDP."))
      stop("codebase too old for data")
    }
    cat("\nChecking for updates to current version.\n")
    DSQDP.update()
    cat("\nLaunching DSQDP.\n\n")
    DSQDP.launch(exitR=exitR)
  }
}


##############################
# PACKAGED DSQ DataProcessor #
# 2014-01-14 by SKOPF        #
##############################




#############################################
# code for DSQ/Delta Plus data analyzer     #
# Copyright 2013 Sebastian Kopf             #
# seb.kopf@gmail.com                        #
#############################################

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this program and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

######################
# Save and Load Data #
######################

# save all the important global variables into an R data dump
DSQDP.saveData<-function(backup=TRUE){
	cat("\nSaving Workspace.\n")
	save.image()
  filename<-NULL
  if (backup) {
	  cat("\nBacking up DSQDP Data...\n")
	  if (is.na(file.info("backups")$size)) dir.create("backups")
	  filename<-paste("DSQDP_Data_",format(Sys.time(),format="%Y%m%d_%H%M%S"),".RData", sep="")
	  save(g.compounds, g.peakLists, g.datasets, g.currentVersion, file=file.path("backups", filename))
	  cat(paste(filename,"created.\n\n"))
  }
	cat("\nSaving DSQDP.RDATA\n")
	save(g.compounds, g.peakLists, g.datasetLists, g.datasets, g.currentVersion, file=file.path("DSQDP.RDATA"))
	return(filename)
}

# load all the global variables from an R data dump
# if using the R interface, this can also be accomplished simply by double clicking on the file while in the right workspace
DSQDP.loadData<-function(filename) {
	cat("\nLoading DSQDP Data\n")
	if (is.na(file.info(filename)$size)) 
		stop(paste("File", filename,"does not exist. Please make sure to pass a proper DSQDP Data file to this function."))
	load(filename)
	cat(paste(filename,"loaded\n\n"))
}

################
# MAIN PROGRAM #
################

# V3 gui overhaul!!
# This vision has the peak matching right in the chromparser (extended chromparse) and separate screens for managing the compounds (or maybe just separate tabs?)
# - top level menu item for switching to compounds database (manage compounds, warning when editing names! also has spectra, etc. - also warning when editing!)
# - top level menu item for switching to peak lists (can copy between FID/TIC/DPLUS w/ conversion factor, edit lists, create new ones based on a dataset)
# - top level menu item for switching to datasets (just datatsets, can search them, edit them (main params, not even the peak table itself), graph them - just 2 columns for more graphing space!) --> compounds dropdown for IS (only defined ones allowed) and for specific peak normalization (only the ones contained in any of the selected datasets are allowed)
# - implement the peak matching in chromparse
#   --> chromparser opens as usual for pasted chroms (opens with pseudo graph if pasted peaks - pseudograph has both imitated chrom with peak heights and a bar graph with the areas below)
#   --> but in addition below the graph is the whole peak list updated in real time as changes are made to the chrom (try to fit whatever identified peaks there are always before re-integration again afterwards)
#   --> peak lists and compounds passed to chromparse, displayed next to table as usual (just dropdown for peak lists?, searchable list for compounds), can assign as needed with live update on graph
#   --> once all is done, can save and table is displayed in main screen
#   --> for editing, click edit on main screen, opens chromparse back up but with peak integration locked (can unlock with warning that this is tricky and might loose peaks)
#   --> also allow adding mass spectra to the chrom (which will display in a small graphing window on the right for easy comparison with the compounds library)
# - for ISODAT also implement this for direct isodat files reading (see if integrations all work okay in comparision)
#   --> have plotting tab at top to allow switching from chrom (real chrom or stylized height / area barchart dual graph) to isotopic plot (showing rR and dD)
# graphing options for individual datasets
#   --> just chromatogram with labels OR area/heigh hybrid bar and peak schematic chart
#   --> just normalization options here
#   --> same for D+ but there also option to plot other params (or multiple, all implemented with checkboxes)
# this way allow having multiple datasets open at the same time (different windows for each, not a modal dialog, make sure to manage the graphing devices properly!)
# CONSIDER alternatively also to have an editable big table (more like a spreadsheet) where all the auxiliary information about a dataset is stored (and can be edit and amended much like an excel spreadsheet so people can add columns they like, etc) - careful that ID remains unique?
#   --> potentially even set this up with tabs so that people can really use it in a spreadsheet like manner where they can add whatever extra properties they want
#   --> also allow easy copy and pasting of multiple columns, rows, etc
#   --> also implement UNDO (every important for something like this)
# also in this table allow multiple selections (And then on autoplot, plot them all together or if single select, just one)

# FIXME: feature - implement custom attributes for datasets (e.g. selecting C and then being able to plot off of that)
# FIXME: implement mass spectra comparisons
#   NOTE: store mass spectra in special files for both compounds and for peaks in peak list (if double clicking on peak in peak list you can get the comparison with the currently selected compound)
#   NOTE: implement proper understanding of double clicks (write how to guide)
# FIXME: implement structures for compounds --> use notation used in NIST library (locations of atoms, together with indicators for bonds) - stored in chemical structures examples
# FIXME: HIGH PRIORITY
#   - implement better plotting that combines both datasets
#   --> have a 3rd plotting tab (for plotting both)
#   --> checkboxes for Area/Height for DSQ and rest for D+, as well as scaling factor (radiobutton for DSQ vs D+) and number to scale the RT by
#   --> this will make facet grid with all the diffrent selections atop of each each other (enables names?)
#   FIXME: ideally this has the ability o stack the actual chromatograms (not possible to get from D+ I think)
# FIXME: IMPLEMENT ME!!
#   - multi graph allowing selection of several parameters to be plotted in a ggplot grid layout
#   - implement isodat chromatogram import and also allow comparison of chromatograms (with stretching and all)
DSQDP.launch<-function(exitR=FALSE) {
	library(psych)
	library(gWidgets)
	library(ggplot2)
	library(reshape2)
	library(plyr)
	library(scales)
	library(sqldf)
  library(stringr)
  library(graphics)
  library(cairoDevice)
	options("guiToolkit"="RGtk2")
  
	### main window
	DSQDP.running <<- TRUE
  
  # modal dialog
	#gw <- gbasicdialog(
	#  title=paste("DSQ / D+ Data Processor", DSQDP.getVersion()$label,"- Workspace:", getwd()), do.buttons=FALSE) #FIXME add parent = 
	#size(gw) <- c(1400, 700)
  
	gw<-gwindow(paste("DSQ / D+ Data Processor", DSQDP.getVersion()$label,"- Workspace:", getwd()), width=1400, height=700, spacing=10)
	addHandlerDestroy(gw, handler=function(h,...) { DSQDP.running <<- FALSE; if (exitR==TRUE) q("yes") })
  
  ### major divisions
  gl<-ggroup(horizontal=FALSE, expand=TRUE) # left column
	gc<-ggroup(horizontal=FALSE, expand=TRUE) # center column
	gr<-ggroup(horizontal=FALSE, expand=TRUE) # right column
	gpanedgroup(gl, gpanedgroup(gc, gr, expand=TRUE), container=gw, expand=TRUE) # allow collapsing
  
  ### major frames organization
	datasets.frame <- gframe("Data Sets", horizontal=FALSE, container=gl, expand=TRUE)
	peaks.frame <- gframe("Peak Lists", horizontal=FALSE, container=gl, expand=TRUE)
	comp.frame <- gframe("Available Compounds", horizontal=FALSE, container=gl, expand=TRUE)
	info.frame <- gframe("Dataset", horizontal=FALSE, cont=gc, expand=TRUE)
	plotting.grp <- ggroup(horizontal=TRUE, container=gr) 
    plot.type <- gnotebook(container=plotting.grp, expand=TRUE)
	  plot.buttons<-gframe("Operations",cont=plotting.grp)
  plot.frame<-gframe("Plots", cont=gr, horizontal=FALSE, expand=TRUE)

	### peak lists
	peaklistsGUI<-peaklists.GUI(peaks.frame, gw)
	gbutton(action=gaction("Copy from\ndata", icon="gtk-convert", 
       handler=function(h,...) {
         if ((tab<-svalue(dsGUI$dataNotebook)) == 1) # DSQ data
           peaklist.copyFromDataset(peaklistsGUI$peaklist.table, dsGUI$widgets$Data, svalue(dsGUI$widgets$Datatype))
         else if (tab==2) # D+ data
           peaklist.copyFromDataset(peaklistsGUI$peaklist.table, dsGUI$widgets$Dplus, "DPLUS")
       }), cont=peaklistsGUI$peaklist.buttons)
	
	### available compounds
	compGUI<-compounds.GUI(comp.frame, gw)
	addDropSource(compGUI$comp.table, handler= function(h,...) {return("addPeak")})
	adddroptarget(peaklistsGUI$peaklist.table, #FIXME - this drop target will throw a warning, could not figure out how to disable/catch it (none of the catchtrys worked)
	              handler=function(h,...){
	                if (h$dropdata=="addPeak" && 
	                      !is.null(compound<-table.getSelection(compGUI$comp.table))) { #something is selected in the compounds
	                  newPeak<-compound[c("ID","Name")]
	                  newPeak$DPLUS<-newPeak$FID<-newPeak$TIC<-NA #no values to start out with
	                  table.addAfterSelection(peaklistsGUI$peaklist.table, newPeak)
	                } else
	                  gmessage("Please select a compound to add.") 
	                })  
  
  ###### datasets GUI ######
  dsGUI<-datasets.GUI(datasets.frame, info.frame, gw)
  
  ### datasets action handlers 
  dataset.selectionHandler<-function() {
    if (!table.isHandlerBlocked(dsGUI$datasets.table, "clicked"))  { # for enabled clicked events
      dataset<-datasets.getDatasetByID(table.getSelectedValue(dsGUI$datasets.table))
      widgets.load(dsGUI$widgets, dataset)
      dataset.autoPlotHandler() # autoplot if enabled
    }
  }
  
  dataset.saveHandler<-function() {
    ID<-table.getSelectedValue(dsGUI$datasets.table) # get dataset id
    dataset.update(ID, widgets.getValues(dsGUI$widgets)) # update data set in global variable
    table.updateSelection(dsGUI$datasets.table, datasets.getDatasetByID(ID), ignoreMissing=TRUE) # update table
    dataset.autoPlotHandler() # autoplot if enabled
  }
  
  dataset.addHandler<-function(duplicate = FALSE) {
    newDS<-dataset.new() # make new dataset
    currentDS<-datasets.getDatasetByID(table.getSelectedValue(dsGUI$datasets.table)) # get current data set
    if (duplicate) 
      for (field in c("Date", "ISAmount", "IS", "DSQMethod", "Datatype", "Name")) # duplicate from current data set
        newDS[[field]]<-currentDS[[field]] 
    dataset.add(newDS, currentOrderN = currentDS$Order) # add dataset to global variable
    table.addAfterSelection(dsGUI$datasets.table, datasets.getDatasetByID(newDS$ID), select=TRUE, blockHandlers=c("changed", "clicked"), ignoreMissing=TRUE) # add original dataset
    dataset.selectionHandler() # load
  }

  dataset.deleteHandler<-function() {
    if (!is.null(ID<-table.getSelectedValue(dsGUI$datasets.table))){
      if(gconfirm(paste("Do you really want to delete this dataset:\n", datasets.getDatasetByID(ID, fields="Name")))) { # ask for confirmation
        table.deleteSelection(dsGUI$datasets.table, reSelect=TRUE, blockHandlers=c("changed", "clicked"))
        dataset.delete(ID)
        dataset.selectionHandler() # load
      }
    }
  }

  ### datasets table action buttons and handlers
	dsGUI$autoplot<-gcheckbox("Auto-\nplot", checked=TRUE, cont=dsGUI$datasets.buttons, handler=function(h,...) dataset.autoPlotHandler()) # auto plot marker
	addSpring(dsGUI$datasets.buttons)
	gbutton(action=gaction("Add\nDataset", icon="add", handler=function(h,...) dataset.addHandler()), cont=dsGUI$datasets.buttons)
	gbutton(action=gaction("Duplicate\nDataset", icon="gtk-copy", handler=function(h,...) dataset.addHandler(duplicate = TRUE)), cont=dsGUI$datasets.buttons)
	gbutton(action=gaction("Delete\nDataset", icon="delete", handler=function(h,...) dataset.deleteHandler()), cont=dsGUI$datasets.buttons)
	gbutton(action=gaction("Save\nDataset", icon="save", handler=function(h,...) dataset.saveHandler()), cont=dsGUI$datasets.buttons)
	addHandlerClicked(dsGUI$datasets.table, handler=function(h, ...) dataset.selectionHandler())
  
	### dataset data dialog action buttons and handlers
	addHandlerDoubleclick(dsGUI$widgets$Data, handler=function(h,...) 
	  dataset.peakSpecPlotHandler(pn, dsGUI$widgets$Data, dsGUI$datasets.table)) # MS plotting
	gbutton("Paste MS", cont = dsGUI$dsqActionsGrp, handler=function(h,...) {
	  if (excaliburImport.pasteMS(dsGUI$datasets.table, dsGUI$widgets$Data) == TRUE) # successful ms adding
	    dataset.peakSpecPlotHandler(pn, dsGUI$widgets$Data, dsGUI$datasets.table) })
	gbutton("Paste Peaks", cont = dsGUI$dsqActionsGrp, handler=function(h,..) {
	  excaliburImport.pastePeakList(dsGUI$widgets)
	  dataset.autoPlotHandler() })
	gbutton("Paste Chrom", cont = dsGUI$dsqActionsGrp, handler=function(h,...){
	  excaliburImport.pasteChromatogram(dsGUI$datasets.table, dsGUI$widgets, dataset.autoPlotHandler) })
	gbutton("Reprocess Chrom", cont = dsGUI$dsqActionsGrp2, handler=function(h,...) {
	  excaliburImport.reprocessChromatogram(dsGUI$datasets.table, dsGUI$widgets, dataset.autoPlotHandler) })
	gbutton("Paste Peaks", cont = dsGUI$dpActionsGrp, handler=function(h,...) {
	  isodat.pastePeakList(dsGUI$widgets)
	  dataset.autoPlotHandler() })
	gbutton("Import CSV File", cont = dsGUI$dpActionsGrp, handler=function(h,...) {
	  isodat.pasteCSVFile(dsGUI$widgets)
	  dataset.autoPlotHandler()	})
	gbutton(action=gaction("Parse", icon="gtk-execute", handler=function(h,...) { 
	  dataset.parsePeakListHandler(peaklistsGUI$peaklist.table, dsGUI$dataNotebook, dsGUI$widgets$Datatype, dsGUI$widgets$Data, dsGUI$widgets$Dplus)
    dataset.autoPlotHandler() }), cont=dsGUI$infoActionsGrp)
	gbutton(action=gaction("Clean", tooltip="Removes peak identifications not associated with an actual peak (e.g. introduced by 'Parse').", icon="gtk-clear", handler=function(h,...) {
	  dataset.cleanPeaksHandler(dsGUI$dataNotebook, dsGUI$widgets$Data, dsGUI$widgets$Dplus) 
    dataset.autoPlotHandler() }), cont=dsGUI$infoActionsGrp)
	adddroptarget(gbutton(action=gaction("Set Peak", tooltip="Sets the peak identification of the currently selected peak to the compound selected in the compounds list.\nHint: you can also drag a compound onto this button to set a peak, both from the compounds and saved peak lists.", icon="gtk-apply", handler=function(h,...) { 
    dataset.assignPeakHandler(compGUI$comp.table, dsGUI$dataNotebook, dsGUI$widgets$Data, dsGUI$widgets$Dplus)
    dataset.autoPlotHandler() }), cont=dsGUI$infoActionsGrp), handler=function(h,...) {
      dataset.assignPeakHandler(compGUI$comp.table, dsGUI$dataNotebook, dsGUI$widgets$Data, dsGUI$widgets$Dplus, dropdata = h$dropdata)
      dataset.autoPlotHandler() })
  gbutton(action=gaction("Unset", tooltip="Removes the peak identification from the currently selected peak.", icon="gtk-cancel", handler=function(h,...) {
    dataset.unassignPeakHandler(dsGUI$dataNotebook, dsGUI$widgets$Data, dsGUI$widgets$Dplus)
    dataset.autoPlotHandler() }), cont=dsGUI$infoActionsGrp)
  gbutton(action=gaction("Plot", icon="plot", handler=function(h,...) 
    dataset.plotHandler()), cont=dsGUI$infoActionsGrp)
  gbutton(action=gaction("Save", icon="gtk-save", handler=function(h,...) 
    dataset.saveHandler()), cont=dsGUI$infoActionsGrp)
	    
	### dataset plot handlers
	dataset.plotHandler<-function() { # SINGLE PLOT 
	  # reactive previous graph (just in case coming from an external data processor)
	  pn.reactivatePlot(pn)
	  if (svalue(plot.type) == 1) { # plot DSQ
	    dataset.plotDSQHandler(pn, svalue(dsGUI$datasets.table, index=FALSE), 
	                           svalue(dsGUI$widgets$Name), svalue(dsGUI$widgets$Datatype), 
	                           dsGUI$widgets$Data[], svalue(plot.dsq.single, index=TRUE), svalue(plot.dsq.showRT))
	  } else if (svalue(plot.type) == 2) { # plot D+ 
	    dataset.plotDPHandler(pn, svalue(dsGUI$datasets.table, index=FALSE), svalue(dsGUI$widgets$Name), dsGUI$widgets$Dplus[], svalue(plot.dp.single, index=TRUE), 
	                          plotRefs=svalue(plot.dp.refs), plotNonRefs=svalue(plot.dp.nonrefs))
	  }
	}
	
	dataset.autoPlotHandler<-function() if (svalue(dsGUI$autoplot)==TRUE) dataset.plotHandler() # SINGLE AUTO PLOT
	
	datasets.plotHandler<-function() { # MULTI PLOT
	  if (!is.empty(sel<-which(dsGUI$datasets.table[]$Sel=="Yes"))) {
	    datasetIDs<-dsGUI$datasets.table[]$ID[sel] 
	    if (svalue(plot.type) == 1) { # plot DSQ
	      datasets.plotDSQHandler(pn, datasetIDs, svalue(plot.dsq.multi, index=TRUE), normPeakID=svalue(compGUI$comp.table, index=FALSE))
	    } else if (svalue(plot.type) == 2) { # plot D+ 
	      datasets.plotDPlusHandler(pn, datasetIDs, svalue(plot.dp.multi, index=TRUE), normPeakID=svalue(compGUI$comp.table, index=FALSE))
	    }
	  } else
	    gmessage("No datasets selected.")
	}
	
	datasets.showPlotDataHandler<-function() { # PLOT HANDLER
	  if (!is.null(info<-plots.getInfo(pn)) && !is.null(info$dataView))
	    datasets.showPlotData(gw, info$dataView)
	}
  
	###### plotting interface ######
	plotHandlers<-list(
	  Clicked = function(h, ...) plots.zoomHandler(pn, h),
	  Rightclick = function (h, ...) plots.unzoomHandler(pn),
	  MouseMotion = function (h, ...) plots.resetZoomHandler(pn))
	
	pn<-pn.GUI(plot.frame, gw, plotEventHandlers=plotHandlers) 
	pn.showDataButton<-gbutton(action=gaction("Show\nData", icon="gtk-select-font", 
	                                          handler=function(h,...) datasets.showPlotDataHandler(),
	                                          tooltip="Show data table for this plot."), cont=pn$buttons.grp)
  
  ### plotting options 
	plot.type.dsq <- glayout(cont=plot.type, label="Plot DSQ Data", expand=TRUE)
	plot.type.dsq[1, 1] <- "Single dataset:"
  plot.type.dsq[1, 2] <- (plot.dsq.single<-
                            gcombobox(c("Chromatogram", "Area/Height Summary"), cont=plot.type.dsq))
  plot.type.dsq[2, 1] <- "Multiplot:"
  plot.type.dsq[2, 2] <- (plot.dsq.multi<-
                            gcombobox(datasets.getDSQPlottingOptions()$label, selected = 2, cont=plot.type.dsq))
  plot.type.dsq[3, 1] <- "Labels:"
  plot.type.dsq[3, 2] <- (plot.dsq.showRT<-gcheckbox("Show Retention Times", cont=plot.type.dsq))
  
	plot.type.dp <- glayout(cont=plot.type, label="Plot D+ Data", expand=TRUE)
  plot.type.dp[1, 1] <- "Single dataset:"
  plot.type.dp[1, 2] <- (plot.dp.single<-
                           gcombobox(c("Area2 [mV s]", "Amplitude 2 [mV]", "dD vs SMOW [permil]", "rRatio (area 3 / area 2)"), handler=function(h,...) if (svalue(autoplot)==TRUE) dataset.plotHandler(), cont=plot.type.dp))
  plot.type.dp[2, 1] <- "Options:"
  plot.type.dp[2, 2] <- ggroup(cont=plot.type.dp, horizontal=FALSE)
    plot.dp.refs<-gcheckbox("Reference peaks", handler=function(h,...) if (svalue(autoplot)==TRUE) dataset.plotHandler(), cont=plot.type.dp[2, 2], checked=TRUE)
  	plot.dp.nonrefs<-gcheckbox("Non-ref peaks", handler=function(h,...) if (svalue(autoplot)==TRUE) dataset.plotHandler(), cont=plot.type.dp[2, 2], checked=TRUE)
    
  plot.type.dp[3, 1] <- "Multiplot:"
	plot.type.dp[3, 2] <- (plot.dp.multi<-
	                         gcombobox(datasets.getDPlusPlottingOptions()$label, cont=plot.type.dp))
  svalue(plot.type)<-1
	addHandlerChanged(dsGUI$dataNotebook, handler=function(h,...) svalue(plot.type)<-h$pageno) # automatically flip both plot type and data notebook together
	  
	### plotting operations
	plot.buttons.left<-ggroup(cont=plot.buttons, horizontal=FALSE)
	plot.buttons.right<-ggroup(cont=plot.buttons, horizontal=FALSE)
	gbutton(action=gaction("Multiplot", icon="gtk-select-color", 
	                       handler=function(h,...) datasets.plotHandler()), cont=plot.buttons.left)
	gbutton(action=gaction("Excel\nExport", tooltip="Export aggregated data to excel", icon="gtk-find-and-replace", 
	                       handler=function(h,...) {
	                         if (!is.empty(sel<-which(dsGUI$datasets.table[]$Sel=="Yes"))) 
	                           data.saveHandler(gw, dsGUI$datasets.table[]$ID[sel], normPeakID=svalue(compGUI$comp.table, index=FALSE))
	                         else 
                             gmessage("No datasets selected.")
	                       }, action=list(win=gw)), cont=plot.buttons.left)
	gbutton(action=gaction("Save\nWorkspace", icon="gtk-harddisk", tooltip="Save all data.", 
	                       handler=function(h,...) { 
	                         datasetListsHandler.storeSessionList(); 
	                         gmessage(paste(DSQDP.saveData()," backup file saved.")) }), cont=plot.buttons.right)
	gbutton(action=gaction("Save &\nClose", tooltip="Save workspace and close DSQDP", icon="gtk-save-as", 
	                       handler=function(h,...) { 
	                         datasetListsHandler.storeSessionList(); 
	                         DSQDP.saveData(backup=DSQDP.getVersion()$released); 
	                         dispose(gw) }), cont=plot.buttons.right)
	gbutton(action=gaction("Close",icon="gtk-stop", 
	                       handler=function(h,...) { 
	                         if(gconfirm("Are you sure you want to close without saving? All changes made since you last saved the workspace will be lost.")) 
	                           dispose(gw)
	                       }), cont=plot.buttons.right)
  
  ###### init #######
  # initiate screen (load whatever needs to be loaded )
	table.setSelectedValue(dsGUI$datasets.table, 1, index=TRUE, blockHandlers=c("changed", "clicked")) # select first dataset in the table 
	dataset.selectionHandler() # load it
  
}

############
# UPDATES  #
############
# FIXME: make this more automatic
DSQDP.getVersion<-function() {
  #vlist<-list(version=1.1, date="5/21/2013", released=TRUE, codename="Pumpernickel")
  #vlist<-list(version=1.2, date="5/22/2013", released=TRUE, codename="Obstsalat")
  #vlist<-list(version=1.3, date="5/30/2013", released=TRUE, codename="Obstsalat2")
  #vlist<-list(version=1.4, date="6/1/2013", released=TRUE, codename="Himbeerrolle")
  #vlist<-list(version=2.0, date="7/6/2013", released=TRUE, codename="Gugelhupf")
  vlist<-list(version=2.1, date="8/22/2013", released=FALSE, codename="Kartoffelsalat")
  vlist$label<-paste("version ", vlist$version, " (", vlist$date, ", codename ", vlist$codename, ")", sep="")
  if (!vlist$released)
    vlist$label<-paste(vlist$label," - DEV VERSION (unreleased)")
  return(vlist)
}

# update from an older version (this is mostly to update the data files)
DSQDP.update<-function() {
  if (!exists("g.currentVersion") || g.currentVersion < DSQDP.getVersion()$version)
    do.call(paste("DSQDP.updateTo.v", DSQDP.getVersion()$version, sep=""), args=list())
}

# update from an older version
DSQDP.updateTo.v2.1<-function() {
  if (g.currentVersion<2) 
    DSQDP.updateTo.v2()
  
  if (g.currentVersion<2.1) {
    cat("\nUpdating DSQDP from version 2.0 to version 2.1.\n")
    ## update code ##
    # just a file sources update
    
    ## update version ##
    g.currentVersion<<-2.1
  }
}

# update from an older version
DSQDP.updateTo.v2<-function() {
  if (g.currentVersion<1.4) 
    DSQDP.updateTo.v1.4()
  
  if (g.currentVersion<1.5) {
    cat("\nUpdating DSQDP from version 1.4 to version 2.0.\n")
    ## update code ##
    g.datasetLists<<-list("<< Last Session >>"=g.datasets$ID[which(g.datasets$Sel=="Yes")], "<< All >>"=c(Inf), "<< None >>"=c(0))
    g.compounds$Order<<-1:nrow(g.compounds)
    g.currentVersion<<-2.0
  }
}

DSQDP.updateTo.v1.4<-function() {
  if (g.currentVersion<1.3) 
    DSQDP.updateTo.v1.3()
  
  if (g.currentVersion<1.4) {
    cat("\nUpdating DSQDP from version 1.3 to version 1.4.\n")
    ## update code ##
    DSQDP.addDataSetField("Order",NA)
    g.datasets$Order<<-1:length(g.datasets$Order)
    g.currentVersion<<-1.4
  }
}

DSQDP.updateTo.v1.3<-function() {
  if (g.currentVersion<1.2) 
    DSQDP.updateTo.v1.2()
  
  if (g.currentVersion<1.3) {
    cat("\nUpdating DSQDP from version 1.2 to version 1.3.\n")
    DSQDP.addDataSetField("WaterdD",NA)
    g.currentVersion<<-1.3
  }
}


DSQDP.updateTo.v1.2<-function() {
  if (g.currentVersion<1.1) 
    DSQDP.updateFrom.v1.0()
  
  if (g.currentVersion<1.2) {
    cat("\nUpdating DSQDP from version 1.1 to version 1.2.\n")
    g.currentVersion<<-1.2
    # no other changes required
  }
}

# update from v 1.0 to 1.1
DSQDP.updateFrom.v1.0<-function() {
  if (g.currentVersion<1.0) # current version is not version v1.0, update to that one first and then continue
    DSQDP.updateFrom.v0.9()
  
  # now update
  if (g.currentVersion<1.1) {
    cat("\nUpdating DSQDP from version 1.0 to version 1.1.\n")
    DSQDP.addDataSetField("IsodatFile", "") #IsotdatFile
    
    # add DPlus field for data plus data
    for (i in 1:length(g.datasets$ID)) {
      g.datasets$Dplus[[i]]<<-data.frame(RT=numeric(), ID=integer(), Name=character(), StartRT=numeric(), EndRT=numeric(), 
                                   dD_VSMOW=numeric(), PeakNr=integer(), Modified=character(), RefPeak=logical(), RefName=character(),
                                   Amp2=numeric(), Amp3=numeric(), BGD2=numeric(), BGD3=numeric(), 
                                   rArea2=numeric(), rArea3=numeric(), rR3H2v2H2=numeric(), Filename=character(), stringsAsFactors=FALSE)
    }
     
    # loop through peak list to add FID, DPLUS Retention times
    for (i in 1:length(g.peakLists$name)) { 
      plist<-g.peakLists$data[[i]]
      names(plist)<-c("ID","TIC")
      plist$DPLUS<-plist$FID<-plist$TIC
      g.peakLists$data[[i]]<<-plist
    }
    g.currentVersion<<-1.1
    cat("Update complete.\n")
  }
}

# update to version 1.0 from version 0.9
DSQDP.updateFrom.v0.9<-function(){
  if (g.currentVersion<1.1) {
    cat("\nUpdating DSQDP from version 0.9 to version 1.0.\n")
    DSQDP.addDataSetField("Sample","")
    DSQDP.addDataSetField("Weight",NA)
    DSQDP.addDataSetField("IS",NA)
    DSQDP.addDataSetField("ISAmount",NA)
    DSQDP.addDataSetField("DSQMethod","")
    DSQDP.addDataSetField("Datatype","TIC")
    DSQDP.addDataSetField("Chrom",NA)
    g.currentVersion<<-1.0
    cat("Update complete.\n")
  }
}








##################################
# code for DSQ data analyzer     #
# PLOTTING FUNCTIONS             #
# Copyright 2013 Sebastian Kopf  #
# seb.kopf@gmail.com             #
##################################

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








##################################
# code for DSQ data analyzer     #
# COMPOUNDS FUNCTIONS            #
# Copyright 2013 Sebastian Kopf  #
# seb.kopf@gmail.com             #
##################################

############################
# Objects and GUI wrappers #
############################

# standard funcs
compounds.getGlobalIndexByID<-function(id) return (var.getIndexByID(g.compounds, id))
compounds.getCompoundByID<-function(id, fields=NULL) return (var.getEntryByID(g.compounds, id, fields=fields))
compound.new<-function(Name="", Active="Yes", Long="", Class="Unknown") 
  return (var.newAsDF(Name=Name, Active=Active, Long=Long, Class=Class, 
                      ID=max(as.integer(g.compounds$ID))+1, 
                      Order=max(as.integer(g.compounds$Order), na.rm=TRUE)+1))
compound.update<-function(id, data) g.compounds<<-var.update(g.compounds, id, data)
compound.delete<-function(id) g.compounds<<-var.delete(g.compounds, id)
compound.add<-function(data, currentOrderN=NULL) g.compounds<<-var.add(g.compounds, data$ID, data, orderN=(if (!is.null(currentOrderN)) currentOrderN+1 else NULL))


# Get compounds table data
compounds.getTableData<-function(class=NULL) {
  data<-g.compounds
  if (!is.null(class) && class%in%unique(g.compounds$Class)) # if the specified class exists, go select from it
      data<-g.compounds[which(g.compounds$Class==class),] 
  ord<-order(data$Order)
  return(compounds.convertToTableData(data[ord,]))
}

# Convert g.compounds data into the table data format
compounds.convertToTableData<-function(data){
  return(data[,c("ID", "Name", "Active", "Class", "Long")])
}

# Get the ordered compounds classes
compounds.getClasses<-function() {
  return (unique(g.compounds$Class)[order(unique(g.compounds$Class))])
}

############
# MAIN GUI #
############

# return objs:
# - comp.table = the compounds table
# - comp.table.changeHandler - the change handler
compounds.GUI<-function(container, window) {
  objs<-list() # return objects
  
  comp.selGrp<-ggroup(cont=container, horizontal=TRUE) # compounds sorting
  comp.tableGrp<-ggroup(cont=container, horizontal=TRUE, expand=TRUE) # compounds table
  comp.buttonsGrp<- ggroup(container=container) #compounds action buttons

  # sel Group
  glabel("Class:", cont=comp.selGrp)
  gcombobox(c("<< All >>", compounds.getClasses()), handler=function(h,...) objs$comp.table[]<-compounds.getTableData(class=svalue(h$obj)), cont=comp.selGrp)
  
  # compounds table
  objs$comp.table<-gtable(compounds.getTableData(), container=comp.tableGrp, expand=TRUE)
  
  # selection handler (active yes/no status)
  addHandlerDoubleclick(objs$comp.table, handler=function(h,...){
    tsel<-svalue(objs$comp.table, index=TRUE) #selected index in table
    gsel<-compounds.getGlobalIndexByID(svalue(objs$comp.table, index=FALSE)) #selected index in global dataset
    if (objs$comp.table[]$Active[[tsel]]=="Yes")# selected dataset status
      g.compounds[gsel,"Active"]<<-objs$comp.table[]$Active[[tsel]]<-"No"
    else
      g.compounds[gsel,"Active"]<<-objs$comp.table[]$Active[[tsel]]<-"Yes"
  })
  #addHandlerClicked(objs$comp.table, handler=function(h,...) if (!table.isHandlerBlocked(h$obj, "clicked")) print("CLICKED HANDLER")) #need this for anything?
  #addHandlerChanged(objs$comp.table, handler=function(h,...) if (!table.isHandlerBlocked(h$obj, "changed")) print("CHANGED HANDLER")) #need this for anything?
  
  # button move handler
  table.moveButtons(comp.tableGrp, objs$comp.table, gvar="g.compounds") # enable ordering of the compounds
  
  ### actions ###
  addEditHandler<-function(add=FALSE){
    if (add) {
      editData<-compound.new() # make new compound
      ID<-editData$ID # new ID
    } else {
      ID<-table.getSelectedValue(objs$comp.table) # get compound id
      editData<-compounds.getCompoundByID(ID) # get selected compound data
    }

    if (!is.null(editData)) { 
      dlg<-compounds.dialog(window) # launch dialog
      
      # make save handler
      gbutton(action=gaction("Save", icon="save", handler=function(h,...){ # save handler
        updatedData<-widgets.getValues(dlg$widgets) # get data from widgets
        if (add) {
          currentOrderN<-compounds.getCompoundByID(table.getSelectedValue(objs$comp.table), fields="Order") # get current Order 
          compound.add(editData, currentOrderN=currentOrderN) # add compound
          compound.update(ID, updatedData)
          tableData<-compounds.convertToTableData(compounds.getCompoundByID(ID))
          table.addAfterSelection(objs$comp.table, tableData, select=TRUE, blockHandlers=c("changed", "clicked")) # add original dataset
        } else {
          compound.update(ID, updatedData)
          tableData<-compounds.convertToTableData(compounds.getCompoundByID(ID))
          table.updateSelection(objs$comp.table, tableData) # update table
        }
        dispose(dlg$win)
      }), container=dlg$actionsGrp)
      
      widgets.load(dlg$widgets, editData) # load dialog
      visible(dlg$win, set=TRUE)
    }
  }

  # add
  gbutton(action=gaction("Add", icon="add", handler=function(h,...) addEditHandler(add=TRUE)), cont=comp.buttonsGrp)
  
  # edit
  gbutton(action=gaction("Edit", icon="gtk-select-font", handler=function(h,...) addEditHandler()), cont=comp.buttonsGrp)
  
  # delete
  gbutton(action=gaction("Delete", icon="delete", cont=comp.buttons, handler=function(h,...){
    if (!is.null(ID<-table.getSelectedValue(objs$comp.table))){
      table.deleteSelection(objs$comp.table, reSelect=TRUE, blockHandlers=c("changed", "clicked"))
      compound.delete(ID)
    }
  }), cont=comp.buttonsGrp)
   
  return(objs)
}


###################
# Add/Edit Dialog #
###################

# Compounds edit dialog
# creates widgets and returns them
# return value:
#   - widgets = list of widgets
#   - win = dialog window
#   - actionsGrp = the group for action buttons
compounds.dialog<-function(parent){
  dlgData<-list() # dialog widgets
  
  win<-gbasicdialog("Compound", container=parent, width=200, height=100, do.buttons=FALSE)
  dlg<-ggroup(horizontal=FALSE, container=win)
  
  dlggrp<-glayout(container=dlg, spacing=10)
  
  dlggrp[1,1]<-glabel("Compound ID:",con=dlggrp)
  dlggrp[1,2]<-(dlgData$ID <- glabel("",container=dlggrp, coerce.with=as.numeric))
  
  dlggrp[2,1]<-glabel("Name:",con=dlggrp)
  dlggrp[2,2]<-(dlgData$Name <- gedit("",container=dlggrp))
  
  dlggrp[3,1]<-glabel("Long Name:",con=dlggrp)
  dlggrp[3,2]<-(dlgData$Long <- gedit("",container=dlggrp))
  
  dlggrp[4,1]<-glabel("Compound Class:",con=dlggrp)
  dlggrp[4,2]<-(dlgData$Class <- gcombobox(compounds.getClasses(), editable=TRUE,container=dlggrp))
  
  #EDITABLE in table directly
  #dlggrp[5,1]<-glabel("Active:", con=dlggrp)
  #dlggrp[5,2]<-(dlgData$Active <- gradio(c("Yes","No"),con=dlggrp))
  
  addSpace(dlg, 2)
  dlgbuttons <- ggroup(container = dlg)
  addSpring(dlgbuttons)
  actionbuttonsgrp<-ggroup(container = dlgbuttons)
  gbutton(action=gaction("Cancel", icon="cancel", handler = function(h,...) dispose(win)), container=dlgbuttons)
  
  return (list(widgets=dlgData, win=win, actionsGrp=actionbuttonsgrp))
}

#TESTING compounds.GUI(gframe("test", horizontal=FALSE, cont=(win<-gwindow("test", width=600, height=600))),win)



##################################
# code for DSQ data analyzer     #
# PEAKLIST FUNCTIONS             #
# Copyright 2013 Sebastian Kopf  #
# seb.kopf@gmail.com             #
##################################

########################
# Unique functionality #
########################

# copy the currently active data set into the peaks list to use it as a template for a peak list
peaklist.copyFromDataset<-function(peakListTable, dataTable, type){
  peaklist<-peakListTable[]
  datapeaks<-dataTable[]
  
  peaklist[type]<-NA # remove existing ones
  if (!is.empty(remainingPeaks<-which(!(is.na(peaklist$DPLUS) & is.na(peaklist$TIC) & is.na(peaklist$FID)))))
    peaklist<-peaklist[remainingPeaks,] # remove completely undefined ones
  else
    peaklist<-peaklist[0,]
  peaklist[type]<-NULL # remove the column entirely before the merge
  
  # merge the lists
  newpeaklist<-merge(x = peaklist, y = subset(datapeaks, !is.na(ID), select=c("ID","Name","RT")), by = c("ID","Name"), all = TRUE)
  names(newpeaklist)[which(names(newpeaklist)=="RT")]<-type
  newpeaklist<-newpeaklist[order(newpeaklist[[type]]),]
  
  peakListTable[]<-newpeaklist[c("ID", "Name", "TIC", "FID", "DPLUS")]
}

############################
# Objects and GUI wrappers #
############################

# standard funcs
peaklist.update<-function(name, table)
  g.peakLists<<-var.update(g.peakLists, name, list(data=table[][c("ID","TIC","FID","DPLUS")]), idField="name")
peaklist.delete<-function(name) g.peakLists<<-var.delete(g.peakLists, name, idField="name")
peaklist.add<-function(name, table) 
  g.peakLists<<-var.add(g.peakLists, name, list(data=table[][c("ID","TIC","FID","DPLUS")]), idField="name")

# Loading of peak list 
peaklist.load<-function(name, table) {
  table[]<-peaklist.convertToTableData(
    var.getEntryByID(g.peakLists, name, idField="name", fields=c("data")))
}

# Convert to table data fields
peaklist.convertToTableData<-function(data) {
  return(sqldf("SELECT pl.ID, gc.Name, pl.TIC, pl.FID, pl.DPLUS FROM data pl left OUTER JOIN 'g.compounds' gc USING(ID)"))
}

# Adjust the retention time of the whole peaklist
# offset is a number
# type is values 1, 2 or 3 (TIC, FID, DPLUS)
peakslist.adjustRTHandler<-function(table, offset, type){
  #Previous attempt to use a slider did not work well #diff<-as.numeric(svalue(h$obj))-as.numeric(g.RTadjust)
  # New method just by button
  switch(type, 
         1==(table[]$TIC<-table[]$TIC+offset),
         2==(table[]$FID<-table[]$FID+offset),
         3==(table[]$DPLUS<-table[]$DPLUS+offset))
}

# Clear retention times for a specific type
# DEPRECATED
peakslist.clearRTHandler<-function(h,...){
  switch(svalue(h$action$type,index=TRUE), 
         1==(h$action$table[]$TIC<-NA),
         2==(h$action$table[]$FID<-NA),
         3==(h$action$table[]$DPLUS<-NA))
}


##################
# MAIN TABLE GUI #
##################

# return objs:
# - peaklist.table = the compounds table
# - comp.table.changeHandler - the change handler
peaklists.GUI<-function(container, window) {
  objs<-list() # return objs
  
  ### actions ###
  # edit
  editHandler<-function() {
    if (!is.null(entry<-table.getSelection(objs$peaklist.table))) {
      # launch edit dialog
      
      # ok handler
      okHandler<-function(h,...){
        updatedEntry<-widgets.getValues(dlg$widgets)
        table.updateSelection(objs$peaklist.table, updatedEntry) 
      }
      dlg<-peaklistentry.dialog(window, okHandler) # launch dialog
      widgets.load(dlg$widgets, entry) # load dialog
      visible(dlg$win, set=TRUE)
    }
  }
  
  # main peak list and edit table
  peaklistsgrp<-ggroup(cont=container)
  objs$peaklist.buttons<-ggroup(cont=container)
  objs$peaklist.table<- gtable(
    peaklist.convertToTableData(g.peakLists$data[[1]]), container=container, expand=TRUE, 
    handler=function(h,...) editHandler())
  
    peaks.RTadjust<-gedit("0",container=objs$peaklist.buttons, coerce.with=as.numeric, width=2)
    peaks.RTtype<-gcombobox(c("TIC", "FID", "D+"), cont=objs$peaklist.buttons)
  
    # move RT
    gbutton(action=gaction("Move RT", icon="gtk-jump-to", 
                           handler=function(h,...) 
                             peakslist.adjustRTHandler(objs$peaklist.table, svalue(peaks.RTadjust), svalue(peaks.RTtype,index=TRUE))), cont=objs$peaklist.buttons)
    # remove compound
    gbutton(action=gaction("Remove\nCompound", icon="gtk-cancel", tooltip="Remove a compound from this peak list.", 
                         handler=function(h,...) table.deleteSelection(objs$peaklist.table, reSelect=TRUE)), cont=objs$peaklist.buttons)
  
    # peak list drop down
    peaks.list <- gcombobox(g.peakLists$name, container=peaklistsgrp)
    peaks.list.changedHandler<-addHandlerChanged(peaks.list, 
                    handler=function(h,...) 
                      peaklist.load(svalue(h$obj), objs$peaklist.table))
  
    # adding peak list
    gbutton( action=gaction("Save as\nNew", icon="gtk-new", 
                     handler=function(h,...) {
                       ginput("Under what name would you like to save this Peaklist?", 
                              title="Input", icon = "question", 
                              handler=function(h,...) {
                                peaklist.add(h$input, objs$peaklist.table)
                                combo.add(peaks.list, h$input, sort=TRUE, select=TRUE, blockHandlers=peaks.list.changedHandler)
                                peaklist.load(h$input, objs$peaklist.table)
                              })  
                     }), cont=peaklistsgrp)
    
    # deleting peak list
    gbutton(action=gaction("Delete\nPeaklist", icon="gtk-delete", 
                    handler=function(h,...){
                      if (gconfirm(paste("Are you sure you want to delete this list?\n", (entry<-svalue(peaks.list)), sep=""))) {
                        peaklist.delete(entry)
                        combo.deleteSelection(peaks.list, blockHandlers=peaks.list.changedHandler, reSelect=TRUE)
                        peaklist.load(svalue(peaks.list), objs$peaklist.table)
                      }
                    }), cont=peaklistsgrp)

    # saving peak list
    gbutton(action=gaction("Save\nPeaklist", icon="gtk-save", 
                           handler=function(h,...) {
                             peaklist.update(svalue(peaks.list), objs$peaklist.table)
                           }), cont=peaklistsgrp)
  
  return(objs)
}

###############
# EDIT Dialog #
###############

# Peak list entry edit dialog
# must run visible(returnlist$win)<-TRUE to enable modal dialog
peaklistentry.dialog<-function(parent, okHandler) {
  dlgData<-list()
  
  win<-gbasicdialog("Peak", container=parent, width=200, height=100, handler=okHandler)
  
  dlg<-ggroup(horizontal=FALSE, container=win)
  
  dlggrp<-glayout(container=dlg, spacing=10)
  dlggrp[1,1]<-glabel("Compound ID:",con=dlggrp)
  dlggrp[1,2]<-(dlgData$ID <- glabel("",container=dlggrp, coerce.with=as.numeric))
  
  dlggrp[2,1]<-glabel("Compound Name:",con=dlggrp)
  dlggrp[2,2]<-(dlgData$Name <- glabel("",container=dlggrp))
  
  dlggrp[3,1]<-glabel("Retention Time (TIC):",con=dlggrp)
  dlggrp[3,2]<-(dlgData$TIC <- gedit("",container=dlggrp, coerce.with=as.numeric))
  
  dlggrp[4,1]<-glabel("Retention Time (FID):",con=dlggrp)
  dlggrp[4,2]<-(dlgData$FID <- gedit("",container=dlggrp, coerce.with=as.numeric))
  
  dlggrp[5,1]<-glabel("Retention Time (D+):",con=dlggrp)
  dlggrp[5,2]<-(dlgData$DPLUS <- gedit("",container=dlggrp, coerce.with=as.numeric))

  return (list(widgets=dlgData, win=win))
}




##################################
# code for DSQ data analyzer     #
# IMPORT FUNCTIONS               #
# Copyright 2013 Sebastian Kopf  #
# seb.kopf@gmail.com             #
##################################

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



##################################
# code for DSQ data analyzer     #
# DATASETS FUNCTIONS             #
# Copyright 2013 Sebastian Kopf  #
# seb.kopf@gmail.com             #
##################################

####################
# Data aggregation #
####################

# aggregate all the data in a number of selected data sets
# pass in vector of dataset IDs and (optionally) a normalized peak ID to normalize data against
# returns the melted dataset
datasets.aggregateData<-function(datasetIDs, normPeakID = NULL) {
  # generate aggragated datasets
  df<-data.frame(stringsAsFactors=FALSE) # melted data frame
  for (id in datasetIDs) { 
    #DSQ DATA
    # get all peaks that actually have data (and are assigned)
    # IMPORTANT: this also sums all peaks that have the same assignment
    # FIXME: figure out whether we actually want to do it this way
    i<-which(g.datasets$ID==id) # get index
    
    if (nrow(g.datasets$Data[[i]]) > 0 && !is.empty(rows<-which(!is.na(g.datasets$Data[[i]]$Area)))) {
      data<-ddply(g.datasets$Data[[i]][rows,], .(ID, Name), function(x) colSums(x[c("Height", "Area")]))
      if (nrow(data)>0){
        # get all the sums
        allPeaksArea<-sum(data$Area)
        allPeaksHeight<-sum(data$Height)
        data<-subset(data, !is.na(ID)) # remove the ones without ID
        if (nrow(data)>0) { # check again there's anything there at all
          activePeaks<-subset(merge(data, g.compounds, by="ID"), Active=="Yes")
          activePeaksArea<-sum(activePeaks$Area)
          activePeaksHeight<-sum(activePeaks$Height)
          
          # additional dataset info
          data$Datatype<-g.datasets$Datatype[[i]]
          data$DatasetID<-g.datasets$ID[[i]]
          data$Dataset<-g.datasets$Name[[i]]
          data$Weight<-g.datasets$Weight[[i]]
          data$WaterdD<-g.datasets$WaterdD[[i]]
          
          # normalize to total 
          data$HeightRelT<-data$Height/allPeaksHeight
          data$AreaRelT<-data$Area/allPeaksArea
          
          # relative to active peaks
          data$HeightRelA<-data$Height/activePeaksHeight
          data$AreaRelA<-data$Area/activePeaksArea
          
          # normalize relative to a peak 
          data$HeightRelP<-NA
          data$AreaRelP<-NA
          if (!is.empty(normPeakID) && !is.empty(which(data$ID==normPeakID))) { # make sure the peak exists, otherwise everything is just stored as NA
            data$HeightRelP<-data$Height/data$Height[which(data$ID==normPeakID)]
            data$AreaRelP<-data$Area/data$Area[which(data$ID==normPeakID)]
          }
          
          # calculate amounts and concentration based on internal standard quantification
          data$Amount<-NA
          data$Conc<-NA
          if (!is.na(g.datasets$IS[[i]]) && !is.na(g.datasets$ISAmount[[i]]) && !is.empty(which(data$ID==g.datasets$IS[[i]]))) { #internal standard and amount defined and internal standard peak present
            data$Amount<-data$Area/data$Area[which(data$ID==g.datasets$IS[[i]])]*g.datasets$ISAmount[[i]]
            if (!is.na(g.datasets$Weight[i])) #weight of sample is defined
              data$Conc<-data$Amount/g.datasets$Weight[i]
          }
          
          #combine data
          df<-rbind(df,melt(data,id=c("ID","Name","DatasetID","Dataset","Datatype", "Weight", "WaterdD")))
        }
      }
    }
    
    #DPLUS DATA (only assigned peaks)
    if (nrow(g.datasets$Dplus[[i]]) > 0) {
      dplus<-g.datasets$Dplus[[i]][which(!is.na(g.datasets$Dplus[[i]]$ID) & !is.na(g.datasets$Dplus[[i]]$Amp2)),c("ID", "Name", "rArea2", "Amp2", "dD_VSMOW", "rR3H2v2H2")]
      if (nrow(dplus)>0){
        dplus$Datatype<-"D+"
        dplus$DatasetID<-g.datasets$ID[[i]]
        dplus$Dataset<-g.datasets$Name[[i]]
        dplus$Weight<-g.datasets$Weight[[i]]
        dplus$WaterdD<-g.datasets$WaterdD[[i]]
        
        dplus$NormalizedArea<-NA
        if (!is.empty(normPeakID) && !is.empty(which(dplus$ID==normPeakID)))
          dplus$NormalizedArea<-dplus$rArea2/dplus$rArea2[which(dplus$ID==normPeakID)]
        
        # combine data
        df<-rbind(df,melt(dplus,id=c("ID","Name","DatasetID","Dataset","Datatype", "Weight", "WaterdD")))
      }
    }
  }
  # get proper compounds names from the IDs
  # remove all inactive compounds from the data melt
  if (nrow(df) > 0){
    df<-sqldf("SELECT data.*, gc.Name AS CName, gc.'Order' FROM df data LEFT JOIN 'g.compounds' gc USING(ID) WHERE gc.Active='Yes'") 
    return (df)
  } else {
    return (NULL)
  }
}

######################
# FILE SAVE HANDLERS #
######################

# function to save the current data set into an excel file (in recast mode)
data.saveHandler<-function(window, datasetIDs, normPeakID=NULL){
 
  f=gfile("Select where to save this export file.", type="save", cont=window,
          initialfilename = paste(format(Sys.time(),format="%Y%m%d_myexport"),".csv", sep=""),
          filter = list("CSV Files" = list(patterns=c("*.csv")), "All files" = list(patterns = c("*"))))
  
  if (!is.na(f)){
    if (length(grep("\\.csv$", f))==0) f<-paste(f,".csv",sep="")
    
    allData<-datasets.aggregateData(datasetIDs, normPeakID=normPeakID)
    castData<-dcast(allData, variable + DatasetID + Dataset + Weight + WaterdD~CName)
    plotoptions<-rbind(datasets.getDSQPlottingOptions(normPeakID = normPeakID), datasets.getDPlusPlottingOptions())
    mergeData<-merge(castData, plotoptions)
    expData<-data.frame(Data=mergeData$label, mergeData[names(castData)[which(names(castData)!="variable")]])
    
    write.csv(expData, file=f)
  }
}

############################
# Objects and GUI wrappers #
############################

# standard funcs
datasets.getGlobalIndexByID<-function(id) return (var.getIndexByID(g.datasets, id))
datasets.getDatasetByID<-function(id, fields=NULL) return (var.getEntryByID(g.datasets, id, fields=fields))
dataset.new<-function(Name="", Excfile="", IsodatFile="", Date=format(Sys.time(), "%Y-%m-%d"), Notes="", Sel="Yes",
                      Sample="", Weight=NA, IS=NA, ISAmount=NA, DSQMethod="", Datatype="TIC", Chrom=NA, WaterdD=NA) {
  return(c(
    list(ID=max(as.integer(g.datasets$ID))+1, Order=max(as.integer(g.datasets$Order), na.rm=TRUE)+1),
    ls.asList(environment()), 
    list(Data = data.frame(RT=numeric(), ID=integer(), Name=character(), StartRT=numeric(), EndRT=numeric(), Height=numeric(), Area=numeric(), stringsAsFactors=FALSE)),
    list(Dplus = data.frame(RT=numeric(), ID=integer(), Name=character(), StartRT=numeric(), EndRT=numeric(), 
                            dD_VSMOW=numeric(), PeakNr=integer(), Modified=character(), RefPeak=logical(), RefName=character(),
                            Amp2=numeric(), Amp3=numeric(), BGD2=numeric(), BGD3=numeric(), 
                            rArea2=numeric(), rArea3=numeric(), rR3H2v2H2=numeric(), Filename=character(), stringsAsFactors=FALSE))))
}
dataset.update<-function(id, data) g.datasets<<-var.update(g.datasets, id, data)
dataset.delete<-function(id) g.datasets<<-var.delete(g.datasets, id)
dataset.add<-function(data, currentOrderN=NULL) g.datasets<<-var.add(g.datasets, data$ID, data, orderN=(if (!is.null(currentOrderN)) currentOrderN+1 else NULL))

# Loading the datasets into the data set table
datasets.getTableData<-function() {
  data<-g.datasets
  ord<-order(data$Order)
  return(data.frame(ID=data$ID[ord], Sel=data$Sel[ord], Name=data$Name[ord], Filename=data$Excfile[ord], Date=data$Date[ord], stringsAsFactors=FALSE))
}

##########################
# DATASET LISTS HANDLERS #
##########################

# store session dataset list
datasetListsHandler.storeSessionList<-function() {
  g.datasetLists[["<< Last Session >>"]]<<-g.datasets$ID[which(g.datasets$Sel=="Yes")]
}


# load dataset list
datasetListsHandler.load<-function(dsLists.combo, selection="<< Last Session >>") {
  dsLists.combo[]<-c(names(g.datasetLists)[1:3], names(g.datasetLists)[-c(1:3)][order(names(g.datasetLists)[-c(1:3)])]) # repopulate combobox (alphabetical order)
  svalue(dsLists.combo)<-selection # select new entry
}

# select a specific dataset list
datasetListsHandler.select<-function(dsLists.combo, ds.table, dsLists.deleteButton, dsLists.saveButton) {
  if ( !is.null(selIndex<-svalue(dsLists.combo, index=TRUE)) ) {
    if (selIndex <= 3)  # one of the 3 default lists is selected, disable delete and save buttons
      enableButtons<-FALSE
    else
      enableButtons<-TRUE
    enabled(dsLists.deleteButton)<-enableButtons
    enabled(dsLists.saveButton)<-enableButtons
    
    g.datasets$Sel<<-ds.table[]$Sel<-rep("No", each=nrow(ds.table[])) # select none by default
    if ( ( is.infinite(ids<-g.datasetLists[[svalue(dsLists.combo)]] )[1]) ) # select all
      g.datasets$Sel<<-ds.table[]$Sel<-rep("Yes", each=nrow(ds.table[]))
    else if (length(ids) > 0 && ids[1] != 0)  # select from the saved list
      g.datasets$Sel[which(g.datasets$ID%in%ids)]<<-ds.table[]$Sel[which(ds.table[]$ID%in%ids)]<-"Yes"
  }
}

# add a new dataset list
datasetListsHandler.add<-function(dsLists.combo, ds.table) {
  ginput("Under what name would you like to save this selection of datasets?", 
         title="Input", icon = "question", 
         handler=function(h,...) {
           g.datasetLists[[h$input]]<<-c(0) # generate new saved datasets selection
           datasetListsHandler.save(dsLists.combo, ds.table, dsList.name=h$input) # assign value
           datasetListsHandler.load(dsLists.combo, selection=h$input) # reload list
         })  
}

# save a dataset list
datasetListsHandler.save<-function(dsLists.combo, ds.table, dsList.name=NULL){
  if (is.null(dsList.name))
    dsList.name<-svalue(dsLists.combo)
  if ( !is.empty(selected<-which(ds.table[]$Sel=="Yes")) )
    g.datasetLists[[dsList.name]]<<-ds.table[]$ID[selected]
  else
    g.datasetLists[[dsList.name]]<<-c(0)
}

# delete a dataset list
datasetListsHandler.delete<-function(dsLists.combo){
  if (svalue(dsLists.combo)%in%c("<< Last Session >>", "<< All >>", "<< None >>"))
    gmessage("Cannot delete a default list.")
  else
    if(gconfirm(paste("Are you sure you want to delete this list?\n", svalue(dsLists.combo), sep=""))) {
      g.datasetLists[[svalue(dsLists.combo)]]<<-NULL
      dsLists.combo[]<-names(g.datasetLists)
      svalue(dsLists.combo, index=TRUE)<-1
    }
}

#####################
# PEAK LIST PARSING #
#####################

# Parse a peak list and see what best fits can be found for a given dataset
dataset.parsePeakListHandler<-function(peaklistTable, dataNotebook, dataType, dsqDataTable, dplusDataTable){
  peaklist<-peaklistTable[]
  tab<-svalue(dataNotebook)
  if (tab==1) {#DSQ data
    data<-dsqDataTable[] # get the currently selected peak list
    data<-data[which(!is.na(data$Area)),] # make sure to only try to match real data
    peaklist$RT<-peaklist[[svalue(dataType)]]
  } else if (tab==2) {#D+ data
    data<-dplusDataTable[] # get the currently selected peak list
    data<-data[which(!is.na(data$dD_VSMOW)),] # make sure to only try to match real data
    peaklist$RT<-peaklist$DPLUS
  }
  peaklist<-peaklist[which(!is.na(peaklist$RT)),]
  df<-data.matchPeaks(data, peaklist)
  
  #remove any columsn that don't have and ID and don't have Area (orphans)
  if (tab==1) {
    df<-df[which(!(is.na(df$ID)&is.na(df$Area))),]
    dsqDataTable[]<-df
  } else if (tab==2){
    df<-df[which(!(is.na(df$ID)&is.na(df$dD_VSMOW))),]
    dplusDataTable[]<-df
  }
}

# peaks is a list of peaks with RT, ID and NAME
data.matchPeaks<-function(data, peaks) {
  peaks<-peaks[which(!is.na(peaks$RT)),] # make sure all missing peaks are ignored  
  if (nrow(peaks)>0) { 
    sql<-paste("select p.RT, d.RT as REALRT, p.ID, p.Name,", paste(paste("d.", names(data)[which(!names(data)%in%c("RT","ID","Name"))], sep=""), collapse=", "), " from data d left join peaks p on (p.RT between d.startRT and d.endRT)")  
    pidf<-sqldf(sql) 
    # remove any duplicate retention times generated if there are any peaks whose start to end RT covers more than peak defined in the peaklist
    pidf<-pidf[!duplicated(pidf$REALRT),]
    #outer join to add in any peaks in the peaklist that are left unassigned
    pidf<-merge(x=pidf, y=peaks, by=c("RT", "ID", "Name"), all=TRUE) 
    # replace RTs with real RTs for actual peaks with data (rest keep the theorteical RTs)
    pidf[which(!is.na(pidf$REALRT)),"RT"]<-pidf[which(!is.na(pidf$REALRT)),"REALRT"]
    df<-pidf[names(data)]
  } else 
    df<-data
  df<-df[order(df$RT),] #sort the data by the retention time
  return(df)
}

# Assign peak handler (can support drag and drop fromt the compounds list)
dataset.assignPeakHandler<-function(compoundsTable, dataNotebook, dsqDataTable, dplusDataTable, dropdata = NULL){
  if (is.null(dropdata) || dropdata=="addPeak") { # this is the correct handler
    tab<-svalue(dataNotebook)
    if (tab==1)  #DSQ data
      datapeaks<-dsqDataTable
    else if (tab==2)  #D+ data
      datapeaks<-dplusDataTable
    
    if (length(idval<-svalue(compoundsTable))>0 && length(peakrt<-svalue(datapeaks))>0) { #something is selected in the compounds as well as the data peaks
      datapeaks[][which(datapeaks[]["RT"]==peakrt),c("ID","Name")]<-compoundsTable[][which(compoundsTable[]["ID"]==idval),c("ID","Name")]
      index<-min(svalue(datapeaks, index=TRUE)+1, nrow(datapeaks[]))
      svalue(datapeaks, index=TRUE)<-index
    } else
      gmessage("Please select a peak in the dataset and a compound to assign.") 
  }
}

# Unassign peak handler
dataset.unassignPeakHandler<-function(dataNotebook, dsqDataTable, dplusDataTable){
  tab<-svalue(dataNotebook)
  if (tab==1)  #DSQ data
    datapeaks<-dsqDataTable
  else if (tab==2)  #D+ data
    datapeaks<-dplusDataTable
  
  if (length(peakrt<-svalue(datapeaks))>0) { #something is selected in the data peaks
    if ((tab==1 && is.na(datapeaks[][which(datapeaks[]["RT"]==peakrt),c("Area")])) || (tab==2 && is.na(datapeaks[][which(datapeaks[]["RT"]==peakrt),c("dD_VSMOW")])))# unassigning this peak means it's an orphan --> delete completely
      datapeaks[]<-datapeaks[][which(datapeaks[]["RT"]!=peakrt),]
    else
      datapeaks[][which(datapeaks[]["RT"]==peakrt),c("ID","Name")]<-c(NA,NA) # set entry to unassigned
  }
}

# Clean up the data set peaks and remove the ones that are not actually associated with a peak (introduced by parse but don't fit)
dataset.cleanPeaksHandler<-function(dataNotebook, dsqDataTable, dplusDataTable) {
  tab<-svalue(dataNotebook)
  if (tab==1) { #DSQ data
    datapeaks<-dsqDataTable
    realpeaks<-which(!is.na(datapeaks[]$Area))
  } else if (tab==2) { #D+ data
    datapeaks<-dplusDataTable
    realpeaks<-which(!is.na(datapeaks[]$dD_VSMOW))
  }
  datapeaks[]<-datapeaks[][realpeaks,]
}

############
# MAIN GUI #
############

# MAIN GUI elements of dataset processing are created here
datasets.GUI<-function(tableCont, dialogCont, window) {
  # return objects
  objs<-list()
  
  # top level organizational groups
  datasets.selGrp<-ggroup(cont = tableCont, horizontal=TRUE) # datasets selection interface
  datasets.tableGrp<-ggroup(cont = tableCont, horizontal = TRUE, expand = TRUE) # datasets table and moving (up, down, ...) elements
  objs$datasets.buttons<-ggroup(cont = tableCont, horizontal=TRUE)
    
  # datasets table
  objs$datasets.table<-gtable(datasets.getTableData(), cont = datasets.tableGrp, expand=TRUE)
  
  # datasets table moving buttons (enable ordering)
  table.moveButtons(datasets.tableGrp, objs$datasets.table, gvar="g.datasets") 
  
  # datasets selections
  objs$datasetLists.combo<-gcombobox(c("test"), handler=function(h,...) 
    datasetListsHandler.select(objs$datasetLists.combo, objs$datasets.table, datasetListsDelB, datasetListsSaveB), cont=datasets.selGrp)
  gbutton(action=gaction("New", icon="new", handler=function(h,...) 
    datasetListsHandler.add(objs$datasetLists.combo, objs$datasets.table)), cont=datasets.selGrp)
  datasetListsDelB<-gbutton(action=gaction("Delete\nSelection", icon="delete", handler=function(h,...) 
    datasetListsHandler.delete(objs$datasetLists.combo)), cont=datasets.selGrp)
  datasetListsSaveB<-gbutton(action=gaction("Save\nSelection", icon="save", handler=function(h,...) 
    datasetListsHandler.save(objs$datasetLists.combo, objs$datasets.table)), cont=datasets.selGrp)
  datasetListsHandler.load(objs$datasetLists.combo)
  
  # dataset selection handler (active yes/no status)
  addHandlerDoubleclick(objs$datasets.table, handler=function(h,...) {
    tsel<-svalue(objs$datasets.table, index=TRUE) #selected index in table
    gsel<-datasets.getGlobalIndexByID(svalue(objs$datasets.table, index=FALSE)) #selected index in global dataset
    if (objs$datasets.table[]$Sel[[tsel]]=="Yes")# selected dataset status
      g.datasets$Sel[[gsel]]<<-objs$datasets.table[]$Sel[[tsel]]<-"No"
    else
      g.datasets$Sel[[gsel]]<<-objs$datasets.table[]$Sel[[tsel]]<-"Yes"
  }) 

  # dataset data dialog (not really a dialog but inscreen edit)
  objs <- c(objs, dataset.dialog(dialogCont))
  svalue(objs$dataNotebook)<-1 # select dsq data tab

  # return all objects
  return (objs)
}

###################
# ADD/EDIT SCREEN #
###################

# Dataset Info dialog (this is not really a dialog, it's embedded in the screen, but works the same way)
dataset.dialog<-function(cont){
  dlgWidgets<-list() # saved objects
  dlgDS<-dataset.new() # loaded default values
  dlggrp<-glayout(container=cont, spacing=10)
  
  ### Basic information
  dlggrp[(i<-1),1]<-glabel("Name:",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$Name <- gedit(dlgDS$Name,container=dlggrp))
  
  dlggrp[(i<-i+1),1]<-glabel("Date (yyyy-mm-dd):",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$Date <- gedit(dlgDS$Date,container=dlggrp))
  
  dlggrp[(i<-i+1),1]<-glabel("Excalibur File:",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$Excfile <- glabel(dlgDS$Excfile,container=dlggrp))
  
  dlggrp[(i<-i+1),1]<-glabel("Isodat File(s):",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$IsodatFile <- glabel(dlgDS$IsodatFile,container=dlggrp))
  
  dlggrp[(i<-i+1),1]<-glabel("GC-MS Method:",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$DSQMethod <- gedit(dlgDS$DSQMethod,container=dlggrp))
  
  dlggrp[(i<-i+1),1]<-glabel("Sample:",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$Sample <- gedit(dlgDS$Sample,container=dlggrp))
  
  dlggrp[(i<-i+1),1]<-glabel("Weight [mg]:",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$Weight <- gedit(dlgDS$Weight,container=dlggrp, coerce.with=as.numeric))
  
  dlggrp[(i<-i+1),1]<-glabel("IS (Compound ID):",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$IS <- gedit(dlgDS$IS,container=dlggrp, coerce.with=as.integer))
  
  dlggrp[(i<-i+1),1]<-glabel("IS Amount [ug]:",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$ISAmount <- gedit(dlgDS$ISAmount,container=dlggrp, coerce.with=as.numeric))
  
  dlggrp[(i<-i+1),1]<-glabel("Water dD [permil]:",con=dlggrp)
  dlggrp[i,2]<-(dlgWidgets$WaterdD <- gedit(dlgDS$WaterdD,container=dlggrp, coerce.with=as.numeric))
  
  dlggrp[(i<-i+1),1]<-glabel("Notes:",con=dlggrp)
  dlggrp[(i<-i+1),1:2]<-(dlgWidgets$Notes <- gtext(dlgDS$Notes,container=dlggrp))
  
  ### Data
  dlggrp[(i<-i+1),1]<-glabel("Data:", con=dlggrp)
  
  # data notebook and dsq / d+ subgroups
  dataNB <- gnotebook(container = cont, expand=TRUE) 
  dsqgrp<-ggroup(cont=dataNB, label="DSQ Data", horizontal=FALSE)
  dsqgrp.buttons<-ggroup(cont=dsqgrp, horizontal=TRUE)
  dpgrp<-ggroup(cont=dataNB, label="D+ Data", horizontal=FALSE)
  dpgrp.buttons<-ggroup(cont=dpgrp, horizontal=TRUE)
  
  # dsq data type
  glabel("Datatype:",con = dsqgrp.buttons)
  dlgWidgets$Datatype<-gradio(c("TIC","FID"),cont = dsqgrp.buttons, horizontal=TRUE)
  svalue(dlgWidgets$Datatype)<-dlgDS$Datatype # starting value
  
  # dsq and D+ data tables
  dlgWidgets$Data<-gtable(dlgDS$Data, cont=dsqgrp, expand=TRUE)
  dlgWidgets$Dplus<-gtable(dlgDS$Dplus, cont=dpgrp, expand=TRUE)
  
  # add springs to button groups
  addSpring(dsqgrp.buttons)
  addSpring(dsqgrp.postButtons<-ggroup(cont=dsqgrp, horizontal=TRUE))
  addSpring(dpgrp.buttons)
  
  ### Return all widgets, the data notebook and the buttons groups
  return (list(widgets=dlgWidgets, dataNotebook = dataNB, 
               infoActionsGrp = ggroup(cont=cont, horizontal=TRUE),
               dsqActionsGrp = dsqgrp.buttons, dsqActionsGrp2 = dsqgrp.postButtons, 
               dpActionsGrp = dpgrp.buttons))
}

####################
# DATA VIEW SCREEN #
####################

# function to show data of a plot
datasets.showPlotData<-function(window, data, title="View Data", height=500, width=700) {
  #FIXME: get order of compounds correctly
  dlg<-gbasicdialog(title, parent=window, horizontal=FALSE, expand=TRUE, do.buttons=FALSE)
  size(dlg)<-c(width, height)
  table<-gtable(data, cont = dlg, expand=TRUE)
  size(table)<-c(width, (height-40))
  dlgbuttons <- ggroup(container = dlg, horizontal=TRUE, expand=FALSE)
  addSpring(dlgbuttons)
  gbutton(action=gaction("Copy to\nclipboard", icon="gtk-copy", handler = function(h,...) cp.copyDF(data)), container=dlgbuttons)
  gbutton(action=gaction("Close", icon="gtk-cancel", handler = function(h,...) dispose(dlg)), container=dlgbuttons)
  visible(dlg)<-TRUE
}




##################################
# code for generic GUI functions #
# Copyright 2013 Sebastian Kopf  #
# seb.kopf@gmail.com             #
##################################

###########
# WINDOWS #
###########

# standard window
# -- makes gwindow or gdialogbasic depending on input
win.init <- function (title = "", width = 100, height = 100, modal = FALSE, parent=NULL) {
  # get window going
  if (modal) {
    gw <- gbasicdialog(title=title, do.buttons=FALSE, parent=parent) 
    size(gw) <- c(width, height)
  } else
    gw <- gwindow(title, visible=FALSE, width=width, height=height)
  return (gw)
}


##########
# COMBOS #
##########

# load combo
# sort=FALSE/TRUE does alphabetical sorting
combo.load<-function(combo, entries, sort=FALSE, selection=NULL, blockHandlers=NULL) {
  combo.blockHandlers(combo, blockHandlers)
  if (sort) # sort the combo
    entries<-entries[order(entries)]
  combo[]<-entries
  if (!is.null(selection))
    combo.setSelection(combo, selection)
  combo.unblockHandlers(combo, blockHandlers)
}

# add to combo
combo.add<-function(combo, entry, sort=FALSE, select=TRUE, blockHandlers=NULL) {
  if (select)
    selection<-entry # select new entry
  else
    selection<-1 # select first entry
  combo.load(combo, c(combo[], entry), sort=sort, selection=selection, blockHandlers=blockHandlers)
}

# delete selected entry
combo.deleteSelection<-function(combo, reSelect=TRUE, blockHandlers=NULL) {
  index<-combo.getSelection(combo, index=TRUE)
  if (reSelect) 
    selection<-min(index,length(combo[])-1) # select the second to last object if the delted one was the last one
  else
    selection<-1 # select first entry otherwise
  combo.load(combo, combo[-index], selection=selection, blockHandlers=blockHandlers)
}

# selection
combo.getSelection<-function(combo, index=FALSE) return (svalue(combo, index=index))
combo.setSelection<-function(combo, value) {
  if (is.numeric(value))
    svalue(combo, index=TRUE)<-value
  else
    svalue(combo, index=FALSE)<-value
}

# block handlers
combo.blockHandlers<-function(combo, handlerIDs){
  if (!is.null(handlerIDs))
    for (handlerID in handlerIDs)
      blockHandler(combo, handlerID)
}

# unblock handlers
combo.unblockHandlers<-function(combo, handlerIDs){
  if (!is.null(handlerIDs))
    for (handlerID in handlerIDs)
      unblockHandler(combo, handlerID)
}

##########
# TABLES #
##########

####### ENTRIES (get, set, add, edit, delete) ########

# get value of a table entry at the provided index
# if no field is given, returns whole row, if field is given (e.g. c("adsfs", "sdfd")) then only field
table.getValue<-function(table, index, fields=NULL) {
  if (!is.null(fields))
    return(table[][index, fields])
  else
    return(table[][index, ])
}

# get the selected index of a table
table.getSelectedIndex<-function(table) {
  if (!is.empty(index<-svalue(table, index=TRUE)))
    return (index)
  else
    return (NULL)
}

# get selected value from a table (depends on what is defined as the selection column, usually the first column)
# or if providing field param, get specific one
table.getSelectedValue<-function(table, field=NULL) {
  if (is.null(field) && !is.empty(value<-svalue(table, index=FALSE)))
    return (value)
  else if (!is.null(field) && !is.null(dataset<-table.getSelection(table)))
    return (dataset[field])
  return (NULL)
}

# get selected record from a gtable
table.getSelection<-function(table) {
  if (!is.null(index<-table.getSelectedIndex(table))) { # check if anything is selected
    if (is.null(nrow(table[]))) 
      return(table[]) # if there's only one object in the table
    else
      return(table[][index, ]) # if there's multiple objects, return just selected
  }
  return (NULL)
}

# select something in the table
# blocks the changed handler if provided during the selection
table.setSelectedValue<-function(table, value, index=FALSE, blockHandlers=NULL) {
  table.blockHandlers(table, handlers=blockHandlers) # block handlers
  svalue(table, index=index)<-value # set value (use index to decide whether by value or by index)
  table.unblockHandlers(table, handlers=blockHandlers) # unblock handlers
}

# delete selected entry in a data table
# reselect - reselects the record at the same position
# if blockChangedHandlerID is provided, it will block this handler before doing the reselection business (and then unblock it)
table.deleteSelection<-function(table, reSelect=FALSE, blockHandlers=NULL) {
  if (!is.null(index<-table.getSelectedIndex(table))) { # check if anything is selected
    if (!is.null(total<-nrow(table[]))) { # does not support deleting the last one
      table.blockHandlers(table, handlers=blockHandlers) # block handlers
      table[]<-table[][-index,]  # remove line
      if (reSelect) {
        if (index==total) index<-max(1,total-1) # select the second to last object if the delted one was the last one
        table.setSelectedValue(table, index, index=TRUE) 
      }
      table.unblockHandlers(table, handlers=blockHandlers)
    } else
      print("WARNING: can't delete the last item in a table")
  }
}

# update table entry
table.update<-function(table, data, index, ignoreMissing = FALSE) {
  for (field in names(data)) {
    if (field%in%names(table[])) # update table field
      table[][index,field]<-data[[field]]
    else if (!ignoreMissing)
      cat(paste("\nWARNING (to disable, pass ignoreMissing = TRUE):\n\tTrying to update table field", 
                  field, #"with value", data[[field]], 
                  "but field does not exist in table (", paste(names(table[]), collapse=", "), ")"))  
  }
}

# update currently selected table entry
table.updateSelection<-function(table, data, ignoreMissing = FALSE) {
  if (!is.null(index<-table.getSelectedIndex(table))) # check if anything is selected
      table.update(table, data, index, ignoreMissing = ignoreMissing)
}

# add an entry in the data table
# index --> add at specific index, if NULL, adds at the end of the table (or if index > length of table)
# returns index of newly added entry
table.add<-function(table, data, index=NULL, select=TRUE, blockHandlers=NULL, ignoreMissing = FALSE) {
  table.blockHandlers(table, handlers=blockHandlers) # block handlers
  if (!is.null(index) && index<=nrow(table[])) 
    table[][(index+1):(nrow(table[])+1),]<-table[][index:nrow(table[]),] # move records up one
  else
    index<-nrow(table[])+1 # adding at the end of the table
  table.update(table, data, index, ignoreMissing = ignoreMissing)
  
  if (select) 
    table.setSelectedValue(table, index, index=TRUE) 
  table.unblockHandlers(table, handlers=blockHandlers) # unblock handlers
  return (index)
}

# add an entry in the data table after the current selection
# if nothing is selected, adds a new entry at the end of the table
# returns index of newly added entry
table.addAfterSelection<-function(table, data, select=TRUE, blockHandlers=NULL, ignoreMissing = FALSE) {
  if ( !is.null(index<-table.getSelectedIndex(table)))
    index<-index + 1
  return (table.add(table, data, index=index, select=select, blockHandlers=blockHandlers, ignoreMissing = ignoreMissing))
}

####### HANDLERS ##########

# table handler blocks (This is only implemented this way because handler blocking does not seem to work for gtables!)
# important, need to implement this in the handlers though!!
table.blockHandlers<-function(obj, handlers=c("changed", "clicked")) for (handler in handlers) tag(obj, paste(handler,"Handler", sep=""))<-TRUE
table.unblockHandlers<-function(obj, handlers=c("changed", "clicked")) for (handler in handlers) tag(obj, paste(handler,"Handler", sep=""))<-FALSE
table.isHandlerBlocked<-function(obj, handler=NULL) {
  if (is.null(handler) || is.null(tag(obj, paste(handler,"Handler", sep=""))))
      return (FALSE)
  return (tag(obj, paste(handler,"Handler", sep="")))
}
  
###### TABLE SORTING ########

# datasets move up and down in tables
# move = c("up", "down", "top", "bottom"), pick one
# gvar = the name of a global variable to update
# tableID = if a global varaible is provided, use this field in the table to find the right entries in the global variable to update
# gvarID = if a global variable is provided, use this field to find the right entries to update
# gvarOrder = if a global variable is provided, use this field to keep track of the locations
table.moveHandler<-function(table, move=NULL, gvar=NULL, tableID="ID", gvarID="ID", gvarOrder="Order", blockHandlers=NULL) {
  if (!is.null(move)) { # only move if a move command is provided
    if (!is.empty(index<-svalue(table, index=TRUE))) { # make sure something is selected
      # block event handlers
      table.blockHandlers(table, handlers=blockHandlers) 
      
      # get new index
      newIndex<-switch(move, "up"=(index-1), "down"=(index+1), "top"=1, "bottom"=nrow(table[])) # find the record to exchange with
      if ( index!=newIndex && newIndex >= 1 && newIndex <= nrow(table[])) { # can move (no point moving outside the table or staying at the same place)
        
        # update global variable if provided
        if (!is.null(gvar)) {
          globalvar<-get(gvar, envir=.GlobalEnv) # get global variable
          
          ID<-table[index, tableID] # figure out which record in the global variable to update
          sort<-globalvar[[gvarOrder]][which(globalvar[[gvarID]]==ID)] # current sort 
          newID<-table[][newIndex, tableID] # figure out ID of new position
          newSort<-globalvar[[gvarOrder]][which(globalvar[[gvarID]]==newID)] # sort of new position
          
          if (move=="top")
            globalvar<-var.updateSorting(globalvar, newSort, sort, orderField=gvarOrder, changeBy=1) # update sorts
          else if (move=="bottom")
            globalvar<-var.updateSorting(globalvar, sort, newSort, changeBy=-1) # update sorts
          else
            globalvar[[gvarOrder]][which(globalvar[[gvarID]]==newID)]<-sort # update exchange item with current sort
          globalvar[[gvarOrder]][which(globalvar[[gvarID]]==ID)]<-newSort
          
          assign(gvar, globalvar, envir=.GlobalEnv) # assign global variable
        }
        
        ### UPDATE TABLE ###
        selectedRow<-table[][index,]
        if (move=="top")
          table[][2:index,]<-table[][1:(index-1),] # move everything down 1 that's above the item
        else if (move=="bottom")
          table[][index:(nrow(table[])-1),]<-table[][(index+1):nrow(table[]),] # move everything up 1 that's below the item
        else # up and down moves
          table[][index,]<-table[newIndex,] # exchange it with the exchange index
        
        # update new index position and select it
        table[][newIndex,]<-selectedRow
        svalue(table, index=TRUE)<-newIndex 
      }
      # unblock event handlers
      table.unblockHandlers(table, handlers=blockHandlers)
    }
  }
}

# provide the buttons for moving data in a table
table.moveButtons<-function(container, table, gvar=NULL, tableID="ID", gvarID="ID", gvarOrder="Order", blockHandlers=c("changed", "clicked") ) {
  moveButtonsGrp<-ggroup(horizontal=FALSE, cont=container)
  addSpring(moveButtonsGrp)
  gbutton(action=gaction("T", icon="gtk-goto-top", handler=function(h,...) table.moveHandler(table, move="top", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)), cont=moveButtonsGrp) 
  gbutton(action=gaction("U", icon="gtk-go-up", handler=function(h,...) table.moveHandler(table, move="up", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)), cont=moveButtonsGrp) 
  gbutton(action=gaction("D", icon="gtk-go-down", handler=function(h,...) table.moveHandler(table, move="down", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)),  cont=moveButtonsGrp)
  gbutton(action=gaction("B", icon="gtk-goto-bottom", handler=function(h,...) table.moveHandler(table, move="bottom", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)),  cont=moveButtonsGrp)
  addSpring(moveButtonsGrp)
}

####### special tables ########
# table that has toggle abalitiy
# returns a rGtkDataFrame whose data frame content can be accessed by []
table.toggleTable <- function(cont, df, toggleColumn, toggleFunc = NULL, invisibleColumns = NULL) {
  index <- which(names(df) == toggleColumn) # index of toggle column
  model <- rGtkDataFrame(df) # data frame model
  view <- gtkTreeView() # display structure
  
  # visible columns
  if (!is.null(invisibleColumns))
    excludeIndices <- c(index, which(names(df) %in% invisibleColumns))
  else 
    excludeIndices <- index
    
  # cell renderers
  cr <- gtkCellRendererToggle() # cell renderer for the check box
  cr['activatable'] <- TRUE
  
  # add cell renderers to table
  view$insertColumnWithAttributes (0, toggleColumn, cr, active = (index-1) ) 
  mapply(view$insertColumnWithAttributes, -1, colnames(df[-excludeIndices]), list(gtkCellRendererText()), text = (seq_along(df)[-excludeIndices]) -1)
  
  # signal for processing cell renderer
  gSignalConnect (cr, "toggled", function (cr, path, user.data ) {
    row <- (as.numeric(path) + 1)
    model <- user.data$getModel()
    #if (is.null(toggleFunc) || do.call(toggleFunc, list(data = model, row = row))) #FIXME --> write toggle func
    model[row, index] <- !model[row, index]
  }, data=view)
  
  # add model
  view$setModel(model)
  
  # add to a scroll window in container
  scrolled_window <- gtkScrolledWindow()
  getToolkitWidget(cont)$packStart(scrolled_window , expand = TRUE, fill = TRUE)
  scrolled_window$add(view)
  scrolled_window$setPolicy ("automatic", "automatic")
  
  # return model table --> can access data by model[]
  return(model)
}


###########
# GENERIC #
# WIDGETS #
###########

# load widgets from a data object
widgets.load<-function(widgets, data) {
  sapply(names(widgets), function(i) {
    if (i%in%names(data)) { #field exists in data
      if (class(widgets[[i]])[[1]]=="gTable") { # gtable style widgets
        if (class(data[[i]])=="list") # single record
          widgets[[i]][] <- data.frame(data[[i]], stringsAsFactors=FALSE)	
        else # multiple records
          widgets[[i]][] <- data[[i]]	
      } else # all other widgets
        svalue(widgets[[i]]) <- data[[i]]
    } else
      print(paste("WARNING: trying to load widget", i, "but no corresponding field found in dataset."))
  })
}

# get widgets into list
widgets.getValues<-function(widgets) {
  return (sapply(names(widgets), function(var) {
      tryCatch(list(widget.getValue(widgets[[var]])),
               warning=function(w) { return(NA) } ) # coerce with NA values      
    }))
}

# get widget value (returns gTable always as data frame)
widget.getValue<-function(widget) {
  if (class(widget)[[1]]=="gTable") #in case it's a gTable(i.e. data frame), have to access info slightly differently
    if (class(widget[])=="list") # single record is returned as a list rather than a dataframe
      return (data.frame(widget[], stringsAsFactors=FALSE))
    else # multiple records in table are returned properly as data frame
      return (widget[]) 
  else
    return(svalue(widget))
}

# get widgets into dataframe (WARNING: will collapse any widgets that are tables down to NULL, if you want to preserve those, use widgets.getValues instead to get a list)
widgets.getValuesAsDF<-function(widgets) {
  return (data.frame(sapply(names(widgets), function(var) {list(svalue(widgets[[var]]))}), stringsAsFactors=TRUE))
}

#################
# PLOT NOTBEOOK #
#################

#FOR TESTING PURPOSES
#win<-gwindow("blub")
#pn.GUI(gframe(cont=win, horizontal=FALSE), win)

# make GUI forplot notbook
# new plot objs = list() object defining what kind of parameters are on a plot object by default
# - the load handlers are just passed the currently selected plot object for doing whatever they want with it
# add event handlers to the plot as needed, currently supported: "droptarget", "Clicked", "Rightclick", "MouseMotion"
# --> pass like this plotEventHandlers=list(droptarget=fun, Clicked=fun)
pn.GUI<-function(container, window, newPlotObj=NULL, 
                 newPlotObjLoadHandler=NULL, plotObjLoadHandler=NULL, plotEventHandlers=list(),
                 enablePlotLabel=TRUE, enableMenuButtons=TRUE, startWithTab=TRUE){
  
  pn<-list() # plots notebook object
  pn$win<-window
  pn$enablePlotLabel<-enablePlotLabel
  
  # actions to interact with the plots
  #FIXME: figure out how to make keyboard accelerators work (should be key.accel="Control-n" and parent=win for gaction but always fails, not sure why)
  #NOTE: as of august 2013, the keyboard accelerators were not implemented for RGtk2
  pn$actions<-list(
    aNewPlot = list(label="New Plot", icon="gtk-page-setup", handler=function(...) pn.newPlotTab(pn, tabObj=newPlotObj, eventHandlers=plotEventHandlers, loadHandler=newPlotObjLoadHandler, label=paste("Plot", length(pn$plot.nb)+1, sep="")) ),
    aClosePlot = list(label="Close Plot", icon="gtk-cancel", handler=function(...) pn.deletePlotTab(pn, loadHandler=plotObjLoadHandler)), 
    aSavePlot = list(label="Save Plot", icon="gtk-save-as", handler=function(...) pn.savePlotGUI(pn, index=svalue(pn$plot.nb))), 
    aPrintPlot = list(label="Print Plot", icon="gtk-print", handler=function(...) pn.printPlot(pn, index=svalue(pn$plot.nb))), 
    aSaveAll = list(label="Save All", icon="gtk-harddisk", handler=function(...) pn.savePlotGUI(pn)))
  if (enableMenuButtons) {
    pn$buttons.grp<-ggroup(cont=container, horizontal=TRUE)
    addSpring(pn$buttons.grp)
    for (act in pn$actions)
      gbutton(action=gaction(act$label, icon=act$icon, handler=act$handler), cont=pn$buttons.grp)
  }
  
  # plots notebook
  pn$plot.nb <- gnotebook(cont=container, expand=TRUE)
  pn$plot.nb.changedHandler<-addHandlerChanged(pn$plot.nb, handler=function(h,...) pn.selectPlotTab(pn, h$pageno, loadHandler=plotObjLoadHandler))
  if (startWithTab)
    pn.newPlotTab(pn, tabObj=newPlotObj, label="Plot1", loadHandler=newPlotObjLoadHandler, eventHandlers=plotEventHandlers)
  
  return(pn)
}

# save handler
# save the plot with the provided index
# if none is provided, save all plots
pn.savePlotGUI<-function(pn, index=NULL){
  if (is.null(index)) { # save all plots
    f=gfile("Select the folder where to save all the plots.", type="selectdir", cont=pn$win)
  } else { # save index plot
    f=gfile("Select where to save this graph.", type="save", cont=pn$win, 
          initialfilename = paste(format(Sys.time(),format="%Y%m%d"),"_", names(pn$plot.nb)[index],".pdf", sep=""),
          filter = list("PDF Files" = list(patterns=c("*.pdf")), "All files" = list(patterns = c("*"))))
  }
  
  if (!is.na(f)){
    grp<-ggroup(cont=(w<-gwindow("Save plot as pdf", width=200, height=100, spacing=30)), horizontal=FALSE, expand=TRUE)
    dlggrp<-glayout(container=grp, spacing=10)
    dlggrp[1,1]<-glabel("Width [inches]:",con=dlggrp)
    dlggrp[1,2]<-(width <- gedit(8,container=dlggrp, coerce.with=as.numeric))
    
    dlggrp[2,1]<-glabel("Height [inches]:",con=dlggrp)
    dlggrp[2,2]<-(height <- gedit(6,container=dlggrp, coerce.with=as.numeric))
    
    #dlggrp[3,1]<-glabel("Unit:",con=dlggrp)
    #dlggrp[3,2]<-(units <- gcombobox(c("in","cm","mm"),container=dlggrp))
    
    gbutton("save", cont=grp, handler=function(h,...) {
      if (is.null(index)) { # save all
        for (i in 1:length(pn$plot.nb)) 
          pn.savePlot(pn, i, file.path(f, paste(format(Sys.time(),format="%Y%m%d"),"_", names(pn$plot.nb)[i],".pdf", sep="")), width=svalue(width), height=svalue(height))
      } else { # save just the current
        if (length(grep("\\.pdf$", f))==0) f<-paste(f,".pdf",sep="") # ensure .pdf ending
        pn.savePlot(pn, index, f, width=svalue(width), height=svalue(height))
      }
      pn.reactivatePlot(pn) # reactivate previously active plot
      dispose(w)
    })
  }
}

# save the plot with the given index
pn.savePlot<-function(pn, index, file, width=8, height=6) {
  pn.activatePlot(pn, index)
  dev.copy2pdf(file=file, width=width, height=height) # copy graph
}

# print the plot with the given index
pn.printPlot<-function(pn, index, width=8, height=6) {
  if (exists("win.print")) { # on windows, go print
    pn.activatePlot(pn, index)
    win.print(width=width, height=height) # launches print interface
    pn.activatePlot(pn, index) # reactivate graphics device
  } else 
    gmessage("Sorry, direct printing is not yet supported on Linux/MacOS.\nPlease save the plot as a pdf and print from there.")
}

# make new plot tab
# provide more detailed plot object if keeping other parametrs is desired
# add event handlers to the plot as needed, currently supported: "droptarget", "Clicked", "Changed", "Rightclick", "MouseMotion", "RightlickMousePopupmenu" 
# --> pass like this plotEventHandlers=list(droptarget=fun, clicked=fun)
pn.newPlotTab<-function(pn, tabObj=NULL, label="Plot", loadHandler=NULL, eventHandlers=list()) {
  # block handlers
  blockHandler(pn$plot.nb, pn$plot.nb.changedHandler)
  
  # make new tab
  grp<-ggroup(cont=pn$plot.nb, horizontal=FALSE, label=label)
  if (pn$enablePlotLabel)
    addHandlerKeystroke(gedit(label, cont=grp), handler=function(h,...) {pn.changePlotTabName(pn, svalue(h$obj))})
  gg<-ggraphics(cont=grp)
  blockHandler(obj=gg) # disable automatic 2nd mouse button popup handler (for save and copy)
   
  # event handlers
  if (!is.null(eventHandlers$droptarget))
    adddroptarget(gg, targetType="object", handler=eventHandlers$droptarget)
  if (!is.null(eventHandlers$Clicked))
    addHandlerClicked(gg, handler=eventHandlers$Clicked)
  if (!is.null(eventHandlers$Changed))
    addHandlerChanged(gg, handler=eventHandlers$Changed)
  if (!is.null(eventHandlers$Rightclick))
    addHandlerRightclick(gg, handler=eventHandlers$Rightclick)
  if (!is.null(eventHandlers$MouseMotion))
    addHandlerMouseMotion(gg, handler=eventHandlers$MouseMotion)
  if (!is.null(eventHandlers$RightlickMousePopupmenu))
    add3rdMousePopupmenu(obj=gg, menulist=eventHandlers$RightlickMousePopupmenu)
  
  # make new object
  if (is.null(tabObj))
    tabObj<-list() # new object
  tabObj$gg<-gg # store the graphics object
  
  if (length(pn$plot.nb) == 1) 
    tag(pn$plot.nb, "tabs")<-list()
  tag(pn$plot.nb, "tabs")[[length(pn$plot.nb)]]<-tabObj # add new object
  
  # load
  if (!is.null(loadHandler))
    do.call(loadHandler, list(obj=tabObj))
  
  # unblock handlers
  unblockHandler(pn$plot.nb, pn$plot.nb.changedHandler)
}

# change plot tab name (not stored in object)
pn.changePlotTabName<-function(pn, label) {
  names(pn$plot.nb)[svalue(pn$plot.nb)]<-label
}

# delete plot
# (deletes specific plot if index is passed in otherwise just the currently selected ones)
pn.deletePlotTab<-function(pn, index=NULL, loadHandler=NULL) {
  if (!is.null(index))
    svalue(pn$plot.nb)<-index
  else
    index<-svalue(pn$plot.nb)
  blockHandler(pn$plot.nb, pn$plot.nb.changedHandler) # block changed handler
  dispose(pn$plot.nb) #remove plot
  tag(pn$plot.nb, "tabs")[[index]]<-NULL # remove plot object
  unblockHandler(pn$plot.nb, pn$plot.nb.changedHandler) # unblock changed handler
  pn.selectPlotTab(pn, svalue(pn$plot.nb), loadHandler=loadHandler)
}

# set plot tab specifrically
pn.setPlotTab<-function(pn, plotI, loadHandler=NULL) {
  svalue(pn$plot.nb)<-plotI
  pn.selectPlotTab(pn, plotI, loadHandler=loadHandler)
}

#select plot
pn.selectPlotTab<-function(pn, plotI, loadHandler=NULL) {
  if (plotI<=length(tag(pn$plot.nb, "tabs"))) { # make sure this is not when adding a new plot #FIXME
    pn.activatePlot(pn, plotI)
    if (!is.null(loadHandler))
      do.call(loadHandler, list(obj=pn.getPlotTabParam(pn, plotI)))
  }
}

# activate graphics widget of a plot index
pn.activatePlot<-function(pn, plotI) {
  gg<-pn.getPlotTabParam(pn, plotI, params="gg") # set ggraphics visible
  visible(gg)<-TRUE
}

# reactivate currently selected graphics widget
pn.reactivatePlot<-function(pn) {
  pn.activatePlot(pn, svalue(pn$plot.nb))
}

#get plot properti(es) for a tab
#params as c("test", "test2")
pn.getPlotTabParam<-function(pn, index, params=NULL) {
  if (is.null(params))
    return (tag(pn$plot.nb, "tabs")[[index]])
  else if (length(params)==1)
    return (tag(pn$plot.nb, "tabs")[[index]][[params]])
  else
    return (tag(pn$plot.nb, "tabs")[[index]][params])
}

# get all plot tab objs
pn.getAllPlotTabObjs<-function(pn) return (tag(pn$plot.nb, "tabs"))

# get all plot tab names
pn.getAllPlotTabNames<-function(pn) return(names(pn$plot.nb))

# get selected plot tab name
pn.getSelectedPlotTabName<-function(pn) return(names(pn$plot.nb)[svalue(pn$plot.nb)])

# get them for the selected tab
pn.getSelectedPlotTabParam<-function(pn, params=NULL) return (pn.getPlotTabParam(pn, svalue(pn$plot.nb), params=params))

# set plot properti(es)
# params as list
pn.setPlotTabParam<-function(pn, index, params) {
  for (var in names(params))
    tag(pn$plot.nb, "tabs")[[index]][var]<-params[var]
}

# set them for the selected tab
pn.setSelectedPlotTabParam<-function(pn, params) pn.setPlotTabParam(pn, svalue(pn$plot.nb), params)

# utility function for storing user information within the current tab (with id "plotinfo")
# info = list of parameters
pn.storeInfo<-function(pn, info, reset=FALSE) {
  if (reset)
    plotinfo<-list()
  else
    plotinfo<-pn.getSelectedPlotTabParam(pn, params="plotinfo")
  for (name  in names(info))
    plotinfo[name]<-info[name]
  pn.setSelectedPlotTabParam(pn, list(plotinfo=plotinfo)) # save plot parameter
}

# utility function for retrieving all user information from the current tab
pn.getAllInfo<-function(pn) return(pn.getSelectedPlotTabParam(pn, params="plotinfo"))

# utility function for retrieving parts of the user information from the current tab
pn.getInfo<-function(pn, fields) return(pn.getAllInfo(pn)[fields])




###################################
# code for generic data functions #
# Copyright 2013 Sebastian Kopf   #
# seb.kopf@gmail.com              #
###################################

###############
# excel files #
###############

# reads a data frame from an excel sheet with the headers provided in the startRow
# trueColNames - gives the data frame the real names from the columns
excel.readDF <- function(file, sheet = 1, startRow = 1, stringsAsFactors=FALSE, trueColNames = TRUE) {
  df <- read.xlsx2(file, sheet, startRow=startRow, stringsAsFactors=stringsAsFactors, header=TRUE) 
  if (trueColNames) {
    dfcols <- read.xlsx(file, sheet, rowIndex=startRow, header=FALSE, stringsAsFactors=stringsAsFactors) 
    names(df) <- gsub("\\s*$", "", dfcols, perl=T) # trailing whitespaces removed
  }
  return(df)
}

# write data frames to an excel sheet
# file = file name/path
# df = either
#   - a data frame (data.frame(x=.., y=...))
#   - a list of data frames with the list IDs as sheet names (list("Sheet 1" = data.frame(x=.., y=...)))
excel.writeDF <- function(file, df) {
  wb <- createWorkbook(type="xlsx")
  csStd <- CellStyle(wb) + Font(wb)
  csBold <- CellStyle(wb) + Font(wb, isBold=TRUE) 
  if (identical(class(df), "data.frame")) { # single data frame
    sheet <- createSheet(wb, sheetName="Sheet1")
    addDataFrame(df, sheet, startRow=1, startColumn=1, colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))
  } else if (identical(class(df), "list")) { # multiple data frames
    for (dfi in 1:length(df)) {
      sheetName <- names(df)[dfi]
      if (identical(sheetName, "")) # no sheet name given
        sheetName <- paste0("Sheet", dfi)
      sheet  <- createSheet(wb, sheetName=sheetName)
      addDataFrame(df[[dfi]], sheet, startRow=1, startColumn=1, colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))
    }
  }
  saveWorkbook(wb, file) 
}

#################
# data transfer #
#################

# function to output a dataframe to the clipboard (can then be easily copied into excel for example)
cp.copyDF<-function(df) {
  if (exists("writeClipboard")) # windows
    clipboard <- "clipboard"
  else # unix/MacOS
    clipboard <- pipe("pbcopy", "w")
    
  write.table(df, file=clipboard, sep="\t", row.names=FALSE)
    
  if (!exists("writeClipboard")) # unix
    close(clipboard)
}

# function to paste clipboard to data frame
cp.pasteDF<-function(header=TRUE, sep="\t", skip=0, comment.char="#", row.names=NULL, quote=""){
  return(read.clipboard(sep=sep, stringsAsFactors=FALSE, header=header, 
                        skip=skip, comment.char=comment.char, row.names=NULL, quote=quote))
}

#############################
# reading binary data files #
#############################
# NOTE: using the 010 Editor software, figuring out structures will be easier!

# read whole binary file and return the raw data
bin.readFile<-function(path) {
  con<-file(path, "rb")
  rawdata<-readBin(con, raw(), n=file.info(path)$size)
  close(con)
  return(rawdata)
}

# assemble read structure for binary file
# either pass in vectors and it will assemble a dta frame from it or pass in structure to amend
bin.struct<-function(what, length, size = 1, id = NA, struct = NULL) {
  df<-data.frame(id=id, what=what, length=length, size=size, stringsAsFactors=FALSE)
  if (!is.null(struct))
    df<-rbind(struct, df)
  return (df)
}

# provides the total length (in bytes) of a structure
bin.structLength<-function(struct) {
  return(sum(struct$length*struct$size))
}

# read from byte stream (data has to be a byte stream, structure has to be a structure data frame)
# returns a list coded by the ids in the structure
bin.parseStruct<-function(data, struct, offset = 0, saveUnknown = TRUE) {
  results <- list()
  pos <- offset + 1
  size <- length(data)
  for (i in 1:nrow(struct)) {
    id<-struct$id[i]
    if (!is.na(id) || saveUnknown) { # only process if real data or saving unknowns
      
      # different reads
      if (struct$what[i] == "raw")
        read<-paste(readBin(data[pos:size], "raw", n=struct$length[i], size=struct$size[i]), collapse=" ")
      else if (struct$what[i] == "character" && struct$size[i] == 1)
        read<-rawToChar(readBin(data[pos:size], "raw", n=struct$length[i], size=1))
      else if (struct$what[i] == "character" && struct$size[i] == 2)
        read<-paste(readBin(data[pos:size], "character", n=struct$length[i], size=2), collapse="")
      else
        read<-readBin(data[pos:size], struct$what[i], n=struct$length[i], size=struct$size[i])
      
      # saving results
      if (is.na(id))
        id <- "unknown"
      if (!is.null(results[[id]]))
        results[[id]]<-c(results[[id]], list(read))
      else
        results[[id]]<-read
    }
    pos <- pos + struct$size[i] * struct$length[i]
  }
  return(results)
}

# optimized method for repeat reading of the same structure (say to read an entire data array)
# WARNING: only supported for structures where each element is of length=1 (otherwise, mayhem!)
bin.multiParseStruct<-function(data, struct, rep, offset = 0) {
  datalength <- bin.structLength(struct)
  subdata<-data[(offset+1):(offset+rep*datalength)]
  structpos<-0
  df<-data.frame(read = 1:rep)
  for (i in 1:nrow(struct)) {
    byteselect<-rep(FALSE, datalength)
    byteselect[(structpos+1):(structpos<-structpos+struct$size[i])]<-TRUE
    if (!is.na(struct$id[i]))
      df[struct$id[i]]<-readBin(subdata[which(rep(byteselect, times=rep))], struct$what[i], size=struct$size[i], n=rep)
  }
  return (df)
}

# find all ascii strings in a data stream
# FIXME: it appears that after each string, there are 3x null character (i.e. 00 00 00) --> use this to make finding strings better! (couldn't quite figure out how to recognize 00 characters)
bin.findAllAscii<-function(data, minlength=10) {
  regexp<-paste("[\u0020-\u007e]{", minlength, ",}", sep="")
  text<-data.frame(
    byteStart = grepRaw(regexp, data, all=TRUE), #get ANSII strings
    value = ldply(grepRaw(regexp, data, all=TRUE, value=TRUE), 
                  function(x) rawToChar(x))$V1, encoding='ASCII', stringsAsFactors=FALSE)
  text$byteEnd<-text$byteStart + nchar(text$value) - 1
  text$byteLength<-text$byteEnd - text$byteStart + 1
  text$strLength<-text$byteLength
  return (text)
}

# find all unicode strings in a binary data stream
bin.findAllUnicode<-function(data, minlength=5) {
  regexp<-paste("([\u0020-\u007e][^\u0020-\u007e]){", minlength, ",}", sep="")
  text<-data.frame(
    byteStart = grepRaw(regexp, data, all=TRUE), #get Unicode strings
    value = ldply(grepRaw(regexp, data, all=TRUE, value=TRUE), 
                  function(x) rawToChar(x[c(TRUE, FALSE)]))$V1,
    #paste(readBin(x, "character", n=length(x)/2, size=2), collapse=""))$V1, 
    encoding='Unicode', stringsAsFactors=FALSE)
  text$byteEnd<-text$byteStart + nchar(text$value) * 2 - 1
  text$byteLength<-text$byteEnd - text$byteStart + 1
  text$strLength<-text$byteLength/2
  return (text)
}

# find all text in a binary data stream
bin.findAllText<-function(data, asciiL=10, unicodeL=5) {
  text<-rbind(bin.findAllAscii(data, minlength=asciiL), bin.findAllUnicode(data, minlength=unicodeL))
  text<-text[order(text$byteStart),] # sort all text
  text$byteGap<-diff(sort(c(text$byteStart, text$byteEnd, length(data))))[c(FALSE,TRUE)] # add byte gap
  return(text)
}

# clean up text by removing randomly found strings that are clearly not proper targets
bin.cleanText<-function(text, removeText="Arial", removePattern = "[&{}!^@?#]", unlessByteLength = 26, unlessText = "Is Ref.?", printRemoved = TRUE) {
  rem<-union(
    which(text$value==removeText),
    intersect(grep(removePattern, text$value), which(text$byteLength < unlessByteLength & !(text$value%in%unlessText))))
  if (printRemoved) {
    cat("\nRemoved:\n")
    print(text[rem,"value"])
  }
  text<-text[-rem,]
  # re calculate gaps between occuring strings
  text$byteGap<-diff(sort(c(text$byteStart, text$byteEnd, max(text$byteEnd))))[c(FALSE,TRUE)] # add byte gap
  return (text)
}

####################################
# data frame convenience functions #
####################################

# get indices of columns by name
df.getColIs <- function(df, cols) {
  indices <- sapply(cols, function(col) which(names(df) == col)[1])
  if (!is.empty(which(naIs <- is.na(indices))))
    warning(paste("Some column names were not found:", cols[naIs] ))
  return(indices[!naIs])
}

# delete certain columns from data frame (by name or indices)
df.delCols<-function(df, cols) {
  if (mode(cols)=="numeric") #referring to indices
    return (df[-cols])
  else
    return (df[!names(df) %in% cols]) #referring to names 
}

# rename certain columns
# if "from" is omitted, "to" is expected to contain names for all columns
df.nameCols<-function(df, to, from) {
  if (missing(from)) 
    names(df) <- to
  else {
    for (i in 1:length(from)){
      if (!is.empty(col<-which(names(df)==from[i])))
        names(df)[col]<-to[i]
    }
  }
  return (df)
}

# change the data type of several columns
# type
# - integer
# - numeric
# - excelDate
# - excelTimestamp
df.changeDataType<-function(df, cols, type="numeric") {
  for (i in df.getColIs(df, cols)) {
    if (identical(type, "excelDate"))
      df[,i] <- as.Date(as.integer(df[,i]) - 25569, origin="1970-01-01") # convert from excel date
    else if (identical(type, "excelTimestamp"))
      df[,i] <- as.POSIXct((as.numeric(df[,i])-25569)*86400, tz="GMT", origin="1970-01-01")
    else  
      mode(df[,i])<-type
  }
  return (df)
}

# convert all numeric columns in a data frame to true numeric
df.convertNumerics<-function(df) {
  for (i in which(sapply(df, is.numeric))) df[[i]]<-as.numeric(df[[i]])
  return(df)
}

# change date/time to time diff (makes a column with the name of units)
# baseDT
#	- if omitted, take first entry as reference
#	- if int, take this element of the dataset as BT
#	- actualy date time --> make by strToDateTime("<date>","<time>")
# valid units --> "hours", "days" (probably some others too)
df.dateTimeToDiff<-function(df, baseDT, col=c("date","time"), units="hours", format="%m/%d/%y %H:%M") {
  datetime<-strToDateTime(do.call(paste,c(df[col])), format=format)
  if(missing(baseDT)) baseDT<-datetime[1]
  if(mode(baseDT)=="numeric") baseDT<-datetime[baseDT]
  df[units]<-timeDiff(datetime,baseDT,units=units)
  return(df)
}

# add experimental info to reshaped data set
# col = name of new column, e.g. "strain" (or old column if want to overwrite, resetcol if you want to reset the whole thing)
# info = list coded with the new information
#   e.g. list("PA14"=c(1,4,5), "MAI-1"=c(2,3)), where 1-5 are the ids of the different experimental conditions (after doing df.reshapeAvgs or reshapeRawData)
# regexp (default: FALSE), how to look for these values, by default it looks by fixed string, set regexp to TRUE to evaluate a regular expression, NOTE: additional parameters are passed to the regexp function
# resetcol ( default: TRUE) --> resets the column entirely, otherwise only valuse specified by the matching conditions are set
# asnumeric --> decide whether the newly created values are numerics
df.addInfoCol<-function(df, col, info, id="exp", regexp=FALSE, resetCol=FALSE, asNumeric=FALSE, ...) {
  if (resetCol==TRUE || !(col%in%names(df)))
    df[col]<-NA # make new column / overwrite existing data in it
  for (i in 1:length(info)) {
    value<-names(info)[i]
    idvals<-info[[i]]
    for (idval in idvals) {
      if (regexp)
        matches<-grep(idval, df[[id]],...)
      else
        matches<-which(df[[id]]==idval)
      if (!is.empty(matches)) df[matches,col]<-value
    }
  }
  if (asNumeric)
    df[[col]]<-as.numeric(df[[col]])
  return(df)
}


################################
# generic data variable access #
################################

# get index by id
var.getIndexByID<-function(var, id, idField="ID") {
  if (!is.null(id) && !is.empty(sel<-which(var[[idField]]==id)))
    return(sel) #selected index in dataset
  return (NULL)
}

# get entry by id
var.getEntryByID<-function(var, id, idField="ID", fields=NULL) {
  if (!is.null(index<-var.getIndexByID(var, id, idField=idField))) {
    if(class(var) == "data.frame") { # data.frame
      if (is.null(fields)) 
        return (var[index,]) # return record
      else
        return (var[index, fields]) # return specific fields
    } else if (class(var) == "list") { #list
      if (is.null(fields)) 
        return (lapply(var,"[[",index))
      else if (length(fields) == 1) # return just this one field
        return (var[[fields]][[index]])
      else # multiple records
        return (lapply(var[fields],"[[",index))
    } else
      stop("data structure type not supported")
  }
  return (NULL)
}

# new object with fields whatever is passed in
# returns a list
var.new<-function(...) {
  return (list(...))
}

# new object with fields whatever is passed in
# returns as data frame
var.newAsDF<-function(...){
  return (data.frame(var.new(...), stringsAsFactors=FALSE))
}

# add entry in variable
# returns var
# supports reordering if desired (just supply currentOrderN to insert after this position)
# FIXME: ordering not tested/supported(?) for lists
var.add<-function(var, newId, data, idField="ID", orderField="Order", orderN=NULL) {
  if ( !(newId%in%var[[idField]])) {
    # reordering:
    if (!is.null(orderN)) { # insert at this orderN
      var<-var.updateSorting(var, orderN, orderField=orderField) # update all orderNs
      data[[orderField]]<-orderN
    } 
    # insert new entry
    if (class(var) == "data.frame") {
      newIndex<-nrow(var)+1
      var[newIndex,idField]<-newId
    } else if (class(var) == "list") {
      newIndex<-length(var[[idField]])+1
      var[[idField]][[newIndex]]<-newId
    }
    var<-var.update(var, newId, data, idField=idField)
    return (var)
  } else 
    stop(paste("ERROR: failed to add data set with new ID", newId, "because this ID already exists in the dataset."))
}

# update entry in variable (by default identified with idField but can go via index)
# returns updated variable
# WARNING: this might only work well with data frame fields that are NOT FACTORS
var.update<-function(var, id, data, idField="ID") {
  if (!is.null(index<-var.getIndexByID(var, id, idField=idField))) {
    for (field in names(data))
      if (field%in%names(var)) { # update varialbe
        # get new value (either from list or data frame)
        if (class(data)=="data.frame")
          newValue<-data[1,field]
        else if (class(data)=="list")
          newValue<-data[[field]]
        
        # set value depending on what you're updating (list or data frame)
        if (class(var)=="data.frame")
          var[index,field]<-newValue
        else if (class(var)=="list")
          var[[field]][[index]]<-newValue
        
      } else
        print(paste("WARNING: trying to update table field", field, "with value", data[[field]], "but field does not exist in variable.")) 
  }
  return (var)
}

# delete entry
# returns updated variable
var.delete<-function(var, id, idField="ID") {
  if (!is.null(index<-var.getIndexByID(var, id, idField=idField))) {
    if (class(var)=="data.frame") # delete record from data.frame
      var<-var[-var.getIndexByID(var, id, idField=idField),] 
    else if (class(var)=="list") { # delete record from list
      for (field in names(var)) {
        if (class(var[[field]])=="list") #in case it's a sublist, need to set elements to NULL
          var[[field]][[index]]<-NULL
        else # in case it's a direct array (e.g. numeric, character, logical)
          var[[field]]<-var[[field]][-index]
      }
    }
  }
  return(var) 
}

################
# data sorting #
################

# update order in a variable
# updateN.from - the ordering number from which on (inclusive, >=) all sorting numbers should be updated 
# updateN.to - if not NULL (default), the ordering number until which (inclusive, <=) all ordering numbers should be updated
#            - if NULL, goes till the end (nrow(table[]))
# returns the updated variable
# can changeBy 1 or -1 depending on need
var.updateSorting<-function(var, updateN.from, updateN.to=NULL, orderField="Order", changeBy=1) {
  if (is.null(updateN.to))
    updateN.to<-max(var[[orderField]], na.rm=TRUE)
  if ( !is.null(updateN.from) && !is.empty(indices<-which(var[[orderField]]>=updateN.from & var[[orderField]]<=updateN.to)) ) 
    var[[orderField]][indices]<-var[[orderField]][indices]+changeBy
  return (var)
}


#####################################
# Utility functions  			        	#
# Copyright 2013 by Sebastian Kopf  #
#####################################

######################
# variable functions #
######################

# get all variables/functions/everything defined in the passed in environment as a list with the variable name = variable object
# e.g. to get a list of all variables in a function, just call ls.asList(environment()) from within the function
# params
#  env = the environment to get do ls() on
#  exclude - a vectr of variable/function names to exclude from the list
ls.asList<-function(env, exclude=c()) sapply(setdiff(ls(env=env), exclude), FUN=function(i) list(get(i, env=env)))

#############################
# small scale utility funcs #
#############################

# checks if lenght of the variable is 0 and returns TRUE if it is
# also returns TRUE if variable is NA or NULL
is.empty<-function(variable)
  return (is.null(variable) || is.na(variable) || length(variable) == 0)

# convert strings to date time
strToDateTime<-function(dateStr, timeStr, format="%m/%d/%y %H:%M"){
  if(!missing(timeStr)) dateStr<-paste(dateStr,timeStr)
  return(strptime(dateStr, format))
}

#timeDiff function
timeDiff<-function(dateTime, baseDT, units="days")   
  return (as.numeric(difftime(dateTime,rep(baseDT,length(dateTime))),units))



#####################################
# Graphing functions        				#
# Copyright 2013 by Sebastian Kopf  #
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


