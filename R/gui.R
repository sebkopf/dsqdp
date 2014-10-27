#' GUI launch function
launch_dsqdp <- function() {
  
  # make sure RGtk2 is loaded
  options("guiToolkit"="RGtk2")
  
  # keep track of whether the DSQDP is running
  DSQDP.running <<- TRUE
  
  # show a small loading window
  load_win <- gwindow("Loading DSQDP...", width=400, height=100)
  load_grp <- ggroup(horizontal=FALSE, expand=TRUE, cont=load_win)
  addSpring(load_grp, expand=T)
  glabel("Loading User Interface, please wait...", cont=load_grp)
  addSpring(load_grp, expand=T)
  
  # modal dialog
  #gw <- gbasicdialog(
  #  title=paste("DSQ / D+ Data Processor", packageVersion('dsqdp'),"- Workspace:", getwd()), do.buttons=FALSE) #FIXME add parent = 
  #size(gw) <- c(1400, 700)
  
  gw <- gwindow(paste("DSQ / D+ Data Processor", packageVersion('dsqdp'), "- Workspace:", getwd()), width=1400, height=700, spacing=10, visible = FALSE)
  
  addHandlerDestroy(gw, handler=function(h,...) { message("\nGoodbye!!"); DSQDP.running <<- FALSE })
  
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
  gbutton(action=gaction("Backup\nWorkspace", icon="gtk-harddisk", tooltip="Save all data and back it up.", 
                         handler=function(h,...) { 
                           datasetListsHandler.storeSessionList(); 
                           save_dsqdp(backup = FALSE); # save regularly
                           save_dsqdp(backup = TRUE); # save backup
                           gmessage("Backup files saved in the 'backups' folder.") }), cont=plot.buttons.right)
  gbutton(action=gaction("Save &\nClose", tooltip="Save workspace and close DSQDP", icon="gtk-save-as", 
                         handler=function(h,...) { 
                           datasetListsHandler.storeSessionList(); 
                           save_dsqdp(backup = FALSE); 
                           dispose(gw) }), cont=plot.buttons.right)
  gbutton(action=gaction("Close",icon="gtk-stop", 
                         handler=function(h,...) { 
                           if(gconfirm("Are you sure you want to close without saving? All changes made since you last saved the workspace will be lost.")) 
                             dispose(gw)
                         }), cont=plot.buttons.right)
  
  ###### init #######
  # attach init event to focus handler (enables even modal dialog loading)
  visHandler <- addHandlerFocus(gw, handler=function(...) {
    # initiate screen (load whatever needs to be loaded )
    table.setSelectedValue(dsGUI$datasets.table, 1, index=TRUE, blockHandlers=c("changed", "clicked")) # select first dataset in the table 
    dataset.selectionHandler() # load it
    blockHandler(gw, ID=visHandler)
  })
  
  dispose(load_win)
  visible(gw, TRUE)
}