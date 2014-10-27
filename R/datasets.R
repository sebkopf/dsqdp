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
    
    g.datasets$Sel<<-ds.table[,,drop=F]$Sel<-rep("No", each=nrow(ds.table[,,drop=F])) # select none by default
    if ( ( is.infinite(ids<-g.datasetLists[[svalue(dsLists.combo)]] )[1]) ) # select all
      g.datasets$Sel<<-ds.table[,,drop=F]$Sel<-rep("Yes", each=nrow(ds.table[]))
    else if (length(ids) > 0 && ids[1] != 0)  # select from the saved list
      g.datasets$Sel[which(g.datasets$ID%in%ids)]<<-ds.table[,,drop=F]$Sel[which(ds.table[,,drop=F]$ID%in%ids)]<-"Yes"
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
  if ( !is.empty(selected<-which(ds.table[,,drop=F]$Sel=="Yes")) )
    g.datasetLists[[dsList.name]]<<-ds.table[,,drop=F]$ID[selected]
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
  peaklist<-peaklistTable[,,drop=F]
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
      datapeaks[][which(datapeaks[]["RT"]==peakrt),c("ID","Name")]<-compoundsTable[,,drop=F][which(compoundsTable[]["ID"]==idval),c("ID","Name"), drop=FALSE]
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
      table[,,drop=F][index,field]<-data[[field]]
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
  if (!is.null(index) && index<=nrow(table[,,drop=F])) 
    table[,,drop=F][(index+1):(nrow(table[,,drop=F])+1),,drop=F]<-table[,,drop=F][index:nrow(table[]),,drop=F] # move records up one
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
        selectedRow<-table[,,drop=F][index,]
        if (move=="top")
          table[,,drop=F][2:index,]<-table[,,drop=F][1:(index-1),] # move everything down 1 that's above the item
        else if (move=="bottom")
          table[,,drop=F][index:(nrow(table[,,drop=F])-1),]<-table[,,drop=F][(index+1):nrow(table[,,drop=F]),] # move everything up 1 that's below the item
        else # up and down moves
          table[,,drop=F][index,]<-table[newIndex,] # exchange it with the exchange index
        
        # update new index position and select it
        table[,,drop=F][newIndex,]<-selectedRow
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

