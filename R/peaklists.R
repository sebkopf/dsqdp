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


