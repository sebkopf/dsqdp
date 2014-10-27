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


