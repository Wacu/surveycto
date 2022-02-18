#' Compiling Multiple Variable Choices.
#'
#' @param xlsform  Latest XLSFORM used to collect data.The variable and value
#' labels will be used to label the data.
#' @param dataName The name of the data in the memory collected using the
#' survey.
#'
#' @return
#' @export
#'
#' @examples
rodkmult<- function(xlsform,dataName) {
  suppressMessages(library(dplyr))
  ###Extracting the XLSFORM path
  file_xls<-dirname(xlsform)
  labfile<- basename(xlsform)
  ###Setting the directory to save the scripts and the basename of the electronic survey.
  multiple<-as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='survey'))%>%
    dplyr::select(type,name)%>%
    filter(grepl('select_multiple',type))%>%
    mutate(list.name=trimws(gsub("select_multiple","",type)),
           variable_name=name) %>%
    dplyr:: select(-name)
  mult_choice<-as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='choices'))
  colnames(mult_choice)[grepl('list',colnames(mult_choice))] <- 'list.name'
  colnames(mult_choice)[grepl('value',colnames(mult_choice))] <- 'name'
  mult_choice <- mult_choice %>%
    dplyr:: select(list.name,name,label)%>%
    filter(!is.na(list.name))%>%
    filter(list.name!="")%>%
    mutate(label=gsub('\'','',label,fixed = TRUE))
  DataC <- left_join(multiple,mult_choice,by="list.name") %>%
    mutate(type=trimws(gsub("select_multiple","",type)),
           new_var=paste(variable_name,gsub("[^[:alnum:]]", " ", name),sep = '_'))%>%
    dplyr:: select(new_var,label) %>%
    mutate(mult_var=paste(new_var,paste("\"",label, "\",", sep=""),sep=" = ")) %>%
    dplyr:: select(mult_var)
  DataC[1,]<-paste(dataName,"<- expss:: apply_labels(",dataName,',',DataC$mult_var[[1]],sep = ' ')
  DataC[nrow(DataC),]<-gsub('\",',"\")",DataC$mult_var[[nrow(DataC)]])
  varlabels<- paste(paste(file_xls,"/",sep=''),"ODK ",tools::file_path_sans_ext(labfile),"Multiple Select Variables Labels.R",sep=" ")
  writeLines(as.character(DataC$mult_var),varlabels)
}


