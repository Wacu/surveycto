#' Generates a Script to add single variable value labels.
#'
#' @param xlsform Latest XLSFORM used to collect the data.
#' @param dataName name of the data, which was collected using the XLSFORM
#' stated above.
#'
#' @return
#' @export
#'
#' @examples
rodkval<- function(xlsform,dataName) {
  suppressMessages(library(dplyr))
  ################################## choices  ###################################
  #For single select variables
  ###Extracting the XLSFORM path
  file_xls<-dirname(xlsform)
  labfile<- basename(xlsform)
  choices <- as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='choices'))
  colnames(choices)[grepl('list',colnames(choices))] <- 'list.name'
  colnames(choices)[grepl('value',colnames(choices))] <- 'name'
  choices <- choices %>%
    filter(!is.na(list.name))%>%
    filter(list.name!="")%>%
    mutate(name=suppressWarnings(as.numeric(as.character(name))))%>%
    drop_na(name)%>%
    mutate(value_s=paste(paste('\"',str_replace_all(label, "[[:punct:]]", " "), '\"', sep=""),name,sep = " = "))%>%
    dplyr:: select(list.name, value_s)%>%
    plyr::ddply(., .(list.name), plyr:: summarize, value_s=paste(value_s, collapse=","))%>%
    mutate(type=list.name)
  survey_varname<- as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='survey'))%>%
    dplyr:: select(name,type)%>%
    filter(grepl('select_one',type))%>%
    mutate(type=trimws(gsub("select_one","",type)))%>%
    dplyr:: select(name,type)%>%
    full_join(choices,by = 'type')%>%
    drop_na(value_s)%>%
    filter(!is.na(name))%>%
    mutate(val_labs=paste(dataName," <- expss::apply_labels(",dataName,','," ",name,"=",'c(',value_s,'))',sep=""))%>%
    dplyr:: select(val_labs)
  vallabels<- paste(paste(file_xls,'/',sep=''),"ODK ",tools::file_path_sans_ext(labfile),"Single Select Value Labels.R",sep=" ")
  writeLines(as.character(survey_varname$val_labs),vallabels)
  #   for (i in 1:nrow(survey_varname)){
  #   eval(parse(text=paste(survey_varname$val_labs[i],sep="")), envir=.GlobalEnv)
  # }
}
