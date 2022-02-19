#' Compile Single Select variable value label into data.frame.
#'
#' @param xlsform Lastest XLSFORM used to collect data.
#'
#' @return
#' @export
#'
#' @examples
#' NOT run
#' rodksinglelab(xlsform)
rodksinglelab<- function(xlsform) {
  suppressMessages(library(dplyr))
  multiple<-as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='survey'))%>%
    dplyr::select(type,name)%>%
    filter(grepl('select_one',type))%>%
    mutate(list.name=trimws(gsub("select_one","",type)),
           variable_name=name) %>%
    dplyr::select(-name)
  mult_choice<-xlsx::read.xlsx2(xlsform,sheetName='choices')
  colnames(mult_choice)[grepl('list',colnames(mult_choice))] <- 'list.name'
  mult_choice<- mult_choice %>%
    dplyr:: select(list.name,name,label)%>%
    filter(!is.na(list.name))%>%
    filter(list.name!="")%>%
    mutate(label=gsub('\'','',label,fixed = TRUE))
  combined_labs <- left_join(multiple,mult_choice,by="list.name") %>%
    mutate(type=trimws(gsub("select_one","",type)),
           variable=variable_name,Value=name,Name=label)%>%
    dplyr::select(variable,Value,Name)%>%
    mutate(variable=paste(variable,"_vlab",sep=""),value=Value,name=Name) %>%
    dplyr:: select(variable,name,value)
  single_list<- unique(na.omit(gsub("_vlab",'',combined_labs$variable)))
  ll_single<-list(combined_labs,single_list)
  return(ll_single)
}
