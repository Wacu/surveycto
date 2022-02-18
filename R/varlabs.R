#' Compile Variable labels into data.frame =
#'
#' @param xlsform  Lastest XLSForm used to collect data.
#'
#' @return
#' @export
#'
#' @examples
varlabs<- function(xlsform) {
  suppressMessages(library(dplyr))
  ################################## choices  ###################################
  #For single select variables
  ###Extracting the XLSFORM path
  file_xls<-dirname(xlsform)
  labfile<- basename(xlsform)
  ###Including multiple variables
  ###Setting the directory to save the scripts and the basename of the electronic survey.
  survey <- as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='survey'))%>%
    filter(grepl('select_multiple|select_one|integer|text|calculate',type)) %>%
    dplyr:: select(name, label)%>%
    dplyr:: select(name, label) %>%
    filter(label!= "" )%>%
    na_if("") %>%
    mutate(labels=paste('[',name,'] ',gsub('<[^>]+>','',gsub("select_one","",
                                                             gsub("select_multiple","",
                                                                  gsub("'", "",gsub("\"", "",
                                                                                    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",label, perl=TRUE)))))),sep = ''))%>%
    dplyr:: select(labels) %>% filter(labels!=" ")

  return(survey)
}
