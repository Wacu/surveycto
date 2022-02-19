#' Generates Rscript to add variable labels
#'
#' @param xlsform Latest XLSFORM used to collect data.
#' @param dataName name given to the data.frame collected using the XLSFORM
#' stated
#'
#' @return
#' @export
#'
#' @examples
#' Not run
#' rodkvar(xlsforn,'dataName')
rodkvar<- function(xlsform,dataName) {
  suppressMessages(library(dplyr))
  ###Extracting the XLSFORM path
  file_xls<-dirname(xlsform)
  labfile<- basename(xlsform)
  ################################   survey   ################################
  survey <- as.data.frame(xlsx::read.xlsx2(xlsform,sheetName='survey'))%>%
    dplyr:: select(type,name,label)%>%
    filter(grepl('select_one|select_multiple|integer|text|calculate',type))%>%   ###Filtering variables that need value labels
    mutate(type= trimws(gsub("select_one","",
                             gsub("select_multiple","",
                                  gsub("'", "",gsub("\"", "",
                                                    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",type, perl=TRUE)))))),
           label=gsub('<[^>]+>','',gsub("select_one","",
                                        gsub("select_multiple","",
                                             gsub("'", "",gsub("\"", "",
                                                               gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",label, perl=TRUE)))))),
           var_labels=paste(name,"=\"",str_replace_all(label, "[[:punct:]]", " "),"\")",sep=""))%>%
    mutate(var_labeeds=paste(dataName," <- expss::apply_labels(",dataName,",",var_labels,sep="")) %>%
    dplyr:: select(var_labeeds)
  varlabeld<- paste(paste(file_xls,"/",sep=''),"ODK ",tools::file_path_sans_ext(labfile),"Variables Labels.R",sep=" ")
  writeLines(as.character(survey$var_labeeds),varlabeld)
  # for (i in 1:nrow(survey)){
  #   eval(parse(text=paste(survey$var_labeeds[i],sep="")), envir=.GlobalEnv)
  # }
  # varnolabel<-warnings()
}
