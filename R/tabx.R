#' Tabulates single select variables by group.
#'
#' @param row  Variable to tabulate
#' @param column Group variable.
#' @param data Data collected using the ODK platform
#'
#' @return
#' @export
#'
#' @examples
#' tabx('row','column','data')
tabx <- function(row,column,data) {
  suppressMessages(library(dplyr))
  dof<- addcols(data,{row}) %>%
    dplyr::select({row},{column})
  if (suppressWarnings(max(dof[,1],na.rm = TRUE))=="1" & suppressWarnings(min(dof[,1],na.rm = TRUE))=="1") {
    tabs<-table(dof[,1],dof[,2],useNA="no")
    # print(tabs)
    # print(row.names(tabs))
    # tabs<-tabs[!(rownames(tabs) %in% "0"),]
  }else {
    tabs<-table(dof[,1],dof[,2],useNA="no")
  }
  tab<-cbind(tabs,Overall = rowSums(tabs,na.rm = FALSE))
  cpercent <-tab
  ncol(cpercent)
  cpercent <-tab
  for(i in 1:ncol(tab)) { cpercent[,i] <-paste(tab[,i],"(",ifelse(tab[,i]==0,"0.0",format(round(tab[,i]/colSums(tab)[i]*100, digits=1),trim=TRUE)),"%)", sep="")}
  #colnames
  cnames<-colnames(tab)
  colnames(cpercent) <- cnames
  #rownames
  rnames <- rownames(cpercent)
  l<-c()
  for (k in rnames ){
    vls<- sing_value_label[sing_value_label[,1]==paste(row,"_vlab",sep="") & sing_value_label[,3]==k,]
    l[[k]]<- vls[,2]
  }
  rnames<-suppressWarnings(do.call(dplyr::bind_rows,l))
  #rnames <- rownames(cpercent)
  cpercent<-rbind(matrix('',1,ncol(cpercent)),cpercent)
  if (is.null(attr(dof[,1],"label"))){g=row} else {g=attr(dof[,1],"label")}
  rnames<- c(g,rnames)
  rownames(cpercent) <- rnames
  tabx<-data.table(cpercent,keep.rownames=TRUE)
  return(cpercent)
}

