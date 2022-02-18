#' The function generate proportion from a multiple-select variable
#' @param row Multiple-select variable
#' @param column Group by variable
#' @param data  data with multiple select variable
#' @param valuelabel  The is a dataset conataining value labels from XLSFROM (generated using rodkmultlas )
#'
#' @return
#' @export
#'
#' @examples
#' tabmmult('row','column','data','valuelabel')
#'
tabmult<- function(row, column,data,valuelabel=NULL) {
  suppressMessages(library(dplyr))
    dof<- data %>%
      filter(!is.na({row})) %>%
      filter(!is.na({column}))%>%
      dplyr::select({row},{column})
    rr<-sort(as.numeric(unique(scan(text=dof[,1], what="", sep=" ",quiet =TRUE))))
    mat<- matrix(NA,length(rr),length(unique(na.omit(dof[,2])))+1)
    rownames(mat) <- rr
    dd<-c(unique(na.omit(dof[,2])),"Overall")
    row2col<-rownames(table(data[{column}]))
    colnames(mat) <- dd
    if (is.null(attr(dof[,1],"label"))){g=row} else {g=attr(dof[,1],"label")}
    for (i in dd) {
      for (j in rr) {
        mat[paste(j),paste(i)]<- paste(sum(grepl(paste(" ",j," ",sep=""),
                                                 paste(" ",dof[dof[,2]==i,][[row]]," ",sep=""))),"(",
                                       round((sum(grepl(paste(" ",j," ",sep=""),paste(" ",dof[dof[,2]==i,][[row]]," ",sep="")))/nrow(dof[dof[,2]==i & !is.na(dof[,2]),]))*100,1), "%)",sep = "")

        mat[paste(j),length(unique(na.omit(dof[,2])))+1]<-paste(sum(grepl(paste(" ",j," ",sep=""),
                                                                          paste(" ",dof[,1]," ",sep=""))),"(",round((sum(grepl(paste(" ",j," ",sep=""),
                                                                                                                               paste(" ",dof[,1]," ",sep="")))/length(is.na(dof[,1])))*100,1), "%)",sep = "")
      }
    }
    ##ADDING variable label if they exist

    ###Adding value llabel
    if (!is.null(valuelabel)) {
      vls<- valuelabel[valuelabel[,1]==paste(row,"_vlab",sep=""),]
      rownames(vls)<-vls[,"value"]
      mat<- merge(vls,mat,by="row.names",all.x=TRUE) %>% dplyr:: select(-c(Row.names,value)) %>%
        filter(!is.na(Overall)) %>%
        mutate(Label=name) %>%
        dplyr:: select(-c(name,variable))%>%
        dplyr::select(Label,everything())
      VV<-matrix(NA,1,length(unique(na.omit(dof[,2])))+2)
      rownames(VV)<-c("Label")
      colnames(VV)<- c("Label",dd)
      mat<-rbind(VV,mat)
      mat[1,"Label"]<- g
      rownames(mat)<- mat[,"Label"]
      mat <- mat %>% dplyr::select(-Label)
      colnames(mat)<-c(row2col,"Overall")
      return(as.data.frame(mat))
    } else {
      colnames(mat)<-row2col
      return(as.data.frame(mat))
    }
  }


