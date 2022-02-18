  #### Tabulation of multi selct variable in r

#' Calculates the mean by group
#'
#' @param row Numeric variable
#' @param column Group variable
#' @param data data where with row and column variables
#'
#' @return
#' @export
#'
#' @examples
#'  odkmean('row','column','data')
 odkmean<- function(row, column,data) {

      df<- data %>%
      filter(!is.na({row})) %>%
      filter(!is.na({column}))%>%
      dplyr::select({row},{column})

      row2col<-rownames(table(data[{column}]))
      dd<-c(unique(na.omit(df[,2])),"Overall")
      mat<- matrix(NA,2,length(dd))

      if (is.null(attr(df[,1],"label"))){g=row} else {g=attr(df[,1],"label") }

      rownames(mat)<-c(g,"Mean(Min,Max)")
      colnames(mat) <- dd

      for (i in dd) {
        mat[2,paste(i)]<- paste(round(suppressWarnings(mean(df[df[,2]==i,][[row]],na.rm=TRUE)),1),"(",
                                round(suppressWarnings(min(df[df[,2]==i,][[row]],na.rm=TRUE)),1), ",",
                                round(suppressWarnings(max(df[df[,2]==i,][[row]],na.rm=TRUE)),1),")",sep="")
      }

      mat[2,length(dd)]<- paste(round(suppressWarnings(mean(df[,1],na.rm=TRUE)),1),"(",
      round(suppressWarnings(min(df[,1],na.rm=TRUE)),1), ",",
      round(suppressWarnings(max(df[,1],na.rm=TRUE)),1),")",sep="")

      colnames(mat)<- c(row2col,"Overall")
      return(mat)
  }
