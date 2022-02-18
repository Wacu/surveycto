
    ### Importing data function

#' Importing Data from SurveyCTO server
#'
#' @param servername  SurveyCTO servername
#' @param formid      Get this from Setting sheet in the XLSForm workbook
#' @param username    SurveyCTO username
#' @param password    SurveyCTO password

#'
#' @return
#' @export
#'
#' @examples
#' ctoimport('servername','formid','username','password')
    ctoimport<- function (servername,formid, username,password) {

      request<-c() ; data_cto<-c() ## Removing the chances of previous data

      request <- httr::GET(paste("https://",servername, ".surveycto.com/api/v1/forms/data/csv/",formid,sep=''),
                           jsonlite::authenticate(username,password))

      data_cto <- read.csv (text = httr::content(request, "text"))

      names<- sub('.*\\.', '', sub('.*\\:', '', names(data_cto)))

      colnames(data_cto)<-names
      return(data_cto)
    }


