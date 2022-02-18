### Importing data function
#' Importing data from Kobo tool box (even Humanitarian Account)
#'
#' @param servername either 'https://kobo.humanitarianresponse.info/' or 'kc.kobotoolbox.org'
#' @param formid  Click the form and copy forms/"formid"/landing
#' @param username your username
#' @param password you password
#'
#' @return
#' @export
#'
#' @examples
#' data<-koboimport(servername,form_id,username,password)
#'
  koboimport<- function (servername,formid, username,password) {
    request<-c() ; data_cto<-c() ## Removing the chances of previous data
    urls<-paste("https://kf.",servername,"/assets/",formid,"/submissions/?format=json",sep='')
    print(urls)
    rawdata<-httr::GET(urls,jsonlite::authenticate(username,password))
    d_content <- rawToChar(rawdata$content)
    d_content <- jsonlite::fromJSON(d_content)
    data_cto <-suppressWarnings(suppressMessages(as.data.frame(d_content)))
    print(paste(nrow(data_cto),"of",formid, "have been downloaded.",sep=' '))
    df_new <- data_cto %>% dplyr:: select(-contains("]/")) # Dropping repeat group data.
    df_names<-gsub('/_','_',gsub("(\\d+)$", "_\\1",gsub('-','',names(df_new)))) ### Removing group names
    df_namesd<- sub('.*/','', df_names) ### Further cleaning of column names
    colnames(df_new)<-df_namesd
    ###Formatting the data
    df_new[] <- lapply(df_new, gsub, pattern='n/a', replacement='NA')
    df_new[] <- lapply(df_new, gsub, pattern='True', replacement='1')
    df_new[] <- lapply(df_new, gsub, pattern='False', replacement='0')
    return(df_new)
  }


  #data<- koboimport('kobotoolbox.org','a595eJvpxXHATXpJqR8GdM','lytons_analytica','Nyakingei#52#')
