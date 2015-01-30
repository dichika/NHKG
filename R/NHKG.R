login <- function(login_id, password){
  url <- "https://hh.pid.nhk.or.jp/pidh02/login.do"
  headers <- list(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "ja,en-US;q=0.8,en;q=0.6",
    "Cache-Control"="max-age=0",
    "Connection" = "keep-alive",
    "Content-Type" =  "application/x-www-form-urlencoded",
    "Host"="hh.pid.nhk.or.jp",
    "Origin" =  "https://hh.pid.nhk.or.jp",
    "Referer" = "https://hh.pid.nhk.or.jp/pidh02/loginform.do",
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/40.0.2214.93 Safari/537.36"
  )
  body <- list(
    "href"="https://cgi2.nhk.or.jp/gogaku/",
    "authMemberType"="regular",
    "AUTO_LOGIN_FLAG"="0",
    "LOGIN_DEVICE"="pc",
    "n"="1",
    "LOGIN_ID"=login_id, 
    "PASSWORD"=password
  )
  a <- httr::POST(url, headers=headers, body=body, encode="form")
  cookie <- paste(sep="; ", 
                  paste0("mygogaku2013-cookie=", a$cookies$`mygogaku2013-cookie`),
                  paste0("MEM=", a$cookies$MEM),
                  paste0("CBSESSIONIDPID=", a$cookies$CBSESSIONIDPID)
  )
  if(is.null(cookie)){stop("login failed")}
  return(cookie)  
}

#' @export
getSinchokuGogaku <- function(login_id, password){
  require("httr")
  require("rvest")
  cookie <- login(login_id, password)
  response <- GET("https://cgi2.nhk.or.jp/gogaku/mygogaku/calendar/", 
                  config(cookie = cookie)
  )
  dat_string <- content(response)
  count <- html(dat_string) %>% html_nodes("div.count-area div span.count-big") %>% html_text()
  count <- gsub("回", "", count) %>% as.numeric
  return(count)
}