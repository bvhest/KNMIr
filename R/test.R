#
# test script to get data from new KNMI Open Data portal
#
# 2021-02-26
#

library(tidyverse)
library(jsonlite)
library(httr)

connectServer <- Sys.getenv("CONNECT_SERVER")
API.key <- Sys.getenv("KNMI_OPEN_DATA_API_KEY")

API.key <-
 "eyJvcmciOiI1ZTU1NGUxOTI3NGE5NjAwMDEyYTNlYjEiLCJpZCI6IjEyOTVjNDgzYzNkNDRlMGJiMzgxNWYwZjNmNmEzY2E0IiwiaCI6Im11cm11cjEyOCJ9"



# GET results in issue:  The certificate chain was issued by an authority that is not trusted.
# possible solution: https://github.com/dleutnant/influxdbr/issues/36
httr::set_config(httr::config(ssl_verifypeer = FALSE))
#  httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE)

# this solves the issue with the certificate, but results in a code=500 response error.

# define (and show) user-agent:
API.user_agent <-
  httr::user_agent("http://github.com/hadley/httr")


# file per jaar-maand combinatie t/m de laatste volledige maand... (bummer!!!)
path <-
  "https://api.dataplatform.knmi.nl/open-data/datasets/weer_en_luchtdruk/versions/1.0/files"


# add context using query-parameters
query_params <- list(maxKeys = 1000)

status_code <- 0
response <- NULL


api.result <-
  httr::GET(url = path,
            query = query_params,
            API.user_agent,
            add_headers(Authorization = API.key),
            httr::verbose())

(status_code <- httr::status_code(api.result))

content <- httr::content(api.result, as = "text")

if (!content == "") {
  content <- jsonlite::fromJSON(httr::content(api.result,
                                              as = "text"),
                                simplifyVector = FALSE)
}


# transform list into S3 object
response <-
  structure(
    list(
      content = content,
      path = path,
      files = content$files
    ),
    class = "KNMI_OpenData"
  )

save(content, response, file="test_20210226.RData")

# response <-
#   content$files[["filename"]]
#
# content[["files"]]
# content$files
#
# str(content$files)
#
# unlist(content)
#
# unname(unlist(content))

#---------------
# DIT WERKT !!!
#---------------
res <-
  unlist(content)
res2 <-
  res[ grepl("filename", names(res)) ]




