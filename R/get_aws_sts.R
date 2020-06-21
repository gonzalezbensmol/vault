#get_aws_sts
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#' Vault: Gets the AWS STS credentials from Vault. 
#'
#' This function gets AWS STS credentials from the registered role in your Vault instance. 
#'
#' @param url url of the Hashicorp Vault instance.
#' @param token token from user/github/approle/etc.... registered in Vault.
#' @param role_name role_name for role registered in Vault
#' @keywords get_aws_creds
#' @return Return's the user token that allows an individual to query secrets in Vault. 
#' @name get_aws_sts
#' @title get_aws_sts
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{  get_aws_sts(url,token,role_name)
#'
#' }
#'
#' @export


get_aws_sts <- function(url=NULL,token=NULL,role_name=NULL){
  ###url of Vault instance
  url <- url
  ###Token from Vault user/github/approle/etc...
  token <- token
  ####Paste together Vault url using the sprintf() function
  complete_url <- sprintf('%s:8200/v1/aws/sts/%s',url,role_name)
  ###Gets the data from the Hashicorp Vault path
  res<- httr::GET(complete_url, httr::add_headers('X-Vault-Token' = token))
  results<- rjson::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  print(results)
  
}



