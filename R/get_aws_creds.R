#get_aws_creds
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
#' Vault: Gets AWS creds from Vault.
#'
#' This function gets the AWS creds from the role/IAM User registered in Vault. 
#'
#' @param url URL of the Hashicorp Vault instance.
#' @param token token from user/github/approle/etc.... registered in Vault.
#' @param role_name role_name for role registered in Vault
#' @keywords get_aws_creds
#' @return Return's the user token that allows an individual to query secrets in Vault. 
#' @name get_aws_creds
#' @title get_aws_creds
#' @import httr
#' @import jsonlite
#' @examples
#'
#' \dontrun{  get_aws_creds(url,token,role_name)
#'
#' }
#'
#' @export


get_aws_creds <- function(url=NULL,token=NULL,role_name=NULL){
  ###url of Vault instance
  url <- url
  ###Token from Vault user/github/approle/etc...
  token <- token
  ####Paste together Vault url using the sprintf() function
  complete_url <- sprintf('%s:8200/v1/aws/creds/%s',url,role_name)
  ###Gets the data from the Hashicorp Vault path
  res<- httr::GET(complete_url, httr::add_headers('X-Vault-Token' = token))
  results<- jsonlite::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  print(results)
  
}



