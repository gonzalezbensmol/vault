#list_aws_roles
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
#' Vault: Lists the AWS roles registered in Vault.
#'
#' This function lists the AWS roles that are registered in Vault. 
#'
#' @param url URL of the Hashicorp Vault instance.
#' @param token token from user/github/approle/etc.... registered in Vault.
#' @keywords list_aws_roles
#' @return Return's the AWS roles that are registered in Vault. 
#' @name list_aws_roles
#' @title list_aws_roles
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{  list_aws_roles(url,token)
#'
#' }
#'
#' @export


list_aws_roles <- function(url=NULL,token=NULL){
  ###url of Vault instance
  url <- url
  ###Token from Vault user/github/approle/etc...
  token <- token
  ####Paste together Vault url using the sprintf() function
  complete_url <- sprintf('%s:8200/v1/aws/roles',url)
  ###Gets the list of AWS roles registered in Vault
  res<- httr::VERB(verb = 'LIST',complete_url, httr::add_headers('X-Vault-Token' = token))
  results<- rjson::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  print(results$data$keys)
  
}

