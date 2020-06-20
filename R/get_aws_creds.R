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
#' Vault: Gets the Vault Data
#'
#' This function gets AWS user creds via Vault to be used to connect to AWS services such as s3 buckets, ec2 instances, etc... 
#'
#' @param url url of the Hashicorp Vault instance.
#' @param role role to get secret key and secret access
#' @param token token to query Vault
#' @keywords get_token
#' @return Return's the user token that allows an individual to query secrets in Vault. 
#' @name get_aws_creds
#' @title get_aws_creds
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{  get_aws_creds(url,role,token)
#'
#' }
#'
#' @export


get_aws_creds <- function(url=NULL,role=NULL,token=NULL){
  ###url of the Hashicorp Vault instance
  url <- 'rgonzovault.us'
  ###Token
  token <- token
  ###Role to get security credentials in AWS
  role <- 'ben'
  ###Pastes the url and path and creates the path through /v1/secret/
  complete_url<- sprintf('%s:8200/v1/aws/roles/%s',url,role)
  ###Puts the data into the Hashicorp Vault path.
  ###Gets the data from the Hashicorp Vault path
  res<- httr::GET(complete_url, httr::add_headers('X-Vault-Token' = token))
  ###If the status returned is 204 return the following message else return an error message
  results<- rjson::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  return(results)
}