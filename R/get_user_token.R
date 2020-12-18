#get_user_token
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
#' This function gets the user token from Vault for the user name registered in your Vault instance registered at the auth/userpass path. 
#'
#' @param url URL of the Hashicorp Vault instance.
#' @param user username registered in Vault.
#' @param pass password for username registered in Vault
#' @keywords get_token
#' @return Return's the user token that allows an individual to query secrets in Vault. 
#' @name get_user_token
#' @title get_user_token
#' @import httr
#' @import jsonlite
#' @examples
#'
#' \dontrun{  get_user_token(url,user,pass)
#'
#' }
#'
#' @export

get_user_token <- function(url,user,pass){
  ###url of the Hashicorp Vault instance
  url <- 'www.rgonzovault.us'
  ###Path to the Hashicorp Vault User Verification Path
  user <- 'ben'
  ###Secrets to be written to Vault.
  secrets <- list(username=user,password='Charlene81#$')
  data_to_insert<- jsonlite::toJSON(secrets)
  ###Pastes the url and path and creates the path through /v1/secret/
  complete_url<- paste0(url,':8200/v1/auth/userpass/login/',user)
  ###Puts the data into the Hashicorp Vault path.
  res <- httr::PUT(complete_url, body = data_to_insert, encode = "json",httr::verbose())
  ###If the status returned is 204 return the following message else return an error message
  results<- jsonlite::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  return(results$auth)
}

