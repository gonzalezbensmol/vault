#get_token
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
#' This function gets secrets or data from the vault instance and returns them inside a dataframe.
#' You will need the Hashicorp Vault url you are using including the full path to the secret e.g. (https://url.vault.com/v1/secret/things)
#' and lastly you will need a token that allows you to interact with Vault via the API. You can pass a list of secrets at once use 'list(one="two",three="four) and so on and so forth. This will allow you to send several secrets to Vault at once.
#'
#' @param url url of the Hashicorp Vault instance.
#' @param user username registered in Vault.
#' @param pass password for username registered in Vault
#' @aliases data
#' @keywords get_token
#' @return Return's the user token that allows an individual to query results in Vault. 
#' @name get_token
#' @title get_token
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{  get_token(url,user,pass)
#'
#' }
#'
#' @export

get_token <- function(url,user,pass){
  ###url of the Hashicorp Vault instance
  url <- 'http://rgonzovault.us'
  ###Path to the Hashicorp Vault User Verification Path
  path <- user
  ###Secrets to be written to Vault.
  secrets <- list(username=user,password=pass)
  data_to_insert<- rjson::toJSON(secrets)
  ###Pastes the url and path and creates the path through /v1/secret/
  complete_url<- paste0(url,':8200/v1/auth/userpass/login/',path)
  ###Puts the data into the Hashicorp Vault path.
  res <- httr::POST(complete_url, body = data_to_insert, encode = "json",verbose())
  ###If the status returned is 204 return the following message else return an error message
  results<- rjson::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  return(results$auth)
}

