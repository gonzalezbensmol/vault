#get_approle_token
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
#' @param role_id role_id of the approle in Vault.
#' @param secret_id secret_id of the approle in Vault. 
#' @keywords get_approle_token
#' @return Return's the user token that allows an approle to query secrets in Vault. 
#' @name get_approle_token
#' @title get_approle_token
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{  get_approle_token(url,role_id,secret_id)
#'
#' }
#'
#' @export

get_approle_token <- function(url,role_id,secret_id){
  ###url of the Hashicorp Vault instance
  url <- url
  ###Secrets to be written to Vault.
  secrets <- list(role_id=role_id,secret_id=secret_id)
  data_to_insert<- rjson::toJSON(secrets)
  ###Pastes the url and the approle login path
  complete_url<- paste0(url,':8200/v1/auth/approle/login')
  ###Posts the data for a return of the approle token to query data from Vault
  res <- httr::POST(complete_url, body = data_to_insert, encode = "json",httr::verbose())
  ###Get the client_token for querying data from Vault via the designated approle
  results<- rjson::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  return(results$auth$client_token)
}