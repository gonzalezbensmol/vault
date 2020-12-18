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
#' Vault: Gets the approle token from Vault.
#'
#' This function gets the approle token from the vault instance. You can then utilize the approle token to query data from vault.
#' @param url URL of the Hashicorp Vault instance.
#' @param role_id role_id of the approle in Vault.
#' @param secret_id secret_id of the approle in Vault. 
#' @keywords get_approle_token
#' @return Return's the user token that allows an approle to query secrets in Vault. 
#' @name get_approle_token
#' @title get_approle_token
#' @import httr
#' @import jsonlite
#' 
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
  data_to_insert<- jsonlite::toJSON(secrets)
  ###Pastes the url and the approle login path
  complete_url<- paste0(url,':8200/v1/auth/approle/login')
  ###Posts the data for a return of the approle token to query data from Vault
  res <- httr::POST(complete_url, body = data_to_insert, encode = "json",httr::verbose())
  ###Get the client_token for querying data from Vault via the designated approle
  results<- jsonlite::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  return(results$auth$client_token)
}
