#post_vault_data
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
#' Vault: Posts data to the path you specify in Vault.
#'
#' This function writes secrets or data to the vault instance and gives a message if they were written correctly.
#' You will need the Hashicorp Vault url you are using including the full path to the secret e.g. (https://url.vault.com/v1/secret/things)
#' and lastly you will need a token that allows you to interact with Vault via the API. You can pass a list of secrets at once use 'list(one="two",three="four) and so on and so forth.
#' This will allow you to send several secrets to Vault at once. Remember to not overwrite secrets you will want to write new secrets with the old secrets as placeholders.
#'
#' @param url URL of the Hashicorp Vault instance.
#' @param token token for the vault instance.
#' @param path path to the secret in the vault instance.
#' @param secrets secrets that are being written to vault.
#' @keywords post_vault_data
#' @return Allows a user to write data to Vault. 
#' @name post_vault_data
#' @title post_vault_data
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{  post_vault_data(url,path,token,token,secrets=list(one="two",three="four"))
#'
#' }
#'
#' @export


post_vault_data <- function(url=NULL,path=NULL,token=NULL,secrets=NULL){
  
  ###url of the Hashicorp Vault instance
  url <- url
  ###Token from the Hashicorp Vault user
  token <- apptoken
  ###Path to the Hashicorp Vault secrets
  path <- path
  ###Secrets to be written to Vault.
  data_to_insert<- jsonlite::toJSON(secrets)
  ###Pastes the url and path and creates the path through /v1/secret/
  complete_url<- paste0(url,':8200/v1/secret/',path)
  body <- jsonlite::toJSON(data_to_insert)
  ###Puts the data into the Hashicorp Vault path.
  # browser()
  res <- httr::PUT(complete_url,httr::add_headers('X-Vault-Token' = token), body = data_to_insert, encode = "json",httr::verbose())
                    
  ###If the status returned is 204 return the following message else return an error message
  if(res$status_code==204){
    message(paste0("Data successfully written to ",path))
  }else{
    message(paste0("Error saving data to ",path))
  }
  return(res)
}
