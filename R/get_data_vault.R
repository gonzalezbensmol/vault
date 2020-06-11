# get_data_vault
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
#' and lastly you will need a token that allows you to interact with Vault via the API.
#'
#' @param url url of the Hashicorp Vault instance.
#' @param token token for the vault instance.
#' @param path path to the secret in the vault instance.
#' @keywords get_vault_data
#' @return Return's the data or secrets that are in the vault instance.
#' @name get_vault_data
#' @title get_vault_data
#' @import httr
#' @import jsonlite
#' @examples
#'
#' \dontrun{  get_vault_data(url,path,token)
#'
#' }
#'
#' @export


get_vault_data <- function(url=NULL,path=NULL,token=NULL){

  ###url of the Hashicorp Vault instance
  url <- url
  ###Token from the Hashicorp Vault user
  token <- token
  ###Path to the Hashicorp Vault secrets
  path <- path
  ###Pastes the url and path and creates the path through /v1/secret/
  complete_url<- paste0(url,'/v1/secret/',path)

  ###Gets the data from the Hashicorp Vault path
  res<- httr::GET(complete_url, httr::add_headers('X-Vault-Token' = token))
  ###Gets the data from the JSON format
  res<- jsonlite::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  ###Puts the data into a data frame
  # if(dataframe=="Y"){
  # res<- as.data.frame(res$data)
  # }else if(dataframe=="N"){
  # res
  # }

  return(res)
}






