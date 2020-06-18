#get_secret_id
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
#' This function obtains the token for the github user from Vault. The user can then utilize the token retrieved to query secrets from Vault. 
#' You will need the url of the Hashicorp Vault you are using and your PAT from github. You should have already created an organization in github
#' to utilize this login path in Vault. 
#'
#' @param url url of the Hashicorp Vault instance.
#' @param role_name name of the role in your Vault instance.
#' @param token token from your user/github/etc... login method
#' @keywords get_secret_id
#' @return Return's the secret id that allows an individual to query secrets in Vault via the approle method. 
#' @name get_secret_id
#' @title get_secret_id
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{  get_secret_id(url='vautl.url',role_name='jenkins',token='12345abcdef)
#'
#' }
#'
#' @export

get_secret_id <- function(url,role_name,token){
  ###url of the Hashicorp Vault instance
  url <- 'rgonzovault.us'
  ###Role Name to get approle ID
  role_name <- role_name
  ###Token from user
  token <- token
  ###Pastes the url and github login path together
  complete_url<- paste0(url,sprintf(':8200/v1/auth/approle/role/%s/secret-id',role_name))
  ###Posts the data to Vault to retrieve the user token
  ###Gets the data from the Hashicorp Vault path
  res<- httr::POST(complete_url, httr::add_headers('X-Vault-Token' = token))
  ###Gets the data from the JSON format
  res<- jsonlite::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  return(res$data$secret_id)

}
