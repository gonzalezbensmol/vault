#get_role_id
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
#' Vault: Gets the Approle ID from Vault.
#'
#' This function obtains the approle ID from Vault. The user can then utilize the approle ID retrieved to query secrets from Vault via the approle method. 
#' You will need the URL of the Hashicorp Vault you are using and your user/github/etc... token to retrieve the approle id.
#'
#' @param url URL of the Hashicorp Vault instance.
#' @param role_name name of the role in your Vault instance.
#' @param token token from your loging role such as user/pass method or github/auth method. 
#' @keywords get_role_id
#' @return Return's the user/github/etc... token that allows an individual to query secrets in Vault via an approle. 
#' @name get_role_id
#' @title get_role_id
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{  get_role_id(url='vautl.url',role_name='jenkins',token='12345abcdef')
#'
#' }
#'
#' @export

get_role_id <- function(url,role_name,token){
  ###url of the Hashicorp Vault instance
  url <- url
  ###Role Name to get approle ID
  role_name <- role_name
  ###Token from user/github/etc... login credentials
  token <- token
  ###Pastes the URL and github login path together
  complete_url<- paste0(url,sprintf(':8200/v1/auth/approle/role/%s/role-id',role_name))
  ###Posts the data to Vault to retrieve the user token
  httr::GET(complete_url)
  ###Gets the data from the Hashicorp Vault path
  res<- httr::GET(complete_url, httr::add_headers('X-Vault-Token' = token))
  ###Gets the data from the JSON format
  res<- jsonlite::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  return(res$data$role_id)
}

