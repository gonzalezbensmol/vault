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
#' Vault: Gets the secret id for the approle registered in vault e.g.(jenkins).
#'
#' This function obtains the secret id for the approle from Vault. The user can then utilize the secret id retrieved to obtain the approle token from Vault. 
#' You will need the URL of the Hashicorp Vault you are using the role name in vault and a token from an auth path in Vault. 
#'
#' @param url URL of the Hashicorp Vault instance.
#' @param role_name name of the role in your Vault instance.
#' @param token token from your user/github/etc... login method
#' @keywords get_secret_id
#' @return Return's the secret id that allows an individual to query secrets in Vault via the approle method. 
#' @name get_secret_id
#' @title get_secret_id
#' @import httr
#' @import jsonlite
#' 
#' @examples \dontrun{  get_secret_id(url='vautl.url',role_name='jenkins',token='12345abcdef')
#'
#' }
#'
#' @export

get_secret_id <- function(url,role_name,token){
  ###url of the Hashicorp Vault instance
  url <- url
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
