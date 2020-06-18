#get_github_token
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
#' get_github_token: Gets a token from Vault utilizing your github login credentials. 
#'
#' This function obtains the token for the github user from Vault. The user can then utilize the token retrieved to query secrets from Vault. 
#' You will need the url of the Hashicorp Vault you are using and your PAT from github. You should have already created an organization in github
#' to utilize this login path in Vault. 
#'
#' @param url url of the Hashicorp Vault instance.
#' @param github_token personal authentication token or PAT from your github repo.
#' @keywords get_github_token
#' @return Return's the user token that allows an individual to query secrets in Vault. 
#' @name get_github_token
#' @title get_github_token
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{ get_github_token(url='vault.url',github_token='12345abcdef')
#'
#' }
#'
#' @export

get_github_token <- function(url,github_token){
  ###url of the Hashicorp Vault instance
  url <- url
  ###Secrets to be written to Vault.
  token <- list(token=github_token)
  token_data<- rjson::toJSON(token)
  ###Pastes the url and github login path together
  complete_url<- paste0(url,':8200/v1/auth/github/login')
  ###Posts the data to Vault to retrieve the user token
  res <- httr::POST(complete_url, body = token_data, encode = "json",verbose())
  ###Converts data from UTF-8 to json
  results<- rjson::fromJSON(httr::content(x = res,type = "text",encoding = "UTF-8"))
  return(results$auth$client_token)
}

