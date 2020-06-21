#config_aws_root_creds
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
#' Vault: Registers the AWS User credentials in Vault. Note: Please do not register the ROOT User tokens from your AWS instance. 
#'
#' This function registers the AWS User credentials in Vault used to get the Access Key ID and Secret Access Key for Users and roles in AWS. 
#'
#' @param url URL of the Hashicorp Vault instance.
#' @param token token from user/github/approle/etc.... registered in Vault.
#' @param aws_key AWS Access Key ID from IAM User registered in AWS.
#' @param aws_secret AWS Secret Access Key from IAM User registered in AWS.
#' @param aws_region AWS Region where IAM User is registered in 
#' @note  Examples of role policy and trust policy required in AWS:
#''\preformatted{Example Trust Policy required in AWS: 
#''{
#''  "Version": "2012-10-17",
#''  "Statement": [
#''    {
#''      "Effect": "Allow",
#''      "Principal": {
#''        "AWS": "arn:aws:iam::ACCOUNT-ID-WITHOUT-HYPHENS:user/VAULT-AWS-ROOT-CONFIG-USER-NAME"
#''      },
#''      "Action": "sts:AssumeRole"
#''    }
#''  ]
#''}
#''}
#' 
#''\preformatted{Example IAM User Policy required in AWS: 
#''{
#''"Version": "2012-10-17",
#''"Statement": {
#''  "Effect": "Allow",
#''  "Action": "sts:AssumeRole",
#''  "Resource": "arn:aws:iam::ACCOUNT-ID-WITHOUT-HYPHENS:role/RoleNameToAssume"
#''}
#''}
#''}
#' 
#' @references \url{https://www.vaultproject.io/docs/secrets/aws}
#' @keywords config_aws_root_creds
#' @return writes the AWS Accesss Key ID, AWS Secret Access Key, Region used by the registered IAM User in AWS to Vault. 
#' @name config_aws_root_creds
#' @title config_aws_root_creds
#' @import httr
#' @import jsonlite
#' @import rjson
#' @examples
#'
#' \dontrun{  config_aws_root_creds(url,token,aws_key,aws_secret,aws_region)
#'
#' }
#'
#' @export

config_aws_root_creds <- function(url=NULL,token=NULL,aws_key=NULL,aws_secret=NULL,aws_region=NULL){
  
  ###url of the Hashicorp Vault instance
  url <- url
  ###Token from the Hashicorp Vault user
  token <- token
  ###AWS Access Key ID from IAM User in AWS
  aws_key=aws_key
  ###AWS secret access key from IAM User in AWS
  aws_secret=aws_secret
  ###AWS region where your IAM User is registered in AWS
  aws_region=aws_region
  ###Create list of AWS secrets
  aws_list <- list(access_key=key,secret_key=secret,region=aws_region)
  ###Write AWS secrets to Json list
  data_to_insert<- jsonlite::toJSON(aws_list)
  ###Pastes the url and path and creates the path through /v1/secret/
  complete_url<- paste0(url,':8200/v1/aws/config/root')
  ###Puts the data into the Hashicorp Vault path.
  res <- httr::PUT(complete_url,httr::add_headers('X-Vault-Token' = token), body = data_to_insert, encode = "json",httr::verbose())
  
  ###If the status returned is 204 return the following message else return an error message
  if(res$status_code==204){
    message(paste0("AWS Secrets successfully written to account: ",path))
  }else{
    message(paste0("Error with AWS Secrets for account: ",path))
  }
  return(res)
}