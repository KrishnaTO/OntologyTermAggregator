#' Retrieve full entity data
#'
#' Get class and properties of an entity, using the BioOntology Class URI
#'
#'
#' @param link link to the BioOntology entity
#' @param format Choose which format to get the results in; default: JSON
#' @inheritParams yourAPIKey
#' @examples
#' \dontrun{
#' # rdfs <- lapply(res.json$links$self, getFullEntity)
#' }
#' @export
#'

getFullEntity <- function(link, format = "json"){
  if(format=="xml"){
    httpGET(paste(link, "?apikey=",yourAPIKey, "&include=all", "&format=xml", sep = "")) %>% xmlParse()
  } else(httpGET(paste(link, "?apikey=",yourAPIKey, "&include=all", sep = "")) %>% fromJSON())
}

