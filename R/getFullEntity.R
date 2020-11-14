#' Retrieve full entity data
#'
#' Get class and properties of an entity, using the BioOntology Class URI.
#' The results are also parsed from json or xml to provide readable results.
#'
#' Before using this function, be sure to add your BioOntology API key
#' using the yourAPIKey variable. See ?yourAPIKey for more instructions.
#'
#'
#' @param link link to the BioOntology entity
#' @param format Choose which format to get the results in; default is JSON, or XML
#' @param yourAPIKey manually include you API key for BioOntology here
#' @examples
#' \dontrun{
#' # getFullEntity("http://data.bioontology.org/ontologies/RCD/classes/http%3A%2F%2Fpurl.bioontology.org%2Fontology%2FRCD%2FX78Tv")
#' }
#' @export
#'

getFullEntity <- function(link,
                          format = "json",
                          yourAPIKey = yourAPIKey){
  if(format=="xml"){
    httpGET(paste(link, "?apikey=",yourAPIKey, "&include=all", "&format=xml", sep = "")) %>% xmlParse()
  } else(httpGET(paste(link, "?apikey=",yourAPIKey, "&include=all", sep = "")) %>% fromJSON())
}

