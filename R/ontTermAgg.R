#' Get all results of term
#'
#' Higher order function to essentially run the same code from the vignette as a function
#'
#' @param searchTerm Enter the term you want the combined data for
#' @param yourAPIKey manually include you API key for BioOntology here
#' @param format Choose which format to get the results in; default is JSON, or XML
#'
#'
#' @export



ontTermAgg <-  function(searchTerm, yourAPIKey, format = "json"){

  # Retrieved function cat_lists: https://stackoverflow.com/a/57171812
  cat_lists <- function(list1, list2) {

    keys <- unique(c(names(list1), names(list2)))
    map2(list1[keys], list2[keys], c) %>%
      set_names(keys)

  }
res.search <- postForm('http://data.bioontology.org/search',
                       q = searchTerm,
                       also_search_obsolete=FALSE ,
                       inlcude="all",
                       pagesize=1000,
                       apikey = yourAPIKey
)

res.json <- fromJSON(res.search)$collection %>%
  filter(ontologyType == "ONTOLOGY") %>%
  select(-obsolete, -ontologyType, -provisional, -`@context`, -cui, -semanticType)
res.json$links$`@context` <- NULL

if(searchTerm == "Neurofibroma" & file.exists("../data/Searchtoclasses_json.RData")){
  load("../data/Searchtoclasses_json.RData")
} else{
  rdfs <- lapply(res.json$links$self, getFullEntity)
}


combined_output <- reduce(rdfs, cat_lists)
combined_output.parents <- reduce(combined_output$parents, cat_lists)
combined_output$parents <- NULL

cols <- c("subClassOf", "definition", "synonym", "cui", "semanticType", "label")
combined_output[cols] <- lapply(combined_output[cols], listUnique)
combined_output.parents[cols] <- lapply(combined_output.parents[cols], listunique)

complex_cols <- c("properties", "links", "@context")
combined_output.complex <- combined_output[complex_cols]
combined_output[complex_cols] <- NULL

combined_output <- lapply(combined_output, unique)
combined_output$id <- unique(combined_output$id)
combined_output$prefLabel <- unique(combined_output$prefLabel)


lapply(combined_output, function(x) write.table(data.frame(x), "combined_output.csv", append= T, sep=',' ))
saveRDS(combined_output.complex, file = "combined_output.complex.RData")

}
