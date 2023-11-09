library(tidyverse)
library(magrittr)
library(urltools)

# build a url for the query
mpdbapi_geturl <- function(scheme,
    domain,
    path,
    index,
    query,
    fields,
    size = 0,
    from = 0
  ) {
  url <- ''
  scheme(url) <- scheme
  domain(url) <- domain
  path(url) <- path
  
  url %<>%
    param_set('query', query) %>%
    param_set('fields', fields) %>%
    param_set('index', index)
  
  if (size != 0)
  url %<>%
    param_set('size', size)
  if (from != 0)
  url %<>%
    param_set('from', from)
  
  url
}

# return results of a single url query
mpdbapi_query <- function(url) {
  doc <- jsonlite::fromJSON(url)
  as_tibble(doc$hits$hits) %>%
    unnest('_source') %>%
    select(!'_index': 'uid')
}

# count hits for a query
mpdbapi_count <- function(
    query = '', 
    fields = '', 
    index = 'published_item',
    scheme = 'https',
    domain = 'musikverlage.slub-dresden.de',
    path = 'api/search'
    ) {
  url <- mpdbapi_geturl(scheme, domain, path, index, query, fields)
  'querying' %>% paste(url) %>% print
  doc <- jsonlite::fromJSON(url)
  
  doc$hits$total$value
}

# perform a query stepwise for larger result sets
mpdbapi_search <- function(
    query = '',
    fields = '',
    index = 'published_item',
    scheme = 'https',
    domain = 'musikverlage.slub-dresden.de',
    path = 'api/search',
    size = 0,
    from = 0,
    step = 100
  ) {
  # init
  url <- mpdbapi_geturl(scheme, domain, path, index, query, fields, step, from)
  pointer <- from + step
  counter <- 1
  
  # count all records if size is not predefined
  if (size == 0) {
    'counting records' %>% print
    size <- mpdbapi_count(query, fields, index, scheme, domain, path)
    size %>% print
  }
  steps <- ceiling(size / step)
  
  # execute initial query
  paste('step', counter, 'of', steps) %>% print
  'querying' %>% paste(url) %>% print
  result <- url %>% mpdbapi_query
  
  # execute subsequent steps
  while (pointer < size) {   
    counter <- counter + 1
    paste('step', counter, 'of', steps) %>% print   
    url <- mpdbapi_geturl(scheme, domain, path, index, query, fields, step, pointer)
    'querying' %>% paste(url) %>% print
    result %<>% rbind(url %>% mpdbapi_query)
    pointer <- pointer + step
  }
  
  result
}
