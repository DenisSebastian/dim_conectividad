#' Crear un grafo con estructura Tidygraph
#'
#' Esta funión crea un grafo con la estructura y formato de la librería
#' [tidygraph](https://cran.r-project.org/web/packages/tidygraph/index.html)
#' desde un objeto Simple Features que correspnde a la Red Vial
#'
#' @param x Es la Red Vial en fomato `sf` (Simple Features), tipo `LINESTRING`
#' @param directed TRUE si la red es dirigida. Default es `TRUE`
#'
#' @return Grapfo cuya estructura contiene dos secciones una los `nodes` y
#' otra los `edges` pudiendo posteriormente acceder a elos de smanera
#' independiente para su manipulación y análisis
#' @export
#' @example

red2grafo <- function(x, directed = TRUE) {
  edges <- x %>%
    dplyr::mutate(edgeID = c(1:n()))
  nodes <- edges %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::rename(edgeID = L1) %>%
    dplyr::group_by(edgeID) %>%
    slice(c(1, n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
    dplyr::mutate(xy = paste(.$X, .$Y)) %>%
    dplyr::mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
    dplyr::select(-xy)
  source_nodes <- nodes %>%
    dplyr::filter(start_end == 'start') %>%
    dplyr::pull(nodeID)
  target_nodes <- nodes %>%
    dplyr::filter(start_end == 'end') %>%
    dplyr::pull(nodeID)
  edges = edges %>%
    dplyr::mutate(from = source_nodes, to = target_nodes)
  nodes <- nodes %>%
    dplyr::distinct(nodeID, .keep_all = TRUE) %>%
    dplyr::select(-c(edgeID, start_end)) %>%
    sf::st_as_sf(coords = c('X', 'Y')) %>%
    sf::st_set_crs(st_crs(edges))

  grafo <- tidygraph::tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
  return(grafo)
}
