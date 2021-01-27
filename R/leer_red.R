#' Función de Lectrura de Red en ShapeFile
#'
#'Esta función lee un archivo ShapeFile con la estructura sf necesario para los siguientes análisis
#'
#' @param ruta_red Es la ruta del archivo ShapeFile que corresponde a la red víal.
#'
#' @return retorna un objeto Simple Featrures tipo `LINESTRING`
#' @export
#'
#' @examples
leer_red <- function(ruta_red) {
  red <-  sf::st_read(dsn = ruta_red) %>%
    sf::st_cast(to = "LINESTRING")
  return(red)
}

