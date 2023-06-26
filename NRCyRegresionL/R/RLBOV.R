#' Regresión lineal para predecir pesos finales en bovinos de engorda.
#'
#' @param datos  (vector) Datos reales para entrenamiento.
#' @param pi (vector) Peso al iniciar la engorda.
#' @param ms (vector) Materia seca consumida.
#' @param em (vector) Energía metabolizable de la ración.
#' @param gdp (vector) Ganancia de peso diaria en kg.
#' @param pf (vector) Pesos finales al termino de la engorda.
#' @return Peso al finalizar la engorda.
#' @export
rlb <- function(pv, cms, em, gpd, pf, datos){
  pf <- datos[ ,pf]
  pi <- datos[ ,pv]
  ms <- datos[ ,cms]
  em <- datos[ ,em]
  gpd <- datos[ ,gpd]
  modelo <- lm(pf ~ pi + ms + em + gpd )
  return(modelo)
}
