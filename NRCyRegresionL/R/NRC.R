#' Cálculo de peso final mediante ecuaciones del NRC.
#' @param data  (vector) Datos reales para entrenamiento.
#' @param pv (vector) Peso al iniciar la engorda.
#' @param cms (vector) Materia seca consumida.
#' @param emta (vector) Energía metabolizable de la ración.
#' @param gdp (vector) Ganancia de peso diaria en kg.
#' @param x (vector) Dato establecido por NRC.
#' @param y (vector) Dato establecido por NRC.
#' @return Peso al finalizar la engorda por NRC.
#' @export
nrc <- function(pv, cms, em, edad, gp, x=0.0031, y=1, data){
  pv <- data[ ,pv]
  cms <- data[ ,cms]
  emta <- data[ ,em]
  edad <- data[ ,edad]
  gdpe <- data[ ,gp]
  model_nrc <- (pv *(exp(cms * emta * x))- y * ( - exp(-x * edad)) + gdpe)
  return(model_nrc)
}
