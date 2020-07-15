#' Adiciona e remove um elemento por vez
#'
#' @param vet vetor binário.
#' @return um data frame com o conjunto de combinações possíveis.
#' @examples
#' modelos_possiveis(c(1,1,0))
#'
modelos_possiveis <- function(vet){


  qnt_var <- length(vet)
  tds_modelos <- bincombinations(qnt_var) # ate 22 variáveis

  conte <- matrix(0,nrow(tds_modelos), ncol(tds_modelos))
  for (i in 1:nrow(tds_modelos)) {
    for (j in 1:ncol(tds_modelos)) {
      if((vet[j]==1 & tds_modelos[i,j]==0) || (vet[j]==0 & tds_modelos[i,j]==1)){
        conte[i,j] <- 1
      }
    }
  }

  ver <- as.data.frame(tds_modelos) %>%
    filter(apply(conte,1,sum)==1)
  return(ver)
}






