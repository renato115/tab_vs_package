
#' Checagen de valores missing
#'
#' @param data Nome do dataframe
#'
#' @return Retorna uma tabela com o nome da coluna e a porcentagem de valores vazios
#' @export
#'
check_missing <- function(data) {

  # Criação de dataframe expondo valores vazios por coluna - em linha
  missing_value_df <- data.frame(
    column_name = names(data),
    percent_missing = round(colSums(is.na(data)) * 100 / nrow(data), 3)
  )

  # Retorna apenas colunas com % de vazios maiores que 0%
  missing_value_df <- missing_value_df[missing_value_df$percent_missing > 0, ]
  missing_value_df <- missing_value_df[order(-missing_value_df$percent_missing), ]

  # Redefine o índice
  rownames(missing_value_df) <- NULL

  return(missing_value_df)
}
