#' Dropa colunas baseado no número de missing
#'
#' @param df Nome do seu DataFrame
#' @param cutpoint ponto de corte. Selecione um ponto de corte (entre 0 e 1) para deletas as colunas acima desse ponto
#'
#' @return Retorna uma tabela com a frequência e a porcentagem
#' @export
# Função para remover colunas com valores ausentes
drop_missing <- function(df, cutpoint) {
  # Cria um df frame expondo valores vazios por coluna - em linha
  missing_value_df <- df.frame(
    column_name = names(df),
    percent_missing = round(colSums(is.na(df)) / nrow(df), 3)
  )

  # Executa a remoção das colunas vazias baseadas no objeto 'cutpoint'
  df <- df[, !(missing_value_df$percent_missing > cutpoint), drop = FALSE]

  return(df)
}
