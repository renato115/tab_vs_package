

#' Tabela cruzada de frequência para duas variáveis
#'
#' @param df Nome do DataFrame
#' @param var_row Variável que deseja ver como linha
#' @param var_col Variáveis que virarão colunas
#' @param pct Retorna frequência ou porcentagem. Por default, a tabela retorna a contagem das categorias, caso use
#' @param pct_row Retorna a frequência ou porcentagem com o total pela linha
#' pct = TRUE, a tabela retornará como porcentagem contabilizando 100% por coluna
#'
#'
#' @return Retorna uma tabela cruzando duas variáveis distintas, pode ser número absoluto (pct=FALSE), ou a porcentagem
#' pela coluna (pct=TRUE)
#' @export
#'
tab_2 <- function(df, var_row, var_col, pct = FALSE, pct_row = FALSE){

  df <- df |>
    dplyr::group_by(
      {{var_row}},
      {{var_col}}
    ) |>
    dplyr::summarise(
      contagem = dplyr::n(),
      .groups = 'drop'
    ) |>
    tidyr::pivot_wider(
      names_from = {{var_col}},
      values_from = c(contagem),
      values_fill = 0
    ) |>
    janitor::adorn_totals("col") |>
    dplyr::arrange(-Total)

  if (pct) {
    df <- df |>
      dplyr::filter(
        {{var_row}} != 'Total'
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          ~round((. / sum(.)) * 100, 1)
        )
      )
  }

  if (pct_row) {
    df <- df |>
      dplyr::filter(
        {{var_row}} != 'Total'
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          ~round((. / Total) * 100, 1)
        )
      )
  }

  df <- df |>
    janitor::adorn_totals("row", name = "Total")

  return(as.data.frame(df))
}

