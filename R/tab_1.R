#' Tabela de frequência para uma variável
#'
#' @param df Nome do seu DataFrame
#' @param coluna Nome da coluna que deseja fazer a estatística
#'
#' @return Retorna uma tabela com a frequência e a porcentagem
#' @export

tab_1 <- function(df, coluna){

  df |>
    dplyr::group_by({{coluna}}) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::mutate(`%` = round((n / nrow(df))*100, 1)) |>
    dplyr::arrange(-n) |>
    dplyr::bind_rows(
      df |>
        dplyr::summarize(
          {{coluna}} := "Total",
          n = dplyr::n()
        ) |>
        dplyr::mutate(`%` = round((n / nrow(df)) * 100))
    )

}
