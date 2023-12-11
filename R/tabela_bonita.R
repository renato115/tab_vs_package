#' Padronizar design de tabela
#'
#' @param df Tabela que receberá o design da tabela
#' @param titulo Título da tabela
#' @param text_row Transforma os textos da linha com letras minúsculas
#'
#' @return Retorna o Dataframe no fundo de tabela estética
#' @export

tabela_bonita <- function(df, titulo, text_row = FALSE){

  if (text_row) {

    df[, 1] <- stringr::str_to_title(df[, 1])

  }

  df |>
    gt::gt() |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          align = "center",
          weight = 'bold',
        )
      ),
      locations = list(
        gt::cells_title(groups = c("title"))
      )) |>
    gt::tab_header(title = titulo)

}
