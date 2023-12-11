#' Tabela pra varias colunas com categorias iguais
#'
#' @param df Nome do DataFrame
#' @param lista Nome da lista com o nome das colunas
#' @param filtro Nome do banco que contém as colunas
#'
#' @return Dataframe em que as linhas são os nomes das colunas, as colunas são as frequências de categorias = 1 no banco
#' @export

tab_multi <- function(df, lista, filtro){

  lista <- as_vector(lista)

  df |>
    dplyr::filter(banco %in% c(filtro)) |>
    dplyr::select(
      par_f,
      lista
    ) |>
    tidyr::pivot_longer(
      cols = c(lista),
      names_to = "colunas",
      values_to = "value"
    ) |>
    dplyr::group_by(
      colunas,
      value
    ) |>
    dplyr::summarise(
      contagem = dplyr::n(),
      .groups = 'drop'
    ) |>
    tidyr::pivot_wider(
      names_from = colunas,
      values_from = contagem,
      values_fill = 0
    ) |>
    as.data.frame() |>
    # trecho novo
    dplyr::filter(
      value == 1
    ) |>
    dplyr::mutate(
      value = ifelse(
        value == 1,
        "Sim",
        "Nao"
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      -value,
      names_to = "categoria",
      values_to = "n"
    ) |>
    dplyr::select(
      categoria,
      n
    ) |>
    dplyr::mutate(pct = round((n/sum(n)*100),1)) |>
    dplyr::arrange(-n) |>
    janitor::adorn_totals('row') |>
    as.data.frame()
}
