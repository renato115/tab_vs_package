#' Tabela para análise de colunas no SINAN violências
#'
#' @param df Nome do dataframe
#' @param list Escolha da lista que será utilizada como categoria, ex: viol, enc, rel
#' @param col Valores nas colunas, ex: ds_raca, faixa_etaria_padrao
#' @param pct_reg Selecione se quer porcentagem por registro
#' @param pct_mul Selecione se quer porcentagem por número de mulheres
#'
#' @return Retorna tabela com as variáveis escolhidas na linha cruzada pela variável escolhida para a coluna
#' @export


tab_cat_sinan<- function(df,list, col, pct_reg = FALSE, pct_mul = FALSE){
  # tibble vazia
  tab_tipo_viol <- tibble()
  # Transformando a coluna das categorias em lista
  nomes_violencias <- list |>
    filter(categoria!='Total') |>
    select(categoria) |>
    pull()
  # Criando coluna de 1
  df$n_ob <- 1
  for (i in 1:length(nomes_violencias)){
    nm <- nomes_violencias[i]
    f <- as.formula(paste0(nomes_violencias[i], " ~ ", col))
    tab_enforq <- dcast(
      df |>
        filter(banco == 'SINAN'),
      f,
      value.var='n_ob',
      sum
    )  |>
      adorn_totals("col")
    # deixando a lista baseado nas categorias anteriores
    names(tab_enforq) <-
      c(paste0('tipo_',deparse(substitute(list))),
        colnames(tab_enforq |> as.data.frame())[-1])
    # Criando o nome da coluna dinâmica
    col_name <- paste0('tipo_', deparse(substitute(list)))
    tab_enforq <- tab_enforq |>
      filter(get(col_name) == 1)  |>
      mutate({{col_name}} := nomes_violencias[i])
    #Juntando os títulos com os dados
    tab_tipo_viol <- rbind(tab_tipo_viol,tab_enforq)
  }
  tab_tipo_viol <- tab_tipo_viol |> as.data.frame()
  mapeamento <- unique(list[, c("categoria", colnames(list)[2])])
  # Substituindo os valores em tab_tipo_viol
  tab_tipo_viol[[col_name]] <- mapeamento[[colnames(list)[2]]][match(tab_tipo_viol[[col_name]], mapeamento$categoria)]
  tab_tipo_viol <- tab_tipo_viol |> arrange(-Total)
  # Seleção das categorias
  l<-list |>
    filter(categoria!='Total') |>
    select(categoria)
  # Linha de quem não tem registros
  b_pivotado<-
    df %>%
    filter(banco == 'SINAN') %>%
    mutate_all(~ ifelse(is.na(.), 0, .)) %>%
    filter_at(
      vars(l$categoria),
      all_vars(. != 1)
    ) %>%
    group_by(get(col)) |>
    summarise(n = n()) |>
    adorn_totals('row') |>
    pivot_wider(
      names_from = `get(col)`,
      values_from = n,
      values_fill = 0
    ) |>
    mutate({{col_name}} := case_when(
      col_name == 'tipo_viol' ~ 'Nenhuma violência registrada',
      col_name == 'tipo_enc' ~ 'Nenhum encaminhamento',
      col_name == 'tipo_proc' ~ 'Nenhum procedimento',
      col_name == 'tipo_rel' ~ 'Nenhum tipo de relacionamento informado',
      TRUE ~ col_name
    )) |>
    as.data.frame()
  tab_tipo_viol <- bind_rows(tab_tipo_viol, b_pivotado)
  #### Linha de registros ####
  regis <- df %>%
    filter(banco == 'SINAN') %>%
    group_by(get(col)) |>
    summarise(n = n()) |>
    adorn_totals('row') |>
    pivot_wider(
      names_from = `get(col)`,
      values_from = n,
      values_fill = 0
    ) |>
    mutate(
      {{col_name}} := "Número total de registros"
    ) |>
    as.data.frame()
  tab_tipo_viol<-bind_rows(tab_tipo_viol, regis)
  tab_tipo_viol[is.na(tab_tipo_viol)] <- 0
  if (pct_reg) {
    tab_tipo_viol_a <- tab_tipo_viol |> as.data.frame()
    # Supondo que '{{col_name}}' seja uma coluna no seu dataframe
    numerador <- tab_tipo_viol_a %>%
      filter(get(col_name) != 'Número total de registros') %>%
      select(-{{col_name}})
    denominador <- tab_tipo_viol_a %>%
      filter(get(col_name) == 'Número total de registros') %>%
      select(-{{col_name}})
    # Replicando a linha do denominador para ter o mesmo número de linhas que o numerador
    denominador_replicado <- suppressWarnings(
      do.call("rbind",
              replicate(
                nrow(numerador),
                denominador,
                simplify = FALSE)
      )
    )
    # Dividindo todas as linhas do numerador pelo denominador replicado
    result <-  round(numerador / denominador_replicado * 100, 1)
    # Adicionando a coluna '{{col_name}}' de volta ao resultado
    result <- cbind(
      tab_tipo_viol_a %>%
        filter(get(col_name) != 'Número total de registros') %>%
        select({{col_name}}),
      result)
    # Convertendo o resultado para um dataframe
    tab_tipo_viol <- as.data.frame(result)
  }
  if (pct_mul) {
    mulheres <- df %>%
      filter(banco == 'SINAN') %>%
      distinct(
        par_f,
        ds_raca,
        faixa_etaria_padrao
      ) |>
      group_by(get(col)) |>
      summarise(n = n()) |>
      adorn_totals('row') |>
      pivot_wider(
        names_from = `get(col)`,
        values_from = n,
        values_fill = 0
      ) |>
      mutate(
        {{col_name}} := "Número de mulheres"
      ) |>
      as.data.frame()
    tab_tipo_viol<- tab_tipo_viol |> as.data.frame()
    numerador <- tab_tipo_viol %>%
      filter(get(col_name) != 'Número total de registros')
    tab_tipo_viol<-bind_rows(tab_tipo_viol, mulheres)
    tab_tipo_viol_a <- tab_tipo_viol |> as.data.frame()
    # Supondo que '{{col_name}}' seja uma coluna no seu dataframe
    numerador <- tab_tipo_viol_a %>%
      filter(get(col_name) != 'Número de mulheres') %>%
      select(-{{col_name}})
    denominador <- tab_tipo_viol_a %>%
      filter(get(col_name) == 'Número de mulheres') %>%
      select(-{{col_name}})
    # Replicando a linha do denominador para ter o mesmo número de linhas que o numerador
    denominador_replicado <- suppressWarnings(
      do.call("rbind",
              replicate(
                nrow(numerador),
                denominador,
                simplify = FALSE)
      )
    )
    # Dividindo todas as linhas do numerador pelo denominador replicado
    result <-  round(numerador / denominador_replicado * 100, 1)
    # Adicionando a coluna '{{col_name}}' de volta ao resultado
    result <- cbind(
      tab_tipo_viol_a %>%
        filter(get(col_name) != 'Número de mulheres') %>%
        select({{col_name}}),
      result)
    # Convertendo o resultado para um dataframe
    tab_tipo_viol <- as.data.frame(result)
    tab_tipo_viol <- tab_tipo_viol |>
      filter(get(col_name) != 'Número total de registros')
  }
  return(tab_tipo_viol)
}
