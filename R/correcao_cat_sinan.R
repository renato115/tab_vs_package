#' Tabela para análise de colunas no SINAN violências
#'
#' @param df Nome do dataframe
#'
#' @return Retorna o dataframe com as variáveis do SINAN padronizadas. Usar antes de aplicar a função `tab_cat_sinan`
#' @export

corrige_cat_sinan <- function(df){
  df<-
    df |>
    mutate(
      rede_enc_sau = case_when((rede_sau=="1" | enc_saude=="1")~1,
                               T~0),
      assit_soc_creas = case_when((assist_soc=="1" |enc_creas=="1")~1,
                                  T~0), # abrigo
      atend_enc_mulh = case_when((atend_mulh=="1" | enc_mulher=="1")~1,
                                 T~0),
      cons_enc_tutela = case_when((cons_tutel=="1" | enc_tutela=="1")~1,
                                  T~0),
      mpu_enc_mpu = case_when((mpu=="1" | enc_mpu=="1")~1,
                              T~0),
      deleg_enc_cria = case_when((deleg_cria=="1"|enc_dpca=="1")~1,
                                 T~0),
      deleg_enc_mulh = case_when((deleg_mulh=="1"| enc_deam=="1")~1,
                                 T~0),
      deleg_enc_deleg = case_when((deleg=="1"|enc_deleg=="1")~1,
                                  T~0),
      infan_enc_juv = case_when((infan_juv=="1"|enc_vara=="1")~1,
                                T~0)
    )
  return(df)
}
