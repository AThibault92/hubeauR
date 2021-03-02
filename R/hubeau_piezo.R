#' Téléchargement des chronique piézomètriques
#'
#' @param id ancien code BSS
#'
#' @return dataframe
#' @export
#' @importFrom glue glue
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom lubridate ymd
#' @importFrom utils URLencode
#'
#' @examples
chronique_piezo = function(id = "07548X0009/F"){

  # if (is.na(start) & is.na(end)){
    url_api_bss = glue::glue('https://hubeau.eaufrance.fr/api/v1/niveaux_nappes/chroniques?code_bss={id}&size=20000')
  # } else if (!is.na(start) & is.na(end)) {
  #   url_api_bss = glue::glue('https://hubeau.eaufrance.fr/api/v1/niveaux_nappes/chroniques?code_bss={id}&date_debut_mesure={start}&size=20000')
  # } else if (is.na(start) & !is.na(end)) {
  #   url_api_bss = glue::glue('https://hubeau.eaufrance.fr/api/v1/niveaux_nappes/chroniques?code_bss={id}&date_fin_mesure={end}&size=20000')
  # } else {
  #   url_api_bss = glue::glue('https://hubeau.eaufrance.fr/api/v1/niveaux_nappes/chroniques?code_bss={id}&date_debut_mesure={start}&date_fin_mesure={end}&size=20000')
  # }

  get_qualite_nappes_by_bss = httr::GET(url = url_api_bss)
  get_qualite_nappe_bss_json <- jsonlite::fromJSON((httr::content(get_qualite_nappes_by_bss, "text", encoding= "UTF-8")), flatten = TRUE)

  # Faire une fonction qui vient faire différentes requetes si le count est sup a 20 000
  if (get_qualite_nappe_bss_json$count > 0 & get_qualite_nappe_bss_json$count < 20000){
    return(tibble::as_tibble(get_qualite_nappe_bss_json$data))
  }
  if(get_qualite_nappe_bss_json$count >= 20000){


    date_debut_prelevement <- unique(max(lubridate::ymd(get_qualite_nappe_bss_json$data$date_debut_prelevement)))
    date_debut_prelevement_1 <- date_debut_prelevement+1

    url_to_use_bss_date2 = URLencode(paste0(url_api_bss,id,"&date_debut_prelevement=",date_debut_prelevement_1,"&size=20000"))
    get_qualite_nappes_by_bss_date2 = httr::GET(url = url_to_use_bss_date2)
    get_qualite_nappe_bss_json_date2 <- jsonlite::fromJSON((httr::content(get_qualite_nappes_by_bss_date2, "text", encoding= "UTF-8")), flatten = TRUE)
    get_qualite_nappe_bss_json$data <- rbind(get_qualite_nappe_bss_json$data, get_qualite_nappe_bss_json_date2$data)

    while(get_qualite_nappe_bss_json_date2$count >= 20000){

      date_debut_prelevement <- unique(max(lubridate::ymd(get_qualite_nappe_bss_json_date2$data$date_debut_prelevement)))
      date_debut_prelevement_1 <- date_debut_prelevement+1

      url_to_use_bss_date2 = URLencode(paste0(url_api_bss,id,"&date_debut_prelevement=",date_debut_prelevement_1,"&size=20000"))
      get_qualite_nappes_by_bss_date2 = httr::GET(url = url_to_use_bss_date2)
      get_qualite_nappe_bss_json_date2 <- jsonlite::fromJSON((httr::content(get_qualite_nappes_by_bss_date2, "text", encoding= "UTF-8")), flatten = TRUE)
      get_qualite_nappe_bss_json$data <- rbind(get_qualite_nappe_bss_json$data, get_qualite_nappe_bss_json_date2$data)

    }


    return(tibble::as_tibble(get_qualite_nappe_bss_json$data))
  }
  else(
    return(NA)
  )
}
