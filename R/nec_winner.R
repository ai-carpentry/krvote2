#' 당선인 정보 조회
#'
#' 선거ID, 선거종류코드, 시도명, 선거구명을 입력받아 당선인 관련 정보를 조회하는 함수
#'
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sdName 시도명 (옵션)
#' @param sggName 선거구명 (옵션)
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 목록 건수 (기본값: 10, 최대값: 100)
#' @param resultType 데이터포맷 (기본값: "json", 다른 옵션: "xml")
#'
#' @return 당선인 정보 데이터프레임
#' @export
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' get_winner_info(sgId = "20220309", sgTypecode = "1")
get_winner_info <- function(sgId, sgTypecode, sdName = NULL, sggName = NULL,
                            pageNo = 1, numOfRows = 10, resultType = "json") {
  serviceKey <- get_krvote2_key()
  base_url <- "http://apis.data.go.kr/9760000/WinnerInfoInqireService2/getWinnerInfoInqire"
  query_params <- list(
    serviceKey = serviceKey,
    pageNo = pageNo,
    numOfRows = numOfRows,
    resultType = resultType,
    sgId = sgId,
    sgTypecode = sgTypecode
  )
  if (!is.null(sdName)) query_params$sdName <- sdName
  if (!is.null(sggName)) query_params$sggName <- sggName

  response <- GET(url = base_url, query = query_params)

  if (status_code(response) != 200) {
    stop("API request failed with status code: ", status_code(response))
  }

  content <- content(response, as = "text", encoding = "UTF-8")

  if (resultType == "json") {
    parsed <- fromJSON(content)
    if (parsed$response$header$resultCode != "INFO-00") {
      stop("API returned an error: ", parsed$response$header$resultMsg)
    }
    df <- parsed$response$body$items$item
  } else if (resultType == "xml") {
    parsed <- xml2::read_xml(content)
    result_code <- xml2::xml_text(xml2::xml_find_first(parsed, "//resultCode"))
    if (result_code != "INFO-00") {
      result_msg <- xml2::xml_text(xml2::xml_find_first(parsed, "//resultMsg"))
      stop("API returned an error: ", result_msg)
    }
    items <- xml2::xml_find_all(parsed, "//item")
    df <- do.call(rbind, lapply(items, function(x) {
      data.frame(t(xml2::xml_text(xml2::xml_children(x))), stringsAsFactors = FALSE)
    }))
  } else {
    stop("Invalid resultType. Choose 'json' or 'xml'.")
  }

  if (is.null(df) || nrow(df) == 0) {
    warning("No data returned from the API.")
    return(data.frame())
  }

  return(df)
}

#' 전체 당선인 정보 조회
#'
#' 선거ID, 선거종류코드, 시도명, 선거구명을 입력받아 모든 페이지의 당선인 정보를 조회하는 함수
#'
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sdName 시도명 (옵션)
#' @param sggName 선거구명 (옵션)
#'
#' @return 전체 당선인 정보 데이터프레임
#' @export
#'
#' @examples
#' get_all_winner_info(sgId = "20220309", sgTypecode = "1")
get_all_winner_info <- function(sgId, sgTypecode, sdName = NULL, sggName = NULL) {
  pageNo <- 1
  numOfRows <- 100
  all_data <- data.frame()

  repeat {
    result <- get_winner_info(sgId, sgTypecode, sdName, sggName,
                              pageNo = pageNo, numOfRows = numOfRows)
    if (nrow(result) == 0) break
    all_data <- rbind(all_data, result)
    if (nrow(result) < numOfRows) break
    pageNo <- pageNo + 1
  }

  return(all_data)
}
