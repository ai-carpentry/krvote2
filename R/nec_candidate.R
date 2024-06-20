#' 예비후보자 정보 조회
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sggName 선거구명 (옵션)
#' @param sdName 시도명 (옵션)
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 출력 개수 (기본값: 10, 최대값: 100)
#'
#' @return 예비후보자 정보 데이터프레임
#' @export
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' get_preliminary_candidate_info(serviceKey = "your_service_key", sgId = "20220309", sgTypecode = "1")
get_preliminary_candidate_info <- function(serviceKey, sgId, sgTypecode, sggName = NULL, sdName = NULL,
                                           pageNo = 1, numOfRows = 10) {

  base_url <- "http://apis.data.go.kr/9760000/PofelcddInfoInqireService/getPoelpcddRegistSttusInfoInqire"

  query_params <- list(
    serviceKey = serviceKey,
    pageNo = pageNo,
    numOfRows = numOfRows,
    resultType = "json",
    sgId = sgId,
    sgTypecode = sgTypecode
  )

  if (!is.null(sggName)) query_params$sggName <- sggName
  if (!is.null(sdName)) query_params$sdName <- sdName

  response <- GET(url = base_url, query = query_params)

  if (status_code(response) != 200) {
    stop("API request failed with status code: ", status_code(response))
  }

  content <- content(response, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(content)

  if (parsed$response$header$resultCode != "INFO-00") {
    stop("API returned an error: ", parsed$response$header$resultMsg)
  }

  df <- parsed$response$body$items$item

  if (is.null(df) || nrow(df) == 0) {
    warning("No data returned from the API.")
    return(data.frame())
  }

  return(df)
}

#' 후보자 정보 조회
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sggName 선거구명 (옵션)
#' @param sdName 시도명 (옵션)
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 출력 개수 (기본값: 10, 최대값: 100)
#'
#' @return 후보자 정보 데이터프레임
#' @export
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' get_candidate_info(serviceKey = "your_service_key", sgId = "20220309", sgTypecode = "1")
get_candidate_info <- function(serviceKey, sgId, sgTypecode, sggName = NULL, sdName = NULL,
                               pageNo = 1, numOfRows = 10) {

  base_url <- "http://apis.data.go.kr/9760000/PofelcddInfoInqireService/getPofelcddRegistSttusInfoInqire"

  query_params <- list(
    serviceKey = serviceKey,
    pageNo = pageNo,
    numOfRows = numOfRows,
    resultType = "json",
    sgId = sgId,
    sgTypecode = sgTypecode
  )

  if (!is.null(sggName)) query_params$sggName <- sggName
  if (!is.null(sdName)) query_params$sdName <- sdName

  response <- GET(url = base_url, query = query_params)

  if (status_code(response) != 200) {
    stop("API request failed with status code: ", status_code(response))
  }

  content <- content(response, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(content)

  if (parsed$response$header$resultCode != "INFO-00") {
    stop("API returned an error: ", parsed$response$header$resultMsg)
  }

  df <- parsed$response$body$items$item

  if (is.null(df) || nrow(df) == 0) {
    warning("No data returned from the API.")
    return(data.frame())
  }

  return(df)
}

#' 모든 예비후보자 정보 조회
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sggName 선거구명 (옵션)
#' @param sdName 시도명 (옵션)
#'
#' @return 모든 예비후보자 정보 데이터프레임
#' @export
#'
#' @examples
#' get_all_preliminary_candidate_info(serviceKey = "your_service_key", sgId = "20220309", sgTypecode = "1")
get_all_preliminary_candidate_info <- function(serviceKey, sgId, sgTypecode, sggName = NULL, sdName = NULL) {
  pageNo <- 1
  numOfRows <- 100
  allData <- data.frame()

  repeat {
    result <- get_preliminary_candidate_info(serviceKey, sgId, sgTypecode, sggName, sdName,
                                             pageNo = pageNo, numOfRows = numOfRows)

    if (nrow(result) == 0) break

    allData <- rbind(allData, result)

    if (nrow(result) < numOfRows) break

    pageNo <- pageNo + 1
  }

  return(allData)
}

#' 모든 후보자 정보 조회
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sggName 선거구명 (옵션)
#' @param sdName 시도명 (옵션)
#'
#' @return 모든 후보자 정보 데이터프레임
#' @export
#'
#' @examples
#' get_all_candidate_info(serviceKey = "your_service_key", sgId = "20220309", sgTypecode = "1")
get_all_candidate_info <- function(serviceKey, sgId, sgTypecode, sggName = NULL, sdName = NULL) {
  pageNo <- 1
  numOfRows <- 100
  allData <- data.frame()

  repeat {
    result <- get_candidate_info(serviceKey, sgId, sgTypecode, sggName, sdName,
                                 pageNo = pageNo, numOfRows = numOfRows)

    if (nrow(result) == 0) break

    allData <- rbind(allData, result)

    if (nrow(result) < numOfRows) break

    pageNo <- pageNo + 1
  }

  return(allData)
}
