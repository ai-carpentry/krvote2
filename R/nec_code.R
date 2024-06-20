#' 선거코드 조회
#'
#' 선거코드, 선거명, 선거종류코드, 선거일자 데이터를 조회하는 함수
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#'
#' @return 선거코드 데이터프레임
#' @export
#'
#' @examples
#' getCommonSgCodeList(serviceKey = "your_service_key")
getCommonSgCodeList <- function(serviceKey, pageNo = 1, numOfRows = 10) {
 url <- "http://apis.data.go.kr/9760000/CommonCodeService/getCommonSgCodeList"
 query_params <- list(
  serviceKey = serviceKey,
  pageNo = pageNo,
  numOfRows = numOfRows,
  resultType = "json"
 )

 response <- GET(url, query = query_params)

 if (response$status_code == 200) {
  json_data <- content(response, "text", encoding = "UTF-8")
  result_list <- fromJSON(json_data)

  if (result_list$response$header$resultCode == "INFO-00") {
   result_df <- as.data.frame(result_list$response$body$items$item)
   return(result_df)
  } else {
   warning("API request failed with message: ", result_list$response$header$resultMsg)
   return(data.frame())
  }
 } else {
  warning("API request failed with status code: ", response$status_code)
  return(data.frame())
 }
}

#' 구시군코드 조회
#'
#' 선거ID를 입력하여 구시군명, 순서, 상위시도명 데이터를 조회하는 함수
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#' @param sdName 시도명 (기본값: NULL)
#'
#' @return 구시군코드 데이터프레임
#' @export
#'
#' @examples
#' getCommonGusigunCodeList(serviceKey = "your_service_key", sgId = "20220309")
getCommonGusigunCodeList <- function(serviceKey, pageNo = 1, numOfRows = 10, sgId, sdName = NULL) {
 url <- "http://apis.data.go.kr/9760000/CommonCodeService/getCommonGusigunCodeList"
 query_params <- list(
  serviceKey = serviceKey,
  pageNo = pageNo,
  numOfRows = numOfRows,
  resultType = "json",
  sgId = sgId
 )

 if (!is.null(sdName)) {
  query_params$sdName <- sdName
 }

 response <- GET(url, query = query_params)

 if (response$status_code == 200) {
  json_data <- content(response, "text", encoding = "UTF-8")
  result_list <- fromJSON(json_data)

  if (result_list$response$header$resultCode == "INFO-00") {
   result_df <- as.data.frame(result_list$response$body$items$item)
   return(result_df)
  } else {
   warning("API request failed with message: ", result_list$response$header$resultMsg)
   return(data.frame())
  }
 } else {
  warning("API request failed with status code: ", response$status_code)
  return(data.frame())
 }
}

#' 선거구코드 조회
#'
#' 선거ID와 선거종류코드를 입력하여 선거구명, 시도명, 구시군명, 선출정수, 순서 데이터를 조회하는 함수
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#'
#' @return 선거구코드 데이터프레임
#' @export
#'
#' @examples
#' getCommonSggCodeList(serviceKey = "your_service_key", sgId = "20220309", sgTypecode = "1")
getCommonSggCodeList <- function(serviceKey, pageNo = 1, numOfRows = 10, sgId, sgTypecode) {
 url <- "http://apis.data.go.kr/9760000/CommonCodeService/getCommonSggCodeList"
 query_params <- list(
  serviceKey = serviceKey,
  pageNo = pageNo,
  numOfRows = numOfRows,
  resultType = "json",
  sgId = sgId,
  sgTypecode = sgTypecode
 )

 response <- GET(url, query = query_params)

 if (response$status_code == 200) {
  json_data <- content(response, "text", encoding = "UTF-8")
  result_list <- fromJSON(json_data)

  if (result_list$response$header$resultCode == "INFO-00") {
   result_df <- as.data.frame(result_list$response$body$items$item)
   return(result_df)
  } else {
   warning("API request failed with message: ", result_list$response$header$resultMsg)
   return(data.frame())
  }
 } else {
  warning("API request failed with status code: ", response$status_code)
  return(data.frame())
 }
}

#' 정당코드 조회
#'
#' 선거ID를 입력하여 정당명, 순서 데이터를 조회하는 함수
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#'
#' @return 정당코드 데이터프레임
#' @export
#'
#' @examples
#' getCommonPartyCodeList(serviceKey = "your_service_key", sgId = "20220309")
getCommonPartyCodeList <- function(serviceKey, pageNo = 1, numOfRows = 10, sgId) {
 url <- "http://apis.data.go.kr/9760000/CommonCodeService/getCommonPartyCodeList"
 query_params <- list(
  serviceKey = serviceKey,
  pageNo = pageNo,
  numOfRows = numOfRows,
  resultType = "json",
  sgId = sgId
 )

 response <- GET(url, query = query_params)

 if (response$status_code == 200) {
  json_data <- content(response, "text", encoding = "UTF-8")
  result_list <- fromJSON(json_data)

  if (result_list$response$header$resultCode == "INFO-00") {
   result_df <- as.data.frame(result_list$response$body$items$item)
   return(result_df)
  } else {
   warning("API request failed with message: ", result_list$response$header$resultMsg)
   return(data.frame())
  }
 } else {
  warning("API request failed with status code: ", response$status_code)
  return(data.frame())
 }
}

#' 직업코드 조회
#'
#' 선거ID를 입력하여 직업코드, 직업명, 순서 데이터를 조회하는 함수
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#'
#' @return 직업코드 데이터프레임
#' @export
#'
#' @examples
#' getCommonJobCodeList(serviceKey = "your_service_key", sgId = "20220309")
getCommonJobCodeList <- function(serviceKey, pageNo = 1, numOfRows = 10, sgId) {
 url <- "http://apis.data.go.kr/9760000/CommonCodeService/getCommonJobCodeList"
 query_params <- list(
  serviceKey = serviceKey,
  pageNo = pageNo,
  numOfRows = numOfRows,
  resultType = "json",
  sgId = sgId
 )

 response <- GET(url, query = query_params)

 if (response$status_code == 200) {
  json_data <- content(response, "text", encoding = "UTF-8")
  result_list <- fromJSON(json_data)

  if (result_list$response$header$resultCode == "INFO-00") {
   result_df <- as.data.frame(result_list$response$body$items$item)
   return(result_df)
  } else {
   warning("API request failed with message: ", result_list$response$header$resultMsg)
   return(data.frame())
  }
 } else {
  warning("API request failed with status code: ", response$status_code)
  return(data.frame())
 }
}

#' 학력코드 조회
#'
#' 선거ID를 입력하여 학력코드, 학력명, 순서 데이터를 조회하는 함수
#'
#' @param serviceKey 공공데이터포털에서 발급받은 서비스키
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#'
#' @return 학력코드 데이터프레임
#' @export
#'
#' @examples
#' getCommonEduBckgrdCodeList(serviceKey = "your_service_key", sgId = "20220309")
getCommonEduBckgrdCodeList <- function(serviceKey, pageNo = 1, numOfRows = 10, sgId) {
 url <- "http://apis.data.go.kr/9760000/CommonCodeService/getCommonEduBckgrdCodeList"
 query_params <- list(
  serviceKey = serviceKey,
  pageNo = pageNo,
  numOfRows = numOfRows,
  resultType = "json",
  sgId = sgId
 )

 response <- GET(url, query = query_params)

 if (response$status_code == 200) {
  json_data <- content(response, "text", encoding = "UTF-8")
  result_list <- fromJSON(json_data)

  if (result_list$response$header$resultCode == "INFO-00") {
   result_df <- as.data.frame(result_list$response$body$items$item)
   return(result_df)
  } else {
   warning("API request failed with message: ", result_list$response$header$resultMsg)
   return(data.frame())
  }
 } else {
  warning("API request failed with status code: ", response$status_code)
  return(data.frame())
 }
}
