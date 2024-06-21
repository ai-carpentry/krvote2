#' 투표결과 조회
#'
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sdName 시도명 (옵션)
#' @param wiwName 구시군명 (옵션)
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 출력 개수 (기본값: 10, 최대값: 100)
#'
#' @return 투표결과 데이터프레임
#' @export
#'
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 read_xml xml_find_first xml_text
#'
#' @examples
#' get_vote_info(sgId = "20220309", sgTypecode = "1")
get_vote_info <- function(sgId, sgTypecode, sdName = NULL, wiwName = NULL,
                          pageNo = 1, numOfRows = 10) {

  serviceKey <- get_krvote2_key()
  base_url <- "http://apis.data.go.kr/9760000/VoteXmntckInfoInqireService2/getVoteSttusInfoInqire"

  query_params <- list(
    serviceKey = serviceKey,
    pageNo = pageNo,
    numOfRows = numOfRows,
    resultType = "json",
    sgId = sgId,
    sgTypecode = sgTypecode
  )

  if (!is.null(sdName)) query_params$sdName <- sdName
  if (!is.null(wiwName)) query_params$wiwName <- wiwName

  response <- GET(url = base_url, query = query_params)

  if (status_code(response) != 200) {
    stop("API request failed with status code: ", status_code(response))
  }

  content <- content(response, as = "text", encoding = "UTF-8")

  tryCatch({
    if (grepl("^<", trimws(content))) {
      # XML response
      parsed <- read_xml(content)
      error_node <- xml_find_first(parsed, "//cmmMsgHeader")
      if (!is.na(error_node)) {
        error_msg <- xml_text(xml_find_first(error_node, ".//errMsg"))
        stop("API returned an error: ", error_msg)
      }
      stop("Unexpected XML response")
    } else {
      # JSON response
      parsed <- fromJSON(content)

      if (!is.null(parsed$OpenAPI_ServiceResponse$cmmMsgHeader$returnAuthMsg)) {
        stop("API returned an error: ", parsed$OpenAPI_ServiceResponse$cmmMsgHeader$returnAuthMsg)
      }

      if (parsed$response$header$resultCode != "INFO-000" &&
          parsed$response$header$resultMsg != "NORMAL SERVICE") {
        stop("API returned an unexpected response: ", parsed$response$header$resultMsg)
      }

      df <- parsed$response$body$items$item

      if (is.null(df) || nrow(df) == 0) {
        warning("No data returned from the API.")
        return(data.frame())
      }

      return(df)
    }
  }, error = function(e) {
    stop("Failed to parse API response: ", e$message)
  })
}

#' 개표결과 조회
#'
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sggName 선거구명 (옵션)
#' @param sdName 시도명 (옵션)
#' @param wiwName 구시군명 (옵션)
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 출력 개수 (기본값: 10, 최대값: 100)
#'
#' @return 개표결과 데이터프레임
#' @export
#'
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 read_xml xml_find_first xml_text
#'
#' @examples
#' get_count_info(sgId = "20220309", sgTypecode = "1")
get_count_info <- function(sgId, sgTypecode, sggName = NULL, sdName = NULL, wiwName = NULL,
                           pageNo = 1, numOfRows = 10) {

  serviceKey <- get_krvote2_key()
  base_url <- "http://apis.data.go.kr/9760000/VoteXmntckInfoInqireService2/getXmntckSttusInfoInqire"

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
  if (!is.null(wiwName)) query_params$wiwName <- wiwName

  response <- GET(url = base_url, query = query_params)

  if (status_code(response) != 200) {
    stop("API request failed with status code: ", status_code(response))
  }

  content <- content(response, as = "text", encoding = "UTF-8")

  tryCatch({
    if (grepl("^<", trimws(content))) {
      # XML response
      parsed <- read_xml(content)
      error_node <- xml_find_first(parsed, "//cmmMsgHeader")
      if (!is.na(error_node)) {
        error_msg <- xml_text(xml_find_first(error_node, ".//errMsg"))
        stop("API returned an error: ", error_msg)
      }
      stop("Unexpected XML response")
    } else {
      # JSON response
      parsed <- fromJSON(content)

      if (!is.null(parsed$OpenAPI_ServiceResponse$cmmMsgHeader$returnAuthMsg)) {
        stop("API returned an error: ", parsed$OpenAPI_ServiceResponse$cmmMsgHeader$returnAuthMsg)
      }

      if (parsed$response$header$resultCode != "INFO-000" &&
          parsed$response$header$resultMsg != "NORMAL SERVICE") {
        stop("API returned an unexpected response: ", parsed$response$header$resultMsg)
      }

      df <- parsed$response$body$items$item

      if (is.null(df) || nrow(df) == 0) {
        warning("No data returned from the API.")
        return(data.frame())
      }

      return(df)
    }
  }, error = function(e) {
    stop("Failed to parse API response: ", e$message)
  })
}

#' 모든 투표결과 조회
#'
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sdName 시도명 (옵션)
#' @param wiwName 구시군명 (옵션)
#'
#' @return 모든 투표결과 데이터프레임
#' @export
#'
#' @examples
#' get_all_vote_info(sgId = "20220309", sgTypecode = "1")
get_all_vote_info <- function(sgId, sgTypecode, sdName = NULL, wiwName = NULL) {
  pageNo <- 1
  numOfRows <- 100
  allData <- data.frame()

  repeat {
    result <- get_vote_info(sgId, sgTypecode, sdName, wiwName,
                            pageNo = pageNo, numOfRows = numOfRows)

    if (nrow(result) == 0) break

    allData <- rbind(allData, result)

    if (nrow(result) < numOfRows) break

    pageNo <- pageNo + 1
  }

  return(allData)
}

#' 모든 개표결과 조회
#'
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sggName 선거구명 (옵션)
#' @param sdName 시도명 (옵션)
#' @param wiwName 구시군명 (옵션)
#'
#' @return 모든 개표결과 데이터프레임
#' @export
#'
#' @examples
#' get_all_count_info(sgId = "20220309", sgTypecode = "1")
get_all_count_info <- function(sgId, sgTypecode, sggName = NULL, sdName = NULL, wiwName = NULL) {
  pageNo <- 1
  numOfRows <- 100
  allData <- data.frame()

  repeat {
    result <- get_count_info(sgId, sgTypecode, sggName, sdName, wiwName,
                             pageNo = pageNo, numOfRows = numOfRows)

    if (nrow(result) == 0) break

    allData <- rbind(allData, result)

    if (nrow(result) < numOfRows) break

    pageNo <- pageNo + 1
  }

  return(allData)
}
