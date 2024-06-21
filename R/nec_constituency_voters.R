#' 선거구별 선거인수 정보 조회
#'
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#' @param sdName 시도명 (옵션)
#' @param wiwName 구시군명 (옵션)
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 출력 개수 (기본값: 10, 최대값: 100)
#'
#' @return 선거구별 선거인수 정보 데이터프레임
#' @export
#'
#' @examples
#' get_constituency_voters(sgId = "20200415", sgTypecode = "2", sdName = "서울특별시")
get_constituency_voters <- function(sgId, sgTypecode, sdName = NULL, wiwName = NULL, pageNo = 1, numOfRows = 10) {
  endpoint <- "getElpcElcntInfoInqire"
  query <- list(
    serviceKey = get_krvote2_key(),
    pageNo = pageNo,
    numOfRows = numOfRows,
    resultType = "json",
    sgId = sgId,
    sgTypecode = sgTypecode
  )
  if (!is.null(sdName)) query$sdName <- sdName
  if (!is.null(wiwName)) query$wiwName <- wiwName

  res <- call_krvote2_api(endpoint, query)
  return(res)
}

#' 읍면동별 선거인수 정보 조회
#'
#' @param sgId 선거ID
#' @param sdName 시도명
#' @param wiwName 구시군명
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 출력 개수 (기본값: 10, 최대값: 100)
#'
#' @return 읍면동별 선거인수 정보 데이터프레임
#' @export
#'
#' @examples
#' get_emd_voters(sgId = "20200415", sdName = "서울특별시", wiwName = "종로구")
get_emd_voters <- function(sgId, sdName, wiwName, pageNo = 1, numOfRows = 10) {
  endpoint <- "getEmdElcntInfoInqire"
  query <- list(
    serviceKey = get_krvote2_key(),
    pageNo = pageNo,
    numOfRows = numOfRows,
    resultType = "json",
    sgId = sgId,
    sdName = sdName,
    wiwName = wiwName
  )

  res <- call_krvote2_api(endpoint, query)
  return(res)
}

#' 투표구별 선거인수 정보 조회
#'
#' @param sgId 선거ID
#' @param sdName 시도명
#' @param wiwName 구시군명
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 출력 개수 (기본값: 10, 최대값: 100)
#'
#' @return 투표구별 선거인수 정보 데이터프레임
#' @export
#'
#' @examples
#' get_voting_district_voters(sgId = "20200415", sdName = "서울특별시", wiwName = "종로구")
get_voting_district_voters <- function(sgId, sdName, wiwName, pageNo = 1, numOfRows = 10) {
  endpoint <- "getVtdsElcntInfoInqire"
  query <- list(
    serviceKey = get_krvote2_key(),
    pageNo = pageNo,
    numOfRows = numOfRows,
    resultType = "json",
    sgId = sgId,
    sdName = sdName,
    wiwName = wiwName
  )

  res <- call_krvote2_api(endpoint, query)
  return(res)
}

#' 구시군별 선거인수 정보 조회
#'
#' @param sgId 선거ID
#' @param sdName 시도명
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 출력 개수 (기본값: 10, 최대값: 100)
#'
#' @return 구시군별 선거인수 정보 데이터프레임
#' @export
#'
#' @examples
#' get_sigungu_voters(sgId = "20200415", sdName = "서울특별시")
get_sigungu_voters <- function(sgId, sdName, pageNo = 1, numOfRows = 10) {
  endpoint <- "getGsigElcntInfoInqire"
  query <- list(
    serviceKey = get_krvote2_key(),
    pageNo = pageNo,
    numOfRows = numOfRows,
    resultType = "json",
    sgId = sgId,
    sdName = sdName
  )

  res <- call_krvote2_api(endpoint, query)
  return(res)
}

#' 시도별 선거인수 정보 조회
#'
#' @param sgId 선거ID
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 출력 개수 (기본값: 10, 최대값: 100)
#'
#' @return 시도별 선거인수 정보 데이터프레임
#' @export
#'
#' @examples
#' get_sido_voters(sgId = "20200415")
get_sido_voters <- function(sgId, pageNo = 1, numOfRows = 10) {
  endpoint <- "getCtpvElcntInfoInqire"
  query <- list(
    serviceKey = get_krvote2_key(),
    pageNo = pageNo,
    numOfRows = numOfRows,
    resultType = "json",
    sgId = sgId
  )

  res <- call_krvote2_api(endpoint, query)
  return(res)
}

#' 모든 선거인수 정보 조회
#'
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드 (선거구별 조회 시 필요)
#' @param sdName 시도명 (옵션)
#' @param wiwName 구시군명 (옵션)
#'
#' @return 모든 선거인수 정보를 포함하는 리스트
#' @export
#'
#' @examples
#' get_all_voters(sgId = "20200415", sgTypecode = "2", sdName = "서울특별시")
get_all_voters <- function(sgId, sgTypecode = NULL, sdName = NULL, wiwName = NULL) {
  result <- list()

  # 선거구별 정보 조회
  if (!is.null(sgTypecode)) {
    result$constituency <- get_constituency_voters(sgId, sgTypecode, sdName, wiwName)
  }

  # 시도별 정보 조회
  result$sido <- get_sido_voters(sgId)

  # 구시군별 정보 조회 (sdName이 제공된 경우)
  if (!is.null(sdName)) {
    result$sigungu <- get_sigungu_voters(sgId, sdName)

    # 읍면동별 및 투표구별 정보 조회 (wiwName이 제공된 경우)
    if (!is.null(wiwName)) {
      result$emd <- get_emd_voters(sgId, sdName, wiwName)
      result$voting_district <- get_voting_district_voters(sgId, sdName, wiwName)
    }
  }

  return(result)
}

# 내부 함수: API 호출
call_krvote2_api <- function(endpoint, query) {
  base_url <- "http://apis.data.go.kr/9760000/ElcntInfoInqireService"
  url <- paste0(base_url, "/", endpoint)

  response <- GET(url, query = query)

  if (status_code(response) != 200) {
    stop("API request failed with status code: ", status_code(response))
  }

  content <- content(response, "text", encoding = "UTF-8")
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
