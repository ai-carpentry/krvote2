# 단일 페이지 조회 함수들

#' 선거 목록 조회 (단일 페이지)
#'
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#'
#' @return 선거 데이터프레임
#' @export
#'
#' @examples
#' get_elections()
get_elections <- function(pageNo = 1, numOfRows = 10) {

  serviceKey <- get_krvote2_key()
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

#' 행정구역 목록 조회 (단일 페이지)
#'
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#' @param sdName 시도명 (기본값: NULL)
#'
#' @return 행정구역 데이터프레임
#' @export
#'
#' @examples
#' get_sigungu(sgId = "20220309")
get_sigungu <- function(pageNo = 1, numOfRows = 10, sgId, sdName = NULL) {

  serviceKey <- get_krvote2_key()
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

#' 선거구 목록 조회 (단일 페이지)
#'
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#'
#' @return 선거구 데이터프레임
#' @export
#'
#' @examples
#' get_constituencies(sgId = "20220309", sgTypecode = "1")
get_constituencies <- function(pageNo = 1, numOfRows = 10, sgId, sgTypecode) {

  serviceKey <- get_krvote2_key()
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

#' 정당 목록 조회 (단일 페이지)
#'
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#'
#' @return 정당 데이터프레임
#' @export
#'
#' @examples
#' get_parties(sgId = "20220309")
get_parties <- function(pageNo = 1, numOfRows = 10, sgId) {

  serviceKey <- get_krvote2_key()
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

#' 직업 목록 조회 (단일 페이지)
#'
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#'
#' @return 직업 데이터프레임
#' @export
#'
#' @examples
#' get_occupations(sgId = "20220309")
get_occupations <- function(pageNo = 1, numOfRows = 10, sgId) {

  serviceKey <- get_krvote2_key()
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

#' 학력 목록 조회 (단일 페이지)
#'
#' @param pageNo 페이지 번호 (기본값: 1)
#' @param numOfRows 페이지당 데이터 수 (기본값: 10)
#' @param sgId 선거ID
#'
#' @return 학력 데이터프레임
#' @export
#'
#' @examples
#' get_education_levels(sgId = "20220309")
get_education_levels <- function(pageNo = 1, numOfRows = 10, sgId) {

  serviceKey <- get_krvote2_key()
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

# get_all* 함수들

#' 전체 선거 목록 조회
#'
#' @return 모든 선거 데이터프레임
#' @export
#'
#' @examples
#' get_all_elections()
get_all_elections <- function() {
  all_data <- data.frame()
  page_no <- 1
  num_of_rows <- 100  # 한 번에 가져올 최대 행 수

  repeat {
    response <- get_elections(pageNo = page_no, numOfRows = num_of_rows)

    if (nrow(response) == 0) {
      break  # 더 이상 데이터가 없으면 반복 중단
    }

    all_data <- rbind(all_data, response)

    if (nrow(response) < num_of_rows) {
      break  # 마지막 페이지라면 반복 중단
    }

    page_no <- page_no + 1
  }

  return(all_data)
}

#' 전체 행정구역 목록 조회
#'
#' @param sgId 선거ID
#' @param sdName 시도명 (기본값: NULL)
#'
#' @return 모든 행정구역 데이터프레임
#' @export
#'
#' @examples
#' get_all_sigungu(sgId = "20220309")
get_all_sigungu <- function(sgId, sdName = NULL) {
  all_data <- data.frame()
  page_no <- 1
  num_of_rows <- 100  # 한 번에 가져올 최대 행 수

  repeat {
    response <- get_sigungu(pageNo = page_no, numOfRows = num_of_rows, sgId = sgId, sdName = sdName)

    if (nrow(response) == 0) {
      break  # 더 이상 데이터가 없으면 반복 중단
    }

    all_data <- rbind(all_data, response)

    if (nrow(response) < num_of_rows) {
      break  # 마지막 페이지라면 반복 중단
    }

    page_no <- page_no + 1
  }

  return(all_data)
}

#' 전체 선거구 목록 조회
#'
#' @param sgId 선거ID
#' @param sgTypecode 선거종류코드
#'
#' @return 모든 선거구 데이터프레임
#' @export
#'
#' @examples
#' get_all_constituencies(sgId = "20220309", sgTypecode = "1")
get_all_constituencies <- function(sgId, sgTypecode) {
  all_data <- data.frame()
  page_no <- 1
  num_of_rows <- 100  # 한 번에 가져올 최대 행 수

  repeat {
    response <- get_constituencies(pageNo = page_no, numOfRows = num_of_rows, sgId = sgId, sgTypecode = sgTypecode)

    if (nrow(response) == 0) {
      break  # 더 이상 데이터가 없으면 반복 중단
    }

    all_data <- rbind(all_data, response)

    if (nrow(response) < num_of_rows) {
      break  # 마지막 페이지라면 반복 중단
    }

    page_no <- page_no + 1
  }

  return(all_data)
}


