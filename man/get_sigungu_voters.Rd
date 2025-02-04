% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nec_constituency_voters.R
\name{get_sigungu_voters}
\alias{get_sigungu_voters}
\title{구시군별 선거인수 정보 조회}
\usage{
get_sigungu_voters(sgId, sdName, pageNo = 1, numOfRows = 10)
}
\arguments{
\item{sgId}{선거ID}

\item{sdName}{시도명}

\item{pageNo}{페이지 번호 (기본값: 1)}

\item{numOfRows}{페이지당 출력 개수 (기본값: 10, 최대값: 100)}
}
\value{
구시군별 선거인수 정보 데이터프레임
}
\description{
구시군별 선거인수 정보 조회
}
\examples{
get_sigungu_voters(sgId = "20200415", sdName = "서울특별시")
}
