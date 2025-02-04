% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nec_voting_and_counting.R
\name{get_all_count_info}
\alias{get_all_count_info}
\title{모든 개표결과 조회}
\usage{
get_all_count_info(
  sgId,
  sgTypecode,
  sggName = NULL,
  sdName = NULL,
  wiwName = NULL
)
}
\arguments{
\item{sgId}{선거ID}

\item{sgTypecode}{선거종류코드}

\item{sggName}{선거구명 (옵션)}

\item{sdName}{시도명 (옵션)}

\item{wiwName}{구시군명 (옵션)}
}
\value{
모든 개표결과 데이터프레임
}
\description{
모든 개표결과 조회
}
\examples{
get_all_count_info(sgId = "20220309", sgTypecode = "1")
}
