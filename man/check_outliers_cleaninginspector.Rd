% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks_outliers.R
\name{check_outliers_cleaninginspector}
\alias{check_outliers_cleaninginspector}
\title{Check for outliers using cleaninginspectoR package and format the log}
\usage{
check_outliers_cleaninginspector(
  input_tool_data,
  input_enumerator_id_col = "enumerator_id",
  input_location_col
)
}
\arguments{
\item{input_tool_data}{Specify the data frame for the tool data}

\item{input_enumerator_id_col}{Specify the enumerator id column}

\item{input_location_col}{Specify the location description column}
}
\value{
The resulting data frame of data out of range
}
\description{
Check for outliers using cleaninginspectoR package and format the log
}
