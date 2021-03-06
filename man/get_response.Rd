% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_functions.R
\name{get_response}
\alias{get_response}
\title{Sends a query to Zendesk and automatically paginates the results}
\usage{
get_response(connection, url, query = list(), page_limit = Inf,
  parsing = "R")
}
\arguments{
\item{url}{API edge to query}

\item{query}{List of query parameters for the edge}

\item{page_limit}{Number of pages to query}

\item{parsing}{If 'JSON' then returns JSON, if 'R' then converts JSON to R lists.}
}
\value{
List of paginated responses from Zendesk
}
\description{
Sends a query to Zendesk and automatically paginates the results
}
\examples{

# get all ads in avaible to the token for a particular account
\dontrun{
get_response(
   url = "https://graph.facebook.com/v2.8/act_16429592/ads",
   query = list(
       access_token = access_token,
       limit = 1000,
       fields = "id,adset_id,campaign_id,account_id,configured_status,created_time,creative,effective_status" # ad_review_feedback
   ),
   parsing = 'JSON'
)
}


}

