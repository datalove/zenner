#' Sends a query to Zendesk and automatically paginates the results
#'
#' @param query List of query parameters for the edge
#' @param url API edge to query
#' @param page_limit Number of pages to query
#' @param parsing If 'JSON' then returns JSON, if 'R' then converts JSON to R lists.
#'
#' @return List of paginated responses from Zendesk
#'
#' @import httr
#' @export
#'
#' @examples
#'
#' # get all ads in avaible to the token for a particular account
#' \dontrun{
#' get_response(
#'    url = "https://graph.facebook.com/v2.8/act_16429592/ads",
#'    query = list(
#'        access_token = access_token,
#'        limit = 1000,
#'        fields = "id,adset_id,campaign_id,account_id,configured_status,created_time,creative,effective_status" # ad_review_feedback
#'    ),
#'    parsing = 'JSON'
#' )
#' }

#'
#'
# get_response <- function(access_token, email_id, url, query = list(), page_limit = Inf, parsing = 'R') {
get_response <- function(connection, url, query = list(), page_limit = Inf, parsing = 'R') {

  # set up pagination
  page <- 1

  # display first query
  message('Page ', page,': ', modify_url(url, query = query), '\n')

  # get first response
  resp <- GET(url = url, connection, query = query)

  # stop and spit out error if necessary
  stop_for_status(resp)

  # run a while loop to capture data rows through pagination
  page <- 2

  # create empty list to start populatibg
  contents <- list()

  # configure how results are to be parsed
  type <- ifelse(parsing == 'JSON', 'text',ifelse(parsing == 'R', 'parse', parsing))

  # get contents from first response
  contents[[1]] <- content(resp, type)[[1]]

  # if FB returns contents containing 'next' we need to paginate
  # while('next_page' %in% names(content(resp, as = 'parse')) && page <= page_limit) {
  while(content(resp, as = 'parse')[['count']] == 1000 && page <= page_limit) {

    next_page <- content(resp)[['next_page']]
    message('Page ', page,': ', next_page, "\n")
    resp <-  GET(next_page, connection)
    stop_for_status(resp)

    contents[[page]] <- content(resp, as = type)
    page <- page + 1
  }

  # return list of paginated responses
  contents

}



#' Create an authtenication object to connect to Zendesk
#'
#' @param email_id An email_id for an account admin in Zendesk. Required paramter.
#' @param access_token An access token for the Zendesk API. If access_token is null, a password must be supplied.
#' @param password The password for the email_id. If password is null, an access_token must be supplied.
#'
#' @return
#' @export
#'
create_connection <- function(email_id, access_token = NULL, password = NULL) {

  if(is.null(access_token) && is.null(password))
    stop("One of 'access_token' or 'password' must not be null")

  if(!is.null(access_token)) {
    auth <- authenticate(paste0(email_id,'/token'), access_token)
  } else {
    auth <- authenticate(email_id, password)
  }

  auth

}


#' Takes a list of pages and combines it into a single piece of json
#'
#' @param pages list of json from individual pages
#'
#' @return json array of paginated json objects
#'
combine_json_pages <- function(pages) {
  pages %>%
    paste0(collapse = ',') %>%
    paste0('[', ., ']')
}
