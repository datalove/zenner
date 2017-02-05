
#' Get a list of tickets from Zendesk that changed from a particular start time
#'
#' @param access_token A Zendesk access token
#' @param email_id Email address of admin account to query using
#' @param start_time A Date or POSIXCT datetime
#'
#' @return a dataframe of tickets
#' @export
#'
get_tickets_incremental <- function(access_token, email_id, start_time) {

  resp <- get_response(
    access_token = access_token,
    email_id     = email_id,
    url          = 'https://chumbacasino.zendesk.com/api/v2/incremental/tickets.json',
    query        = list(start_time = as.character(as.integer(start_time))),#, include = 'users, groups, organizations'),
    parsing      = 'JSON'
  )
  # resp
  process_one <- function(x) {
    x %>%
      enter_object('tickets') %>% gather_array() %>%
      spread_values(
        id              = jnumber('id'),
        url             = jstring('url'),
        organization_id = jnumber('organization_id'),
        submitter_id    = jnumber('submitter_id'),
        requester_id    = jnumber('requestor_id'),
        assignee_id     = jnumber('assignee_id'),
        status          = jstring('status'),
        type            = jstring('type'),
        subject         = jstring('subject'),
        raw_subject     = jstring('raw_subject'),
        description     = jstring('description'),
        created_at      = jstring('created_at'),
        updated_at      = jstring('updated_at'),
        external_id     = jstring('external_id'),
        group_id        = jnumber('group_id')

      ) %>%
      enter_object('via') %>% spread_values( channel = jstring('channel') ) %>%
      enter_object('source') %>% enter_object('from') %>%
      spread_values(
        address     = jstring('address'),
        from_name   = jstring('name'),
        profile_url = jstring('profile_url')
      ) %>%
      as_data_frame() %>%
      select(-document.id, -array.index) %>%
      mutate(
       created_at    = as.POSIXct(strptime(created_at, '%Y-%m-%dT%H:%M:%SZ')),
       updated_at    = as.POSIXct(strptime(updated_at, '%Y-%m-%dT%H:%M:%SZ'))
      )

  }

  purrr::map_df(resp, process_one)

}


#' Get a list of users from Zendesk that changed from a particular start time
#'
#' @param access_token A Zendesk access token
#' @param email_id Email address of admin account to query using
#' @param start_time A Date or POSIXCT datetime
#'
#' @return a dataframe of users
#' @export
#'
get_users_incremental <- function(access_token, email_id, start_time) {

  resp <- get_response(
    access_token = access_token,
    email_id     = email_id,
    url          = 'https://chumbacasino.zendesk.com/api/v2/incremental/users.json',
    query        = list(start_time = as.character(as.integer(start_time))),
    parsing      = 'JSON'
  )

  process_one <- function(x) {
    x %>%
      enter_object('users') %>% gather_array() %>%
      spread_values(
        id = jnumber('id'),
        url = jstring('url'),
        name = jstring('name'),
        email = jstring('email'),
        created_at = jstring('created_at'),
        updated_at = jstring('updated_at'),
        time_zone = jstring('time_zone'),
        phone = jstring('phone'),
        shared_phone_number = jstring('shared_phone_number'),
        photo = jstring('photo'),
        locale_id = jnumber('locale_id'),
        locale = jstring('locale'),
        organization_id = jnumber('organization_id'),
        role = jstring('role'),
        verified = jstring('verified'),
        external_id = jstring('external_id'),
        tags = jstring('tags'), # json field,
        alias = jstring('alias'),
        active = jstring('active'),
        shared = jstring('shared'),
        shared_agent = jstring('shared_agent'),
        last_login_at = jstring('last_login_at'),
        two_factor_auth_enabled = jstring('two_factor_auth_enabled'),
        signature = jstring('signature'),
        details = jstring('details'),
        notes = jstring('notes'),
        custom_role_id = jnumber('custom_role_id'),
        moderator = jstring('moderator'),
        ticket_restriction = jstring('ticket_restriction'),
        only_private_comments = jstring('only_private_comments'),
        restricted_agent = jstring('restricted_agent'),
        suspended = jstring('suspended'),
        chat_only = jstring('chat_only'),
        user_fields = jstring('user_fields')
      ) %>% as_data_frame() %>%
      select(-document.id, -array.index) %>%
      mutate(
       created_at    = as.POSIXct(strptime(created_at, '%Y-%m-%dT%H:%M:%SZ')),
       updated_at    = as.POSIXct(strptime(updated_at, '%Y-%m-%dT%H:%M:%SZ')),
       last_login_at = as.POSIXct(strptime(last_login_at, '%Y-%m-%dT%H:%M:%SZ'))
      )
  }
  map_df(resp, process_one)
}
