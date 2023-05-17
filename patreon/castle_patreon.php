<?php

/* ------------------------------------------------------------------------------

  Query CGE state on Patreon.
  Most prominently: the current status (percentage of completion)
  of our nearest goal on Patreon.

  We use Patreon API, following
  https://docs.patreon.com/
  https://docs.patreon.com/#fetch-a-creator-profile-and-campaign-info
  It is wrapped in a thin PHP wrapper from https://github.com/Patreon/patreon-php .

  We don't use Patreon Wordpress code
  https://wordpress.org/plugins/patreon-connect/
  as we don't need the Wordpress integration.

  ----------------------------------------------------------------------------
*/

require_once 'API.php';

/* Internal.
   Search "included" array of various Patreon API answers for a resource (anything)
   of given id.
   Returns NULL if not found.
*/
// Unused now
/*
function _castle_patreon_find_included_resource($api_answer, $id_resource)
{
  foreach ($api_answer['included'] as $included_data) {
    if ($included_data['id'] == $id_resource) {
      return $included_data;
    }
  }
  return NULL;
}
*/

/* Process paginated array response to get all pages in turn. */
function patreon_read_all_pages($api_client, $request_str)
{
  $last_page = $api_client->get_data($request_str);
  $result = $last_page['data'];

  while (!empty($last_page['meta']['pagination']['cursors']['next'])) {
    $cursor = $last_page['meta']['pagination']['cursors']['next'];
    $escaped_cursor = urlencode($cursor);
    $last_page = $api_client->get_data($request_str ."&page%5Bcursor%5D={$escaped_cursor}");
    $result = array_merge($result, $last_page['data']);
  }

  return $result;
}

/* Returns the details about nearest Patreon goal.
   Returns associative array with these fields:

    'amount_cents'
    'completed_percentage'
    'created_at'
    'description'
    'reached_at'
    'title'

  See https://docs.patreon.com/#apiv2-resources
  for their meaning, they match Patreon API answer. */
function castle_patreon_nearest_goal()
{
  // castle_patreon_config.php is not in repo.
  // It should define $cge_patreon_access_token = '...';
  include 'castle_patreon_config.php';

  if (!isset($cge_patreon_access_token)) {
    throw new Exception('Patreon access token not defined');
    /*
    return array(
      'amount_cents' => 0,
      'completed_percentage' => 0,
      'created_at' => NULL,
      'description' => NULL,
      'reached_at' => NULL,
      'title' => 'Patreon access token not defined'
    );
    */
  }

  $api_client = new Patreon\API($cge_patreon_access_token);

  $campaigns_list = $api_client->fetch_campaigns();
  if (!isset($campaigns_list['data'][0]['id'])) {
    throw new Exception('Patreon campaigns unexpected answer: ' . print_r($campaigns_list, true));
  }
  $campaign_id = $campaigns_list['data'][0]['id'];

  //$campaign_details = $api_client->fetch_campaign_details($campaign_id);

  /* To see available fields, see https://docs.patreon.com/#apiv2-resources

     Or just run v1 query that returns everything:
     curl --request GET \
      --url https://www.patreon.com/api/oauth2/api/current_user/campaigns \
      --header 'Authorization: Bearer <access-token>'
  */
  /*
  // No longer useful, Patreon removed goals details

  $campaign_details = $api_client->get_data("campaigns/{$campaign_id}?include=campaign_installations,benefits,creator,goals,tiers&fields%5Bgoal%5D=amount_cents,completed_percentage,created_at,description,reached_at,title");
  //print_r($campaign_details);
  */

  $campaign_details = $api_client->get_data("campaigns/{$campaign_id}?fields%5Bcampaign%5D=patron_count");
  //print_r($campaign_details);
  $patron_count = $campaign_details['data']['attributes']['patron_count'];
  // echo 'Current Patrons count ' . $patron_count . "\n";

  $campaign_members = patreon_read_all_pages($api_client, "campaigns/{$campaign_id}/members?fields%5Bmember%5D=currently_entitled_amount_cents,full_name,pledge_cadence,patron_status,will_pay_amount_cents");

  /* Since Patreon removed goals, we need to get all members and sum their
     contribution manually.
     Note that we need to support pagination, despite docs saying pagination
     starts at 500, it seems page only contains 20. */
  $total_pledges = 0;
  $members_counted = 0;
  $members_counted_nonzero = 0;
  //print_r($campaign_members);
  foreach ($campaign_members as $member) {
    $member_current_pledge = $member['attributes']['currently_entitled_amount_cents'];

    if (isset($member['attributes']['pledge_cadence'])) {
      $pledge_cadence = $member['attributes']['pledge_cadence'];
      /* Divide by $pledge_cadence, to divide by 12 for amnual payments */
      $member_current_pledge = $member_current_pledge / $pledge_cadence;
    }

    if (isset($member['attributes']['patron_status'])) {
      $member_status = $member['attributes']['patron_status'];
      // reject pledges from Patrons with status = Declined
      if ($member_status != 'active_patron') {
        $member_current_pledge = 0;
      }
    }

    $total_pledges += $member_current_pledge;
    $members_counted++;
    if ($member_current_pledge > 0) {
      $members_counted_nonzero++;
      //echo 'Adding ' . $member_current_pledge . ' from ' . $member['attributes']['full_name'] . "\n";
    }
  }

  // echo 'Members counted: ' . $members_counted . "\n";
  // echo 'Members counted with non-zero: ' . $members_counted_nonzero . "\n";
  // echo 'Total pledges: ' . $total_pledges . "\n";

  if ($members_counted_nonzero != $patron_count) {
    throw new Exception('Members counted with non-zero is not equal to Patrons count');
  }

  // in emergency, you can just hardcode this
  // TODO: above algorithm calculates value slightly larger (but matching Patron manager)
  $total_pledges = 18100;

  // goals are hardcoded now here, as Patreon removed goals
  $goals = array(
    array(
      'amount_cents' => 25000,
    ),
    array(
      'amount_cents' => 50000,
    ),
    array(
      'amount_cents' => 100000,
    ),
  );

  // calculate $nearest_goal
  foreach ($goals as $goal_attribs) {
    $nearest_goal = $goal_attribs;
    if ($goal_attribs['amount_cents'] > $total_pledges) {
      break;
    }
  }

  // set $nearest_goal['completed_percentage'], our _castle_patreon_box expects it
  $nearest_goal['completed_percentage'] =
    (int)round($total_pledges / $nearest_goal['amount_cents'] * 100);

  return $nearest_goal;
}

/* Echo JSON with hardcoded information about nearest CGE goal. */
function castle_echo_patreon_nearest_goal_json()
{
  $nearest_goal = castle_patreon_nearest_goal();
  echo json_encode($nearest_goal);
  // echo  "\n"; // easier debug
}

castle_echo_patreon_nearest_goal_json();
