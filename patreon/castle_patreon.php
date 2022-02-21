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
function _castle_patreon_find_included_resource($api_answer, $id_resource)
{
  foreach ($api_answer['included'] as $included_data) {
    if ($included_data['id'] == $id_resource) {
      return $included_data;
    }
  }
  return NULL;
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

  if (!$cge_patreon_access_token) {
    die('Patreon access token not defined');
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
  $campaign_id = $campaigns_list['data'][0]['id'];

  //$campaign_details = $api_client->fetch_campaign_details($campaign_id);

  /* To see available fields, see https://docs.patreon.com/#apiv2-resources

     Or just run v1 query that returns everything:
     curl --request GET \
      --url https://www.patreon.com/api/oauth2/api/current_user/campaigns \
      --header 'Authorization: Bearer <access-token>'
  */
  $campaign_details = $api_client->get_data("campaigns/{$campaign_id}?include=benefits,creator,goals,tiers&fields%5Bgoal%5D=amount_cents,completed_percentage,created_at,description,reached_at,title");
  //print_r($campaign_details);

  // calculate $goals, containing details for each CGE goal on Patreon
  $goals = array();
  foreach ($campaign_details['data']['relationships']['goals']['data'] as $goal_link) {
    $goal_id = $goal_link['id'];
    $goal_details = _castle_patreon_find_included_resource($campaign_details, $goal_id);
    if ($goal_details === NULL) {
      die('Goal details for ' . htmlspecialchars($goal_id) . ' not found');
    }
    $goals[] = $goal_details['attributes'];
  }

  // print_r($goals);
  /* $goals should look like this now:
  Array
  (
      [0] => Array
          (
              [amount_cents] => 25000
              [completed_percentage] => 62
              [created_at] => 2017-01-24T06:07:05.000+00:00
              [description] => ....
              [reached_at] => 2021-03-10T08:23:23.000+00:00
              [title] =>
          )

      [1] => Array
          (
              [amount_cents] => 50000
              [completed_percentage] => 31
              [created_at] => 2017-01-24T06:07:05.000+00:00
              [description] => ....
              [reached_at] =>
              [title] =>
          )

      [2] => Array
          (
              [amount_cents] => 100000
              [completed_percentage] => 15
              [created_at] => 2017-01-24T06:14:30.000+00:00
              [description] => ....
              [reached_at] =>
              [title] =>
          )

  )
  */

  // calculate $nearest_goal
  $nearest_goal = $goals[0];
  foreach ($goals as $goal_attribs) {
    if ($goal_attribs['completed_percentage'] > $nearest_goal['completed_percentage']) {
      $nearest_goal =  $goal_attribs;
    }
  }

  return $nearest_goal;
}

/* Echo JSON with hardcoded information about nearest CGE goal. */
function castle_echo_patreon_nearest_goal_json()
{
  $nearest_goal = castle_patreon_nearest_goal();
  echo json_encode($nearest_goal);
}

castle_echo_patreon_nearest_goal_json();
