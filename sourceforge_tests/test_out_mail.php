<?php

/* Testing mail sending, following example from
   https://sourceforge.net/p/forge/documentation/Project%20Web%20Email/ .
   We want to make sure that we have working config, for Wordpress.

   Needed local install of pear package Mail.php

// pear config-set php_dir /home/project-web/castle-engine/pear
// pear config-set data_dir /home/project-web/castle-engine/pear
// pear config-set test_dir /home/project-web/castle-engine/pear/test
// pear config-set doc_dir /home/project-web/castle-engine/pear/doc
// check: less ~/.pearrc
// pear install --alldeps mail

*/

set_include_path('/home/project-web/castle-engine/pear' . PATH_SEPARATOR
                 . get_include_path());

// testing pear
//echo get_include_path();
//require_once 'System.php';
//var_dump(class_exists('System', false));

include('Mail.php');

$recipients = array( 'michalis.kambi@gmail.com' ); # Can be one or more emails

$headers = array (
		  'From' => 'no-reply@castle-engine.sourceforge.io',
		  'To' => join(', ', $recipients),
		  'Subject' => 'Testing email from project web',
		  );

$body = "This was sent via php from project web!\n";

$mail_object =& Mail::factory('smtp',
    array(
         'host' => 'prwebmail',
         'auth' => true,
         'username' => 'castle-engine',
         'password' => 'FILL-THIS', # As set on your project's config page
         'debug' => true, # uncomment to enable debugging
    ));

echo 'Start...' . "\n";

$mail_object->send($recipients, $headers, $body);

echo 'Done...' . "\n";
