<?php

  /* Testing mail sending, following example from
     https://sourceforge.net/apps/trac/sourceforge/wiki/Project%20Web%20Email%20Configuration
     I want to make sure that I have working config, I need email working for my local piwik
     (as piwik hosted by SF is broken), and probably wordpress in the future.

     Needed local install of pear package Mail.php

  // pear config-set php_dir /home/project-web/castle-engine/pear
  // pear config-set data_dir /home/project-web/castle-engine/pear
  // pear config-set test_dir /home/project-web/castle-engine/pear/test
  // pear config-set doc_dir /home/project-web/castle-engine/pear/doc
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
		  'From' => 'admin@castle-engine.sourceforge.net',
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
