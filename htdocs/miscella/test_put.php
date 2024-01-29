<?php
/* Trivial PHP script to test HTTP PUT.
   Done to showcase CGE example
   examples/network/put_data/put_data.dpr
*/
if ($_SERVER['REQUEST_METHOD'] === 'PUT') {
  $myEntireBody = file_get_contents('php://input');
  echo 'Got PUT request.
Length: ' . strlen($myEntireBody) . '
Content: ' . htmlspecialchars($myEntireBody);
} else {
  echo 'Not a PUT request.';
}
