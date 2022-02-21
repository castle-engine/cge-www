# Update information from Patreon about our nearest goal

This contains bash script, that runs PHP script, that asks Patreon over HTTP
"what is the status of our nearest goal".
The information is put in patreon.json .

This script should be run occasionally from cron to keep our Patreon info updated
on CGE website.
patreon.json is read by PHP code that displays our website.

## Why like this?

We want a simple solution that

- Doesn't do any additional work when user views CGE website.
  From the user POV, this is just an information that is displayed on CGE website,
  it should be (and it is) queried unrelated to user browsing.

- We want this querying to be resilient to eventual failures. On our website,
  on Patreon end, somewhere in the middle (faulty connection or whatever)...
  So if anything fails, the last JSON just remains unchanged, and the cron job fails,
  which notifies website admin (Michalis).

- We also want something trivially secure.
  For this, we don't expose here any way to actually do anything on Patreon through CGE website.
  The whole patreon/ code is outside of htdocs/, including secret in uncommitted
  castle_patreon_config.php .
  Even patreon.json is outside of htdocs/ and uncommitted,
  although in practice this doesn't contain anything secret.
