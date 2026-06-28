# CGE Social — one-click reposting (must-use plugin)

Own code, no third-party middleman. Every request goes straight from this
WordPress server to the official API of each network. Nothing is sent through
Zapier/Buffer/FS-Poster/etc., and no credentials leave this server.

This was written to automate the **manual** part of the Castle Game Engine news
workflow. The automatic part (LinkedIn *company* page, Mastodon, Bluesky, and the
Discourse forum) keeps working as before via Jetpack + wp-discourse.

### What it does

`../cge-social.php` + this directory adds a **"CGE Social — repost"** box to the post editor with one button per network: **Discord, Facebook (page), Reddit, X/Twitter**.

Must-use plugins are always active and can't be deactivated from the admin — good
for first-party glue code.

### The editor box

On every published post you get, per configured network:
- a text area pre-filled with a sensible default (you edit it — e.g. paste the
  per-platform tags), and
- a **Post to …** button that posts immediately and shows ✓/✗ inline.

The result of each attempt is saved in post meta (`cge_social_log`) so the status
survives reloads and you can see what was already posted where.

### Why buttons, not fully-automatic-on-publish?

Because the real workflow already involves crafting *per-platform* tags.
Discord and the Discourse "listed" fix need no custom text, so the Discourse one
*is* fully automatic; Discord is a button only for consistency (it's trivial to
make it auto — see "Extending" below).

## What is NOT here, and why

- **Patreon** — there is no official post-creation API (it was removed; developer
  support ended in 2020). Every "automation" is browser scripting against ToS.
  Keep posting to Patreon manually.
- **LinkedIn personal profile** — the API technically supports it
  (`w_member_social`), but access is gated behind Marketing Developer Platform
  approval that's effectively enterprise-only. Not worth it; the *company* page is
  already covered by Jetpack. Keep the personal reshare manual.

## Security notes

- Secrets can live **either** in the DB (entered on Settings → CGE Social) **or**
  as constants in `wp-config-production.php` (git-ignored). Constants win and lock
  the field in the UI. For real production secrets, prefer the constants so they
  never enter DB backups or the repo. Constant name = `CGE_SOCIAL_` + the
  upper-cased key, e.g.:

  ```php
  // in wp-config-production.php
  define( 'CGE_SOCIAL_DISCORD_WEBHOOK',     'https://discord.com/api/webhooks/…' );
  define( 'CGE_SOCIAL_FACEBOOK_PAGE_ID',    '123…' );
  define( 'CGE_SOCIAL_FACEBOOK_PAGE_TOKEN', 'EAAB…' );
  define( 'CGE_SOCIAL_REDDIT_CLIENT_ID',     '…' );
  define( 'CGE_SOCIAL_REDDIT_CLIENT_SECRET', '…' );
  define( 'CGE_SOCIAL_REDDIT_REFRESH_TOKEN', '…' );  // or use the Connect button
  define( 'CGE_SOCIAL_REDDIT_SUBREDDIT',     'castleengine' );
  define( 'CGE_SOCIAL_TWITTER_API_KEY',       '…' );
  define( 'CGE_SOCIAL_TWITTER_API_SECRET',    '…' );
  define( 'CGE_SOCIAL_TWITTER_ACCESS_TOKEN',  '…' );
  define( 'CGE_SOCIAL_TWITTER_ACCESS_SECRET', '…' );
  ```

- **Never commit secrets.** The plugin code in this directory is git-tracked;
  keep credentials out of it. (If you must use the DB option route, remember the
  values are in the `wp_options` table and thus in DB dumps.)
- All admin actions are capability-checked (`manage_options` for settings,
  `edit_post` for posting) and nonce-protected. Password fields are never echoed
  back into the page.
- Outbound calls use `wp_remote_post` with short timeouts.

## Per-network setup

Open **Settings → CGE Social**. Each section shows a ✓ when configured.

### Discord (easiest)
1. Discord → **Server Settings → Integrations → Webhooks → New Webhook**.
2. Pick the target channel, **Copy Webhook URL**.
3. Paste into the Discord section (or set `CGE_SOCIAL_DISCORD_WEBHOOK`).

Posts a message with the article URL; Discord unfurls it into an embed using the
site's OpenGraph tags (title/description/screenshot).

### Facebook (page)
This is the channel Jetpack could never post to. Posting via the Graph API
directly avoids that.
1. Create an app at <https://developers.facebook.com/> (type: Business).
2. In **Graph API Explorer**, get a *User* token with scopes
   `pages_manage_posts`, `pages_read_engagement`, `pages_show_list`.
3. Exchange it for a **long-lived** user token, then call `/me/accounts` and copy
   the **Page** access token for the castleengine page. (A page token derived from
   a long-lived user token doesn't expire while you remain a page admin.)
4. Enter the **Page ID** and **Page access token** (or the constants).

Posts `message` + `link`; Facebook renders the link card with the og:image.

### Reddit (password-less OAuth)
1. <https://www.reddit.com/prefs/apps> → **create app** → type **web app**.
2. Set its **redirect uri** to exactly the URL shown in the CGE Social Reddit
   section (the settings page URL).
3. Enter **Client ID**, **Client secret**, **Subreddit** (`castleengine`), and a
   descriptive **User agent** (required by Reddit, e.g.
   `web:io.castle-engine.cgesocial:v0.1 (by /u/yourname)`), then **Save**.
4. Click **Connect Reddit** once. This runs the auth-code flow and stores a
   permanent refresh token. No Reddit password is ever stored; 2FA is fine.

Submits a **link post** (title + article URL), like you do today.

### X / Twitter (paid per post)
1. <https://developer.x.com> → create a project/app, set app permissions to
   **Read and write**.
2. Generate **API Key + Secret** (consumer) and an **Access Token + Secret** for
   the @castleengine account. **Regenerate the access token after** switching to
   Read+write, or posting returns 403.
3. Enter all four values (or the constants).

> 💸 X has **no free tier** since Feb 2026. Pay-per-use is ~$0.015/post, or
> ~$0.20 if the post contains a link. The default text includes the link (so the
> card shows). At a few posts/month this is cents. To avoid the link fee you'd
> post text-only and add the link as a reply — not implemented.

Posts text only; X unfurls the URL into a card via OpenGraph.

## Images / screenshots

v1 relies on each network unfurling the article URL into a preview card using the
site's OpenGraph tags (the `simple-facebook-og-image` plugin already guarantees an
`og:image`). So your screenshot shows up without uploading it separately.

True native media upload (Twitter v1.1 `media/upload`, Facebook `/photos`, Discord
embeds with attached files) is a deliberate future enhancement — see below.

## Extending

- **Make Discord (or any network) auto-post on publish:** add, in
  `cge-social/class-cge-social.php` or a new hook,
  ```php
  add_action( 'transition_post_status', function ( $new, $old, $post ) {
      if ( 'publish' === $new && 'publish' !== $old && 'post' === $post->post_type ) {
          $n = CGE_Social::instance()->network( 'discord' );
          if ( $n && $n->is_configured() ) { $n->publish( $post, $n->default_message( $post ) ); }
      }
  }, 10, 3 );
  ```
  The network classes don't care whether a button or a hook calls `publish()`.
- **Add a network:** create `cge-social/networks/class-cge-social-<id>.php`
  extending `CGE_Social_Network`, `require_once` it in `cge-social.php`, and add it
  to the registry array in `class-cge-social.php`.

## File map

```
mu-plugins/
  cge-discourse-listed.php              # standalone: forum topics published as listed
  cge-social.php                        # loader (requires the files below)
  cge-social/
    README.md                           # this file
    class-cge-social-config.php         # secret access: constant-or-option
    class-cge-social.php                # bootstrap + network registry
    class-cge-social-settings.php       # Settings → CGE Social, + Reddit connect
    class-cge-social-metabox.php        # editor box + AJAX publish + per-post log
    networks/
      class-cge-social-network.php      # abstract base + CGE_Social_Result
      class-cge-social-discord.php
      class-cge-social-facebook.php
      class-cge-social-reddit.php
      class-cge-social-twitter.php
```
