# Maintain redirects from SourceForge to new website

We want accessing https://castle-engine.sourceforge.io/ (obsolete SourceForge website) to redirect to https://castle-engine.io/ (new website).

We cannot achieve this perfectly as in 2025, [SourceForge disabled both .htaccess and PHP support](https://sourceforge.net/p/forge/documentation/Project%20Web%20Services/).

So we generate some static HTML files to make redirects. See
- https://en.wikipedia.org/wiki/Meta_refresh
- https://stackoverflow.com/questions/5411538/how-to-redirect-one-html-page-to-another-on-load

Redirects done this way are not advised, but we have no choice.