<?php
  require_once "vrmlengine_functions.php";
  vrmlengine_header('Versioning scheme', NULL, array('other'));
?>

<h2>Versioning scheme</h2>

<p>This is versioning scheme used in all Michalis' software on these pages.
This versioning scheme is of course inspired by versioning schemes of many
other programs and libraries, so you don't have to search far to find
software with similar or just plainly the same versioning scheme.

<p>Also some thoughts about "what good versioning scheme should look
like" are given at the bottom of this page.

<dl>
  <dt>Major number:
  <dd>
    <ol>
      <li>Final programs: incremented on some major achievement (like VRML 97
        conformance in view3dscene).
      <li>Libraries or programs that are meant to be used by other programs
        (in "batch" mode, like std UNIX programs):
        incremented on incompatible changes. This should be connected
        with "some major achievement", since there really must exist
        a good reason to do some "incompatible change".
    </ol>

  <dt>Minor number:
  <dd>Incremented when some improvements that are really
    noticeable to all users are done. Incrementing this means
    "all users should really upgrade, because it's really significantly
    better than last version".

    <p style="margin-bottom: 0">
    <i>This doesn't concern my programs on these pages for now,
    but it is generally good idea and known convention :</i>
    If program maintains some sort of stable and unstable
    branches, then it's a good idea to distinguish them by
    odd and even minor numbers.

  <dt>Release number:
  <dd>Incremented each time I upload new compiled version to these pages.
    Sometimes new release fixes only some minor problems
    (that are significant only for a very small amount of users),
    or implement some minor functionality that isn't so
    significant to increment minor number.

    <p><i>This doesn't concern my programs on these pages for now,
    but it is generally good idea and known convention :</i>
    If program has public CVS (or SVN, or similar "live" code repository),
    then it's a good idea to increment release number
    right before <b>and</b> right after each "official" public
    release. This means that even release numbers indicate
    some real, "fixed" version of the program, and odd release numbers
    indicate CVS-version, that has no guarantee about stability,
    and changes on every commit.
</dl>

<p>What IMHO good versioning scheme should look like ?
<ul>
  <li>There <em>must</em> be some release number that can be incremented on
    every, even most minor, change to uploaded binaries.
    This way version number specifies the <em>exact</em> version of a program.
  <li>For libraries/batch programs there <em>must</em> be a number to indicate
    incompatible changes.
  <li>There <em>should</em> be a number to indicate "significant changes" to user.
    Two numbers, major and minor, seem to be good convention.
  <li>If program maintains stable/unstable branches
    (whether it's a good idea to do so depends on the program
    &mdash; it's size, number of developers involved etc.)
    then there <em>must</em> be some easy way to distinguish
    releases of stable and unstable branches.
  <li>If program maintains public code repository, like CVS (so that users
    can see state of code in the middle of development),
    then there <em>should</em> be some easy way to distinguish
    versions compiled from the code in the middle of development
    and actual, "fixed" versions.
</ul>

<p>Things to avoid in good versioning scheme (IMHO, as usual):
<ul>
  <li>Don't use numbers like "9" or "99" in minor/release number to indicate
    "we're really close to releasing next version".
    This is a nice indication for users, but usually one cannot keep
    the schedule. Bad examples (as of 2004-01-16): xine: version v0.99.3,
    xcdroast: version 0.98alpha15. Such version numbers tell only
    that "author thinks that it's close to next major release",
    but they really don't tell users <em>how</em> close it is (since noone
    believes that next xine or xcdroast will have 1.0 version.
    I bet these will be 0.99.4 and 0.98alpha16). So they are actually
    useless for users. And they make confusion, because noone is able
    to tell any more how many minor releases program had (without
    looking at some HISTORY file, that is). E.g. I don't suppose
    that xine had exactly 99 releases (from 0.1 to 0.99)
    and then 3 more.
  <li>For the same reason, don't make assumption like "minor number is always
    2 decimal digits". This is the same problem as discussed above,
    as it implies that
    after 0.99 you will have to switch to 1.0. What's so special
    about the number 99 ? Nothing. You can be sure that you will need
    a lot less or a lot more releases than 99 before you will be able
    to proudly present 1.0 release of your software.
</ul>

<?php
  if (!IS_GEN_LOCAL) {
    $counter = php_counter("versioning", TRUE);
  };

  vrmlengine_footer();
?>