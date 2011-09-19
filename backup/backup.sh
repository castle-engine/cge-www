#!/bin/bash
set -eu

backup_svn ()
{
  mkdir -p subversion/
  cd subversion/

  # Documented on
  # https://sourceforge.net/apps/trac/sourceforge/wiki/SVN%20adminrepo
  # rsync -av vrmlengine.svn.sourceforge.net::svn/vrmlengine/* .

  # Documented at the bottom of
  # https://sourceforge.net/p/forge/documentation/svn%20-%20Beta/
  rsync -av svn.code.sf.net::p/castle-engine/code/* .
  cd ../
}

backup_xml ()
{
  export PERL5LIB="$HOME"/sources/adocman/adocman-0.13/
  xml_export --username=kambi --password=`print_sf_password` --groupid=200653 --login
  xml_export --groupid=200653 > vrmlengine_export.xml
}

backup_svn
backup_xml
