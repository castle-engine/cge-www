#!/bin/bash
set -eu

backup_svn ()
{
  mkdir -p subversion/
  cd subversion/
  rsync -av vrmlengine.svn.sourceforge.net::svn/vrmlengine/* .
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
