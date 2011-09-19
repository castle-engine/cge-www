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
  # It seems that below doesn't work (generated xml_export_output is empty)
  echo -n 'xml_export: '
  echo -n 'Logging in... '; xml_export --username=kambi --password=`print_sf_password` --login
  echo -n 'Downloading... '; xml_export --groupid=200653 > xml_export_output.xml
  echo 'Done.'

  echo '--------------------'
  echo "Please check `pwd`/xml_export_output.xml"
  echo "--- if it's bad, you have to use"
  echo "[http://sourceforge.net/export/xml_export.php?group_id=4213]"
  echo " from a real WWW browser"
  echo '--------------------'
}

backup_svn
#backup_xml
