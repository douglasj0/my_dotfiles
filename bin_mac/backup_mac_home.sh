#!/bin/sh
#
# Backup the mac files from ${HOME}/blah to ${HOME}/backup
DESTINATION="/Users/Shared/backup"

cd "${HOME}/Library/Application Support"
echo "*** Backing up AddressBook Camino Cultured Code Firefox"
for i in AddressBook Camino "Cultured Code" Firefox; do 
  tar zcf "${i}".tgz ./"${i}"
  mv "${i}".tgz ${DESTINATION}
done

cd ${HOME}/Library
echo "*** Backing up Calendars Mail Preferences Safari"
for j in Calendars Mail Preferences Safari; do
  tar zcf "${j}".tgz ./"${j}"
  mv "${j}".tgz ${DESTINATION}
done

cd ${HOME}
echo "*** Backing up Mutt mail"
tar zcf mutt-Mail.tgz ./Mail
mv mutt-Mail.tgz ${DESTINATION}

#cd ${HOME}/Documents
#echo "*** Backing up Docs"
#hdiutil unmount ${HOME}/Documents/Docs_work.sparsebundle
#hdiutil compact ${HOME}/Documents/Docs_work.sparsebundle
#${HOME}/bin/eject /Volumes/Docs_work
#tar cf Docs_work.sparsebundle.tar ./Docs_work.sparsebundle
#mv Docs_work.sparsebundle.tar ${DESTINATION}
#hdiutil mount ${HOME}/Documents/Docs_work.sparsebundle

cd ${HOME}/Documents
echo "*** Backing up Documents: RDC Together Timesheets"
tar zcf Documents.tgz ./"RDC Connections" ./Together ./Timesheets
mv Documents.tgz ${DESTINATION}

echo
echo "######"
echo "#     #   ####   #    #  ######"
echo "#     #  #    #  ##   #  #"
echo "#     #  #    #  # #  #  #####"
echo "#     #  #    #  #  # #  #"
echo "#     #  #    #  #   ##  #"
echo "######    ####   #    #  ######"
echo
