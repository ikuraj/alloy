#!/bin/bash

DST=dist

rm -rf $DST/alloy*
mkdir -p $DST/alloy

for f in ls -1 lib/*jar
do
    unzip -o $f -d $DST/alloy
done

cp -r bin/* ../kodkod/bin/* $DST/alloy/

# echo '#!/bin/bash

# CP=classes:$(ls -1 lib/*.jar | xargs | sed "s/\ /:/g")
# java -Xms512m -Xmx2048m -ea -cp $CP edu.mit.csail.sdg.alloy4whole.SimpleGUI
# ' > $DST/alloy/run-alloy.sh

# chmod +x $DST/alloy/run-alloy.sh

cd $DST
rm -rf $DST/alloy/META-INF

for d in META-INF # amd64-linux  help  icons  images  LICENSES  META-INF  models  README.TXT  x86-freebsd  x86-linux  x86-mac  x86-windows
do
    cp -r template/$d alloy/
done

cd alloy
zip -r alloy-dev.jar *
chmod +x alloy-dev.jar
mv alloy-dev.jar ../
cd ..

cd ..
