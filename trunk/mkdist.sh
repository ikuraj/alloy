#!/bin/bash

BUILD_DATE=$(date +"%F %H:%M %Z")
VERSION="4.2"

if [[ -z $KODKOD_HOME ]]
then
    KODKOD_HOME=../kodkod  #../relations-experimental
fi

function compile {
    version_file=src/edu/mit/csail/sdg/alloy4/Version.java
    cp -r $version_file $version_file.bak
    sed -i \
      -e 's/public static String buildDate.*/public static String buildDate() { return "'"$BUILD_DATE"'"; }/' \
      -e 's/public static String version.*/public static String version() { return "'"$VERSION"'"; }/' $version_file

    echo "[cleaning up the bin folder...]"
    rm -rf bin/*

    CP=$KODKOD_HOME/bin:$(ls -1 lib/*.jar | xargs | sed 's/\ /:/g')
    echo "[compiling...]"
    find src -name "*.java" | xargs javac -cp $CP -d bin -target 1.5

    mv $version_file.bak $version_file
}

function dist {
    DST=dist
    MACOSDST=Alloy-OSX

    rm -rf $DST/*
    mkdir -p $DST/alloy

    for f in lib/*jar
    do
	unzip -q -o $f -d $DST/alloy
    done
    
    rm -rf bin/tmp
    cp -r bin/* $DST/alloy/
    cp -r src/* $DST/alloy/
    rm -rf $DST/alloy/kodkod
    cp -r $KODKOD_HOME/bin/kodkod $DST/alloy/kodkod
    cp -r $KODKOD_HOME/src/kodkod/* $DST/alloy/kodkod/
    rm -rf $DST/alloy/META-INF
 
    find $DST/alloy -type d -name ".svn" | xargs rm -rf 
    find $DST/alloy -type d -name "CVS" | xargs rm -rf 
    
    mkdir -p $DST/alloy/META-INF
    cp MANIFEST.MF $DST/alloy/META-INF

    # DEPRECATED    
    # for d in META-INF # amd64-linux  help  icons  images  LICENSES  META-INF  models  README.TXT  x86-freebsd  x86-linux  x86-mac  x86-windows
    # do
    # 	cp -r template/$d alloy/
    # done

    pushd $(pwd) &> /dev/null
    cd $DST/alloy
    # find -type f -name "*.java" | xargs rm -f
    zip -q -r alloy-dev.jar *
    chmod +x alloy-dev.jar
    mv alloy-dev.jar ../
    cd ..
    rm -rf allo
    popd &> /dev/null

    echo " *** jar file created:    $DST/alloy-dev.jar"

    echo "[building OSX app...]"
    ant

    echo "[packaging OSX...]"
    osxdir="alloy-osx"
    rm -rf $DST/$osxdir
    mkdir -p $DST/$osxdir/dist
    cp -r $DST/*.app $DST/$osxdir/dist
    cp -r OSX-extra/* $DST/$osxdir/dist
    cp build-dmg.sh $DST/$osxdir/
    cd $DST
    zip -q -r $osxdir.zip $osxdir
    rm -rf $osxdir
    

    # cp $MACOSDST.zip $DST
    # pushd $(pwd) &> /dev/null
    # unzip -q $MACOSDST.zip
    # rm $MACOSDST.zip
    # cd $MACOSDST
    # echo "[copying JAR file to OSX dist folder...]"
    # cp -f ../alloy-dev.jar dist/alloy4.2-rc.app/Contents/Resources/Java/alloy-dev.jar 
    # cd ..
    # echo "[zipping the OSX dist folder...]"
    # zip -q -r $MACOSDST.zip $MACOSDST
    # rm -rf $MACOSDST
    # popd &> /dev/null

    # echo " *** osx bundle created:  $DST/$MACOSDST.zip"
}

if [[ "X"$1 == "X" ]]
then
  compile 
  dist
else
  $1
fi

# echo '#!/bin/bash
    
# CP=classes:$(ls -1 lib/*.jar | xargs | sed "s/\ /:/g")
# java -Xms512m -Xmx2048m -ea -cp $CP edu.mit.csail.sdg.alloy4whole.SimpleGUI
# ' > $DST/alloy/run-alloy.sh
    
# chmod +x $DST/alloy/run-alloy.sh
    
