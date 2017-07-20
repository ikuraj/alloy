#!/bin/bash

BUILD_DATE=$(date +"%F %H:%M %Z")
VERSION="4.2_$(date +"%F")"

CP_SEP=":"

if [[ ! -z $(uname -o | grep -i "Cygwin") ]]
then
    CP_SEP=";"
fi

PATH=$JDK6/bin:$PATH

function fade_start { echo -ne "$(tput setaf 8)"; }
function fade_end   { echo -ne "$(tput sgr0)"; }

function step  { echo -e "$(tput setaf 12; tput bold)[$@...]$(tput sgr0)"; }
function info  { echo -e "$(tput setaf 4)$@$(tput sgr0)"; }
function emph  { echo -e "$(tput setaf 10; tput bold)$@$(tput sgr0)"; }
function warn  { echo -e "$(tput setaf 3)  [Warn] $@$(tput sgr0)"; }
function error { echo -e "$(tput setaf 9; tput bold)  [ERROR]: $@$(tput sgr0)"; }
function trace { echo -e "$(tput setaf 7)$@$(tput sgr0)"; }
function fatal { error $@; fade_end; exit 1; }

function compile {
    version_file=src/edu/mit/csail/sdg/alloy4/Version.java
    cp -r $version_file $version_file.bak
    sed -i \
      -e 's/public static String buildDate.*/public static String buildDate() { return "'"$BUILD_DATE"'"; }/' \
      -e 's/public static String version.*/public static String version() { return "'"$VERSION"'"; }/' $version_file

    mkdir -p bin

    info "cleaning up the bin folder"
    rm -rf bin/*

    step "compiling"
    CP=$(ls -1 lib/*.jar | xargs | sed 's/\ /'$CP_SEP'/g')
    trace "  using CLASSPATH: $CP"
    find src -name "*.java" | xargs javac -cp $CP -d bin # -source 1.5 -target 1.5
    ok="$?"

    mv $version_file.bak $version_file

    if [[ $ok != "0" ]]; then fatal "Could not compile from sources"; fi

    fade_end
}

function dist {
    DST=dist
    MACOSDST=Alloy-OSX

    rm -rf $DST/*
    mkdir -p $DST/alloy

    step "building JAR distribution"

    for f in lib/*jar
    do
        trace "  extracting: $f"
	unzip -q -o $f -d $DST/alloy
    done

    # copy the content of the extra folder
    cp -r extra/* $DST/alloy

    rm -rf bin/tmp
    cp -r bin/* $DST/alloy/
    cp -r src/* $DST/alloy/
    rm -rf $DST/alloy/META-INF

    find $DST/alloy -type d -name ".git" -exec rm -rf {} \;
    find $DST/alloy -type d -name ".svn" -exec rm -rf {} \;
    find $DST/alloy -type d -name "CVS" -exec rm -rf {} \;

    mkdir -p $DST/alloy/META-INF
    cp MANIFEST.MF $DST/alloy/META-INF

    pushd $(pwd) &> /dev/null
    cd $DST/alloy
    jarName="alloy$VERSION.jar"
    zip -q -r $jarName *
    chmod +x $jarName
    mv $jarName ../
    cd ..
    rm -rf allo
    popd &> /dev/null

    emph " *** jar file created:    $DST/$jarName"
    fade_end
}

if [[ "X"$1 == "X" ]]
then
  compile
  dist
else
  $1
fi
