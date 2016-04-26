#!/bin/bash

VERSION=X

hdiutil create tmp.dmg -ov -volname "Alloy$VERSION" -fs HFS+ -srcfolder dist
hdiutil convert tmp.dmg -format UDZO -o alloy$VERSION.dmg

rm tmp.dmg
