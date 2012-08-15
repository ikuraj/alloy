#!/bin/bash

hdiutil create tmp.dmg -ov -volname "Alloy4.2" -fs HFS+ -srcfolder dist
hdiutil convert tmp.dmg -format UDZO -o alloy4.2.dmg

rm tmp.dmg
