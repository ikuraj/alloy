#!/bin/bash

hdiutil create tmp.dmg -ov -volname "Alloy4.2-rc" -fs HFS+ -srcfolder dist
hdiutil convert tmp.dmg -format UDZO -o alloy4.2-rc.dmg

rm tmp.dmg
