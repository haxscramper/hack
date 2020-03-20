#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash

set -o nounset
set -o errexit

rm -rf /tmp/qt-lib-test
cp -r /mnt/workspace/clean-clone/qiucus-development/source/haxscamper-misc/cpp/fuzzy_select/ /tmp/qt-lib-test

cd /tmp/qt-lib-test

package-orchestrate create \
                    --name:fuzzywidget \
                    --version:0.1 \
                    --type:library \
                    --buildsystem:qmake \
                    --author:demo \
                    --channel:testing \
                    --lang:cpp \
                    --no-example

package-orchestrate build
package-orchestrate publish --password:demo
