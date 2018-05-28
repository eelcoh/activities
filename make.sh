#!/bin/sh
elm make src/Main.elm --output=build/activities.js && cp src/index.html build
