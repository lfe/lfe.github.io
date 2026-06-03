#!/bin/bash

find . -name '*.png' -exec sips --resampleWidth 1600 {} \;
find . -name '*.png' -exec pngquant --quality=65-80 --force --ext .png {} \;
