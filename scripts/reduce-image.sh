#!/bin/bash

sips --resampleWidth 1600 $1
pngquant --quality=65-80 --force --ext .png $1

