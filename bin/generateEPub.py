#!/usr/bin/env python
import argparse

from scriptlib import epub


parser = argparse.ArgumentParser(
    description="Create an .epub from a single-page .html file.")
parser.add_argument('--html', help='path to HTML file to convert')
parser.add_argument('-a', '--archive-path', help='path to HTML file to convert')
args = parser.parse_args()


epub.generate_epub(args.archive_path, src_html=args.html)