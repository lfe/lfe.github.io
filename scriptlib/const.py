# -*- coding: utf-8
delimiter = "---"
contributors = """
John Daily
Mats Westin
Tim Fletcher
"""
font = "fonts/Philosopher.ttf"
font_italic = "fonts/Philosopher-Italic.ttf"


# EPub constants
extension = ".epub"
mimetype_epub = 'application/epub+zip'
mimetype_html = 'application/xhtml+xml'
mimetype_opf = 'application/oebps-package+xml'
mimetype_ncx = 'application/x-dtbncx+xml'
mimetype_js = 'text/javascript'
mimetype_css = 'text/css'
mimetype_jpg = 'image/jpeg'
mimetype_png = 'image/png'
metainf_dir = "META-INF"
container_file = "%s/container.xml" % metainf_dir
mimetype_file = "mimetype"
oebps_dir = "OEBPS"
content_opf_file = "%s/content.opf" % oebps_dir
cover_src = "images/logos/LispFlavoredErlang-large-cover.png"
style = "%s/style.css" % oebps_dir
title_page = "%s/title.html" % oebps_dir
copyright_page = "%s/copyright.html" % oebps_dir
toc_page = "%s/toc.html" % oebps_dir
toc_ncx = "%s/toc.ncx" % oebps_dir
book_page = "%s/book.html" % oebps_dir
lang = "en-US"
generator = "lfe.github.io (custom scripts)"
toc_depth = 4
toc_indent = "&nbsp;&nbsp;"


# EPub XML fragments
container_xml = """
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<container xmlns="urn:oasis:names:tc:opendocument:xmlns:container"
           version="1.0">
  <rootfiles>
    <rootfile full-path="%s" media-type="%s"/>
  </rootfiles>
</container>
""" % (content_opf_file, mimetype_opf)
opf_author = """
    <dc:creator opf:role="aut">%s</dc:creator>
"""
opf_metadata = """
    <dc:identifier id="bookid">urn:uuid:%(uuid)s</dc:identifier>
    <dc:language>%(lang)s</dc:language>
    <dc:title>%(title)s</dc:title>
%(authors)s
    <dc:contributor opf:role="bkp">%(generator)s</dc:contributor>
    <dc:date opf:event="publication">%(year)s</dc:date>
    <opf:meta name="cover" content="%(coverid)s"/>
"""
opf_manifest_html = """
    <opf:item id="%(id)s" media-type="%(mime)s" href="%(filename)s"/>
"""
opf_spine_html = """
<opf:itemref idref="%(idref)s" linear="%(isprimary)s"/>
"""
opf_guide_html = """
    <opf:reference href="%(htmlpage)s" type="%(type)s" title="%(title)s"/>
"""
content_opf_xml = """
<?xml version="1.0" encoding="utf-8" standalone="no"?>
<opf:package
    xmlns:opf="http://www.idpf.org/2007/opf"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    unique-identifier="bookid" version="2.0">
  <opf:metadata>
%(metadata)s
  </opf:metadata>
  <opf:manifest>
%(manifest)s
  </opf:manifest>
  <opf:spine toc="%(ncxid)s">
%(spine)s
  </opf:spine>
  <opf:guide>
%(guide)s
  </opf:guide>
</opf:package>
"""

# HTML fragments
author_html = """
      <p style="font-size: 150%%">%s</p>
"""
title_page_html = """
<html>
  <head>
    <title>%(title)s</title>
  </head>
  <body>
    <div style="text-align: center">
      <br/><br/>
      <h1>%(title)s</h1>
      <br/><br/>
      <h2><i>%(subtitle)s</i></h2>
      <br/><br/><br/><br/>
%(authors)s
    </div>
  </body>
</html>
"""
dedication_page_html = """
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<html>
  <head>
    <title>%(title)s</title>
  </head>
  <body>
    <div style="text-align: center">
      <br/><br/>
      <br/><br/>
      <br/><br/>
      <i>This work is dedicated to</i>
      <br/><br/>
      John McCarthy
      <br/><br/>
      <i>those that came before him:</i>
      <br/><br/>
      Leibniz, Frege, Peano, B. Russell, Wittgenstein, Church, von Neumann, and
      Gödel
      <br/><br/>
      <i>those that came after him:</i>
      <br/><br/>
      S. Russell, Hart, Levin, Hewitt, Steele, Sussman, Armstrong, Virding, and
      Williams
      <br/><br/>
      <i>and to all for whom the love of learning and adventure compells them
      to explore and create.</i>
      <br/><br/>
    </div>
  </body>
</html>
"""
acknowledgements_page_html = """
<html>
  <head>
    <title>%(title)s</title>
  </head>
  <body>
    <div style="text-align: center">
      <br/><br/>
      <h1>%(title)s</h1>
      <br/><br/>
      <p>
        We would like to thank the following individuals who have contributed to
        the text of this book, listed in alphabetical order by first name:
      </p>
      <p>%(contributors)s</p>
    </div>
  </body>
</html>
"""
copyright_page_html = """
<html>
  <body>
    <div style="text-align: center">
      <br/><br/>
      <p>
        Editors <i>The LFE Community</i>
      </p>
      <p>
        Email lisp-flavoured-erlang@googlegroups.com
        <br/>
        Web site http://lfe.github.io/
      </p>
      <p>
        &copy; 2013, Duncan McGreggor
        <br/>
        All rights reserved.
        <br/>
        Printed wherever one can access the internet and download the code.
      </p>
      <p>
        This work is licensed under a Creative Commons
        <br/>
        Attribution-NonCommercial-ShareAlike 3.0 Unported License.
      </p>
      <p>
        This work may be copied, distributed, transmitted, and adapted under
        <br/>
        the conditions that attribution is given and it is not done for commercial
        <br/>
        purposes. Any alterations, transformations, or derivations of this work
        <br/>
        may be distributed under the same license.
      </p>
      <p>
        McGreggor, Duncan.
        <br/>
        %(title)s: %(subtitle)s / Duncan McGreggor.
      </p>
      <p>
        1. Electronic digital computers -- Programming. 2. Erlang
        <br/>
        (Computer program language) 3. Lisp  (Computer program language). I. Title.
      </p>
    </div>
  </body>
</html>
"""
toc_entry = """
    <div style="text-indent: %(indent)sem;">
      <a href="#%(anchor)s">%(linktext)s</a>
    </div>
"""
toc_entry_with_file = """
    <div style="text-indent: %(indent)sem;">
      <a href="%(filename)s#%(anchor)s">%(linktext)s</a>
    </div>
"""
toc_page_html = """
<?xml version="1.0" encoding="utf-8" standalone="no"?>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>%(title)s</title>
  </head>
  <body>
    <h1>Table of Contents</h1>
%(tocentries)s
  </body>
</html>
"""
ncx_toc_entry = """
<navPoint id="%(id)s" playOrder="%(playorder)s">
<navLabel><text>%(indent)s%(linktext)s</text></navLabel>
<content src="%(filename)s#%(anchor)s"/>
</navPoint>
"""
ncx_page = """
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">
  <head>
    <meta name="dtb:uid" content="urn:uuid:%(uuid)s"/>
    <meta name="dtb:depth" content="%(depth)s"/>
    <meta name="dtb:totalPageCount" content="0"/>
    <meta name="dtb:maxPageNumber" content="0"/>
  </head>
  <docTitle>
    <text>%(title)s</text>
  </docTitle>
  <navMap>
%(tocentries)s
  </navMap>
</ncx>
"""