delimiter = "---"
contributors = """
John Daily
Mats Westin
Tim Fletcher
"""

# EPub constants
extension = ".epub"
mimetype_epub = 'application/epub+zip'
mimetype_html = 'application/xhtml+xml'
mimetype_opf = 'application/oebps-package+xml'
mimetype_ncx = 'application/x-dtbncx+xml'
mimetype_js = 'text/javascript'
mimetype_css = 'text/css'
mimetype_jpg = 'image/jpeg'
metainf_dir = "META-INF"
container_file = "%s/container.xml" % metainf_dir
mimetype_file = "mimetype"
oebps_dir = "OEBPS"
content_opf_file = "%s/content.opf" % oebps_dir
cover = "cover.jpg"
cover_src = "images/logos/LispFlavoredErlang-large-cover.jpg"
style = "%s/style.css" % oebps_dir
title_page = "%s/title.html" % oebps_dir
copyright_page = "%s/copyright.html" % oebps_dir
toc_page = "%s/toc.html" % oebps_dir
toc_ncx = "%s/toc.ncx" % oebps_dir
book_page = "%s/book.html" % oebps_dir
lang = "en-US"
generator = "lfe.github.io (custom scripts)"
coverid = "image_1"


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
  <opf:spine toc="ncxtoc">
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
      <br/>
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
      <p style="font-size: 150%%">
        We would like to thank the following individuals who have contributed to
        the text of this book, listed in alphabetical order by first name:
      </p>
      <p style="font-size: 150%%">%(contributors)s</p>
    </div>
  </body>
</html>
"""