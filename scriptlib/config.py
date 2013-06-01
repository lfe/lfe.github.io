from scriptlib import const


class Organizer(object):
    pass


class BookStructure(Organizer):
    # cover
    cover = Organizer()
    cover.id = "cover"
    cover.filename = "cover.png"
    cover.mimetype = const.mimetype_png
    # title page
    title = Organizer()
    title.id = "title"
    title.filename = "title.html"
    title.guide_type = "title-page"
    title.name = "Title Page"
    title.mimetype = const.mimetype_html
    # copyright page
    copyright = Organizer()
    copyright.id = "copyright"
    copyright.filename = "copyright.html"
    copyright.mimetype = const.mimetype_html
    # acknowledgements page
    ack = Organizer()
    ack.id = "acknowledgements"
    ack.filename = "acknowledgements.html"
    ack.mimetype = const.mimetype_html
    # dedication
    dedication = Organizer()
    dedication.id = "dedication"
    dedication.filename = "dedication.html"
    dedication.mimetype = const.mimetype_html
    # embedded toc
    html_toc = Organizer()
    html_toc.id = "toc_1"
    html_toc.filename = "toc.html"
    html_toc.guide_type = "toc"
    html_toc.name = "Table of Contents"
    html_toc.mimetype = const.mimetype_html
    # ncx toc
    ncx_toc = Organizer()
    ncx_toc.id = "toc_2"
    ncx_toc.filename = "toc.ncx"
    ncx_toc.name = "Table of Contents"
    ncx_toc.mimetype = const.mimetype_ncx
    # main doc
    main = Organizer()
    main.id = "html_1"
    main.filename = "1.html"
    main.mimetype = const.mimetype_html
    # all together now...
    guide_components = [title, html_toc]
    all_components = [cover, title, copyright, dedication, ack, html_toc,
                      ncx_toc, main]


struct = BookStructure()


class BookConfig(Organizer):
    cover = "images/logos/LispFlavoredErlang-large-cover.jpg"
    publication_year = "2013"


quick_start = BookConfig()
quick_start.title = "LFE Quick Start"
quick_start.subtitle = "Lisp Flavored Erlang for the Impatient"
quick_start.authors = ["Duncan McGreggor"]
quick_start.name = "quick-start"
quick_start.filename = "%s.html" % quick_start.name
quick_start.html_file = "downloads/%s" % quick_start.filename
quick_start.md_file = "downloads/%s.markdown" % quick_start.name
quick_start.mobi_file = "downloads/%s.mobi" % quick_start.name
quick_start.chapters = [
    "quick-start"
    ]


user_guide = BookConfig()
user_guide.title = "LFE User Guide"
user_guide.subtitle = "A Comprehensive Introduction to Lisp Flavored Erlang"
user_guide.authors = ["Duncan McGreggor", "Robert Virding"]
user_guide.name = "user-guide"
user_guide.filename = "%s.html" % user_guide.name
user_guide.html_file = "downloads/%s" % user_guide.filename
user_guide.md_file = "downloads/%s.markdown" % user_guide.name
user_guide.mobi_file = "downloads/%s.mobi" % user_guide.name
user_guide.chapters = [
    "user-guide/intro",
    "user-guide/diving",
    "user-guide/data",
    "user-guide/funcode",
    "user-guide/recursion",
    "user-guide/check",
    "user-guide/1.markdown",
    "user-guide/2.markdown",
    "user-guide/extra",
    ]


processes_tutorial = BookConfig()
processes_tutorial.title = "Lightweight Processes"
processes_tutorial.subtitle = "A Lisp Flavored Erlang Tutorial"
processes_tutorial.authors = ["Duncan McGreggor"]
processes_tutorial.name = "processes-tutorial"
processes_tutorial.filename = "%s.html" % processes_tutorial.name
processes_tutorial.html_file = "downloads/%s" % processes_tutorial.filename
processes_tutorial.md_file = "downloads/%s.markdown" % processes_tutorial.name
processes_tutorial.mobi_file = "downloads/%s.mobi" % processes_tutorial.name
processes_tutorial.chapters = [
    "tutorials/processes"
    ]


docs = [quick_start, user_guide, processes_tutorial]


def get_book_names():
    return " ".join([x.name for x in docs])