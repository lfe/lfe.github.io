class Organizer(object):
    cover = "images/logos/LispFlavoredErlang-large-cover.jpg"
    publication_year = "2013"


quick_start = Organizer()
quick_start.title = "LFE Quick Start"
quick_start.authors = ["Duncan McGreggor"]
quick_start.name = "quick-start"
quick_start.filename = "%s.html" % quick_start.name
quick_start.html_file = "downloads/%s" % quick_start.filename
quick_start.md_file = "downloads/%s.markdown" % quick_start.name
quick_start.mobi_file = "downloads/%s.mobi" % quick_start.name
quick_start.chapters = [
    "quick-start"
    ]


user_guide = Organizer()
user_guide.title = "LFE User Guide"
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


processes_tutorial = Organizer()
processes_tutorial.title = "LFE Tutorial - Lightweight Processes"
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