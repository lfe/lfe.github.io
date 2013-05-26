class Organizer(object):
    author = "Duncan McGreggor"
    cover = "images/logos/LispFlavoredErlang-large-cover.jpg"


quick_start = Organizer()
quick_start.title = "LFE Quick Start"
quick_start.html_file = "downloads/quick-start.html"
quick_start.md_file = "downloads/quick-start.markdown"
quick_start.mobi_file = "downloads/quick-start.mobi"
quick_start.chapters = [
    "quick-start"
    ]


user_guide = Organizer()
user_guide.title = "LFE User Guide"
user_guide.html_file = "downloads/user-guide.html"
user_guide.md_file = "downloads/user-guide.markdown"
user_guide.mobi_file = "downloads/user-guide.mobi"
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


process_tutorial = Organizer()
process_tutorial.title = "LFE Tutorial - Lightweight Processes"
process_tutorial.html_file = "downloads/processes-tutorial.html"
process_tutorial.md_file = "downloads/processes-tutorial.markdown"
process_tutorial.mobi_file = "downloads/processes-tutorial.mobi"
process_tutorial.chapters = [
    "tutorials/processes"
    ]


docs = [quick_start, user_guide, process_tutorial]