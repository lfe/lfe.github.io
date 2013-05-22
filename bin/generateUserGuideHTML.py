import glob
import os

import markdown


class Organizer(object):
    pass


user_guide = Organizer()
user_guide.html_file = "downloads/user-guide.html"
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
process_tutorial.html_file = "downloads/processes-tutorial.html"
process_tutorial.mobi_file = "downloads/processes-tutorial.mobi"
process_tutorial.chapters = [
    "tutorials"
    ]


docs = [user_guide, process_tutorial]


# XXX don't do markdown until all files have been assembled
def read_file(filename):
    with open(filename) as fh:
        return fh.read().decode("utf-8")


def scan_dir(path):
    markdowns = []
    for filename in glob.glob("%s/*.markdown" % path):
        markdowns.append(read_file(filename))
    return markdowns


def build_chapter(path):
    chapter = []
    if os.path.isfile(path):
        chapter.append(read_file(path))
    elif os.path.isdir(path):
        chapter.extend(scan_dir(path))
    return chapter


def assemble_book(doc, remove_front_matter=True):
    chapters = []
    for chapter in doc.chapters:
        chapters.extend(build_chapter(chapter))
    if remove_front_matter:
        # scan each chapter for Jekyll annotations at the beginning and remove
        # them
        pass
    return chapters


def create_html_file(filename, data):
    html = markdown.markdown("\n".join(data)).encode("utf-8")
    with open(filename, "w") as fh:
        fh.write(html)


def create_mobi_file(html_file, mobi_file):
    pass


def generate_doc(doc):
    files_data = assemble_chapters(doc)
    create_html_file(doc.html_file, files_data)
    create_mobi_file(doc.html_file, doc.mobi_file)


def generate_docs():
    for doc in docs:
        generate_doc(doc)


if __name__ == "__main__":
    generate_docs()
