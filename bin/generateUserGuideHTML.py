import glob
import os

import markdown


class Organizer(object):
    pass


user_guide = Organizer()
user_guide.output_file = "downloads/user-guide.html"
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
process_tutorial.output_file = "downloads/processes-tutorial.html"
process_tutorial.chapters = [
    "tutorials"
    ]


docs = [user_guide, process_tutorial]


def read_markdown(filename):
    with open(filename) as fh:
        data = fh.read().decode("utf-8")
        return markdown.markdown(data)


def scan_dir(path):
    markdowns = []
    for filename in glob.glob("%s/*.markdown" % path):
        markdowns.append(read_markdown(filename))
    return markdowns


def build_chapter(path):
    chapter = []
    if os.path.isfile(path):
        chapter.append(read_markdown(path))
    elif os.path.isdir(path):
        chapter.extend(scan_dir(path))
    return chapter


def assemble_chapters(doc):
    chapters = []
    for chapter in doc.chapters:
        chapters.extend(build_chapter(chapter))
    return chapters



def create_html_file(filename, data):
    output = "\n".join(data).encode("utf-8")
    with open(filename, "w") as fh:
        fh.write(output)


def create_mobi_file(filename):
    pass


def regenerate_doc(doc):
    data = assemble_chapters(doc)
    create_html_file(doc.output_file, data)
    create_mobi_file(doc.output_file)


def regenerate_docs():
    for doc in docs:
        regenerate_doc(doc)


if __name__ == "__main__":
    regenerate_docs()
