#!/usr/bin/env python
import glob
import os

import markdown


class Organizer(object):
    pass


user_guide = Organizer()
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
process_tutorial.html_file = "downloads/processes-tutorial.html"
process_tutorial.md_file = "downloads/processes-tutorial.markdown"
process_tutorial.mobi_file = "downloads/processes-tutorial.mobi"
process_tutorial.chapters = [
    "tutorials"
    ]


docs = [user_guide, process_tutorial]


def read_file(filename):
    with open(filename) as fh:
        return fh.read().decode("utf-8")


def write_file(filename, data):
    with open(filename, "w") as fh:
        fh.write(data)


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


def filter_front_matter(chapter):
    """
    Scan each chapter for Jekyll annotations at the beginning of each section
    and remove them.
    """
    delimiter = "---"
    sections = []
    for section in chapter:
        counts = 0
        start = None
        lines = section.splitlines()
        for index, line in enumerate(lines):
            if counts == 2:
                start = index
                break
            if line == delimiter:
                counts += 1
        sections.append("\n".join(lines[start:]))
    return sections


def assemble_chapters(doc, remove_front_matter=True):
    chapters = []
    for chapter_location in doc.chapters:
        chapter = build_chapter(chapter_location)
        if remove_front_matter:
            chapter = filter_front_matter(chapter)
        chapters.extend(chapter)
    return chapters


def assemble_book(doc, remove_front_matter=True):
    chapters = ["---", "layout: book", "---"] + assemble_chapters(
        doc, remove_front_matter)
    book = "\n".join(chapters)
    return book.encode("utf-8")


def create_markdown_file(filename, markdown_data):
    write_file(filename, markdown_data)


def create_mobi_file(html_file, mobi_file):
    pass


def generate_doc(doc):
    markdown_data = assemble_book(doc)
    create_markdown_file(doc.md_file, markdown_data)
    create_mobi_file(doc.html_file, doc.mobi_file)


def generate_docs():
    for doc in docs:
        generate_doc(doc)


if __name__ == "__main__":
    generate_docs()