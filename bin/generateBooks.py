#!/usr/bin/env python
# -*- coding: utf-8
import glob
import os

import markdown


class Organizer(object):
    pass


quick_start = Organizer()
quick_start.html_file = "downloads/quick-start.html"
quick_start.md_file = "downloads/quick-start.markdown"
quick_start.mobi_file = "downloads/quick-start.mobi"
quick_start.chapters = [
    "quick-start"
    ]

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
    "tutorials/processes"
    ]


docs = [quick_start, user_guide, process_tutorial]
#docs = [process_tutorial]
delimiter = "---"


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


def is_heading(key):
    if (key.startswith("#") and
        not key.startswith("#B(") and
        not key.startswith("#(") and
        not key.startswith("#Fun")):
        return True
    return False


def is_seen(key, seen):
    if seen.intersection([key]):
        return True
    return False


def remove_extra_headings(chapter):
    """
    Several chapters have their headings listed more than once (due to multiple
    markdown docs). This function removes all but the first one.
    """
    sections = []
    seen = set()
    for section in chapter:
        filtered_section = []
        for line in section.splitlines():
            key = line.strip()
            if is_heading(key):
                if not is_seen(key, seen):
                    filtered_section.append(line)
                    seen.add(key)
            else:
                filtered_section.append(line)
        sections.append("\n".join(filtered_section))
    return sections


def assemble_chapters(doc, remove_front_matter=True):
    chapters = []
    for chapter_location in doc.chapters:
        chapter = build_chapter(chapter_location)
        if remove_front_matter:
            chapter = filter_front_matter(chapter)
        chapter = remove_extra_headings(chapter)
        chapters.extend(chapter)
    return chapters


def assemble_book(doc, remove_front_matter=True):
    chapters = [delimiter, "layout: book", delimiter] + assemble_chapters(
        doc, remove_front_matter)
    book = "\n".join(chapters)
    return book.encode("utf-8")


def generate_doc(doc):
    markdown_data = assemble_book(doc)
    write_file(doc.md_file, markdown_data)


def generate_docs():
    for doc in docs:
        generate_doc(doc)


if __name__ == "__main__":
    generate_docs()