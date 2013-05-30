#!/usr/bin/env python
# -*- coding: utf-8
import glob
import os

import markdown

from scriptlib import config, const


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
            if line == const.delimiter:
                counts += 1
        sections.append("\n".join(lines[start:]))
    return sections


def is_heading(key):
    if (key.startswith("#") and
        not key.startswith("#B(") and
        not key.startswith("#b(") and
        not key.startswith("#(") and
        not key.startswith("#Fun")):
        return True
    return False


def get_anchor_name(heading):
    return heading.replace(
        '#', '').strip().replace(
        '.', '').replace(
        ' ', '_').replace(
        "'", '').replace(
        '`', '').lower()


def get_anchor(heading):
    return '<a name="%s"></a>' % get_anchor_name(heading)


def is_seen(key, seen):
    if seen.intersection([key]):
        return True
    return False


def remove_extra_headings(chapter, headings_only=False, include_anchors=True,
                          as_string=True):
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
                    if include_anchors:
                        filtered_section.append(get_anchor(line))
                    filtered_section.append(line)
                    seen.add(key)
            elif not headings_only:
                filtered_section.append(line)
        if as_string:
            sections.append("\n".join(filtered_section))
        else:
            sections.extend(filtered_section)
    return sections


def assemble_headings(book_config):
    headings = []
    for chapter_location in book_config.chapters:
        chapter = build_chapter(chapter_location)
        headings.extend(
            remove_extra_headings(
                chapter, headings_only=True, include_anchors=False,
                as_string=False))
    return headings


def assemble_chapters(book_config, remove_front_matter=True):
    chapters = []
    for chapter_location in book_config.chapters:
        chapter = build_chapter(chapter_location)
        if remove_front_matter:
            chapter = filter_front_matter(chapter)
        chapter = remove_extra_headings(chapter)
        chapters.extend(chapter)
    return chapters


def assemble_book(book_config, remove_front_matter=True):
    chapters = [const.delimiter,
                "layout: book",
                "title: %s" % book_config.title,
                "author: %s" % ", ".join(book_config.authors),
                const.delimiter] + assemble_chapters(
                    book_config, remove_front_matter)
    book = "\n".join(chapters)
    return book.encode("utf-8")


def generate_doc(book_config):
    markdown_data = assemble_book(book_config)
    write_file(book_config.md_file, markdown_data)


def generate_docs():
    for book_config in config.docs:
        generate_doc(book_config)