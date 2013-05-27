import os
import uuid
import zipfile

from scriptlib import config, const


def get_book_name(html_file):
    return os.path.splitext(os.path.basename(html_file))[0]


def get_config_name(path):
    return get_book_name(path).replace('-', '_')


def get_config(path):
    return getattr(config, get_config_name(path))


def create_mimetype(path):
    with open(os.path.join(path, const.mimetype_file), "w") as fh:
        fh.write(const.mimetype_epub)


def create_container(path):
    with open(os.path.join(path, const.container_file), "w") as fh:
        fh.write(const.container_xml)


def get_authors(book_config):
    xml = []
    for author in book_config.authors:
        xml.append(const.opf_author % author)
    return "\n".join(xml)


def get_opf_metadata(book_config):
    return const.opf_metadata % {
        "uuid": uuid.uuid4(),
        "lang": const.lang,
        "title": book_config.title,
        "authors": get_authors(book_config),
        "generator": const.generator,
        "year": book_config.publication_year,
        "coverid": const.coverid}


def get_opf_manifest(book_config, html_file):
    data = [
        # main page
        const.opf_manifest_html % {
            "id": "html_1",
            "mime": const.mimetype_html,
            "filename": os.path.basename(html_file)},
        # book cover
        const.opf_manifest_html % {
            "id": const.coverid,
            "mime": const.mimetype_jpg,
            "filename": const.cover},
        ]
    return "\n".join(data)


def get_opf_spine(book_config):
    return ""


def get_opf_guide(book_config):
    return ""


def build_skeleton(path, html_file):
    metainf_dir = os.path.join(path, const.metainf_dir)
    oebps_dir = os.path.join(path, const.oebps_dir)
    for dir in [metainf_dir, oebps_dir]:
        if not os.path.exists(dir):
            os.makedirs(dir)
    create_mimetype(path)
    create_container(path)


def create_content_opf(path, html_file):
    book_config = get_config(path)
    data = const.content_opf_xml % {
        "metadata": get_opf_metadata(book_config),
        "manifest": get_opf_manifest(book_config, html_file),
        "spine": get_opf_spine(book_config),
        "guide": get_opf_guide(book_config)}
    with open(os.path.join(path, const.content_opf_file), "w") as fh:
        fh.write(data)


def get_file_path(dst_path, filename):
    return os.path.join(dst_path, const.oebps_dir, filename)


def copy_file(src, dst):
    with open(src) as fhr:
        with open(dst, "w") as fhw:
            fhw.write(fhr.read())


def create_archive(archive_path, files):
    archive_name = archive_path + const.extension
    with zipfile.ZipFile(archive_name, 'w') as fh:
        cwd = os.getcwd()
        os.chdir(archive_path)
        fh.write(const.mimetype_file, compress_type=zipfile.ZIP_STORED)
        file_list = []
        file_list.append(const.container_file)
        file_list.append(const.content_opf_file)
        relative_files = [
            os.path.join(const.oebps_dir, os.path.basename(x)) for x in files]
        file_list.extend(relative_files)
        for file_path in file_list:
            fh.write(file_path, compress_type=zipfile.ZIP_DEFLATED)
        os.chdir(cwd)


def generate_epub(archive_path, src_html, clean_up=True):
    book_name = get_book_name(src_html)
    dst_path = os.path.join(archive_path, book_name)
    build_skeleton(dst_path, src_html)
    # add main html file
    dst_html = get_file_path(dst_path, "1.html")
    copy_file(src=src_html, dst=dst_html)
    # add cover image
    dst_cover = get_file_path(dst_path, const.cover)
    copy_file(src=const.cover_src, dst=dst_cover)
    files = [dst_html, dst_cover]
    create_content_opf(dst_path, dst_html)
    create_archive(dst_path, files)