import os
import uuid
import zipfile

from PIL import Image, ImageDraw, ImageFont

from scriptlib import config, const, md


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


def get_opf_authors(book_config):
    xml = []
    for author in book_config.authors:
        xml.append(const.opf_author % author)
    return "\n".join(xml)


def get_opf_metadata(book_config):
    return const.opf_metadata % {
        "uuid": uuid.uuid4(),
        "lang": const.lang,
        "title": book_config.title,
        "authors": get_opf_authors(book_config),
        "generator": const.generator,
        "year": book_config.publication_year,
        "coverid": const.coverid}


def get_opf_manifest(book_config):
    data = [
        # book cover
        const.opf_manifest_html % {
            "id": const.coverid,
            "mime": const.mimetype_png,
            "filename": const.cover},
        # title page
        const.opf_manifest_html % {
            "id": "html_1",
            "mime": const.mimetype_html,
            "filename": "1.html"},
        # copyright page
        const.opf_manifest_html % {
            "id": "html_2",
            "mime": const.mimetype_html,
            "filename": "2.html"},
        # acknowledgements page
        const.opf_manifest_html % {
            "id": "html_3",
            "mime": const.mimetype_html,
            "filename": "3.html"},
        # embedded toc
        const.opf_manifest_html % {
            "id": "html_4",
            "mime": const.mimetype_html,
            "filename": "toc.html"},
        # main page
        const.opf_manifest_html % {
            "id": "html_5",
            "mime": const.mimetype_html,
            "filename": "4.html"},
        ]
    return "\n".join(data)


def get_opf_spine(book_config):
    return ""


def get_opf_guide(book_config):
    data = [
        # title page
        const.opf_guide_html % {
            "htmlpage": "1.html",
            "type": "title-page",
            "title": "Title Page"},
        ]
    return "\n".join(data)


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
        "manifest": get_opf_manifest(book_config),
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

def create_cover_image(path, src, dst):
    book_config = get_config(path)
    img = Image.open(src)
    draw = ImageDraw.Draw(img)
    title_font = ImageFont.truetype(const.font, 200)
    author_font = ImageFont.truetype(const.font_italic, 80)
    draw.text((110, 150), book_config.title, (127,127,127), font=title_font)
    draw.text((105, 145), book_config.title, (255,255,255), font=title_font)
    draw.text((105, 390), book_config.subtitle, (255,255,255), font=author_font)
    location = 2750
    for author in book_config.authors:
        draw.text((105, location), author, (255,255,255), font=author_font)
        location += 120
    img.save(dst)

def get_html_authors(book_config):
    html = []
    for author in book_config.authors:
        html.append(const.author_html % author)
    return "\n".join(html)


def create_title_page(path, dst):
    book_config = get_config(path)
    data = const.title_page_html % {
        "title": book_config.title,
        "subtitle": book_config.subtitle,
        "authors": get_html_authors(book_config)}
    with open(dst, "w") as fh:
        fh.write(data)


def create_copyright_page(path, dst):
    book_config = get_config(path)
    data = const.copyright_page_html % {
        "title": book_config.title,
        "subtitle": book_config.subtitle}
    with open(dst, "w") as fh:
        fh.write(data)


def create_acks_page(dst):
    data = const.acknowledgements_page_html % {
        "title": "Acknowledgements",
        "contributors": ", ".join(const.contributors.strip().splitlines())}
    with open(dst, "w") as fh:
        fh.write(data)


def create_css_file(dst):
    with open(dst, "w") as fh:
        fh.write(const.css)


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
    # add cover image
    dst_cover = get_file_path(dst_path, const.cover)
    create_cover_image(path=dst_path, src=const.cover_src, dst=dst_cover)
    # add title page
    dst_title = get_file_path(dst_path, "1.html")
    create_title_page(path=dst_path, dst=dst_title)
    # add copyright page
    dst_copyright = get_file_path(dst_path, "2.html")
    create_copyright_page(path=dst_path, dst=dst_copyright)
    # add acknowledgements page
    dst_acks = get_file_path(dst_path, "3.html")
    create_acks_page(dst=dst_acks)
    # add embedded toc
    dst_toc = get_file_path(dst_path, "toc.html")
    create_toc_page(dst=dst_toc)
    # add main html file
    dst_html = get_file_path(dst_path, "4.html")
    copy_file(src=src_html, dst=dst_html)
    # assumble epub files
    files = [dst_cover, dst_title, dst_copyright, dst_acks, dst_html]
    create_content_opf(dst_path, dst_html)
    create_archive(dst_path, files)