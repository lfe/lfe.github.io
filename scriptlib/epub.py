import os
import uuid
import zipfile

from PIL import Image, ImageDraw, ImageFont

from scriptlib import config, const, md


content_uuid = uuid.uuid4()


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
        "uuid": content_uuid,
        "lang": const.lang,
        "title": book_config.title,
        "authors": get_opf_authors(book_config),
        "generator": const.generator,
        "year": book_config.publication_year,
        "coverid": config.struct.cover.id}


def get_opf_manifest(book_config):
    data = []
    for component in config.struct.all_components:
        component_data = const.opf_manifest_html % {
            "id": component.id,
            "mime": component.mimetype,
            "filename": component.filename}
        data.append(component_data)
    return "\n".join(data)


def get_opf_spine(book_config):
    data = []
    for component in config.struct.all_components:
        component_data = const.opf_spine_html % {
            "idref": component.id,
            "isprimary": "yes"}
    return "\n".join(data)


def get_opf_guide(book_config):
    data = []
    for component in config.struct.guide_components:
        component_data = const.opf_guide_html % {
            "htmlpage": component.filename,
            "type": component.guide_type,
            "title": component.name}
        data.append(component_data)
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
        "guide": get_opf_guide(book_config),
        "ncxid": config.struct.ncx_toc.id}
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


def create_dedication_page(dst):
    data = const.dedication_page_html % {
        "title": "Dedication"}
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


def get_indent(heading):
    return heading.split()[0].count("#") - 1


def get_toc_entry(linktext, filename, anchor="", indent=0):
    return const.toc_entry_with_file % {
        "linktext": linktext,
        "anchor": anchor,
        "indent": indent,
        "filename": filename}


def get_toc_entries(book_config):
    html = [
        get_toc_entry("Title Page", config.struct.title.filename),
        get_toc_entry("Copyright", config.struct.copyright.filename),
        get_toc_entry("Dedication", config.struct.dedication.filename),
        get_toc_entry("Acknowledgements", config.struct.ack.filename),
        ]
    for heading in md.assemble_headings(book_config):
        linktext = heading.strip("#").replace("`", "").strip()
        anchor = md.get_anchor_name(heading)
        indent = get_indent(heading)
        filename = config.struct.main.filename
        html.append(get_toc_entry(linktext, filename, anchor, indent))
    return "\n".join(html)


def create_toc_page(path, dst):
    book_config = get_config(path)
    data = const.toc_page_html % {
        "title": book_config.title,
        "tocentries": get_toc_entries(book_config)}
    with open(dst, "w") as fh:
        fh.write(data.encode("utf-8"))


def get_ncx_toc_entry(linktext, filename, playorder, anchor="", indent=""):
    return const.ncx_toc_entry % {
            "linktext": linktext,
            "anchor": anchor,
            "indent": indent,
            "filename": filename,
            "playorder": playorder,
            "id": anchor}


def get_ncx_toc_entries(book_config):
    xml = [
        get_ncx_toc_entry("Title Page", config.struct.title.filename, 1),
        get_ncx_toc_entry("Copyright", config.struct.copyright.filename, 2),
        get_ncx_toc_entry("Dedication", config.struct.dedication.filename, 3),
        get_ncx_toc_entry("Acknowledgements", config.struct.ack.filename, 4),
        ]
    for index, heading in enumerate(md.assemble_headings(book_config)):
        anchor = md.get_anchor_name(heading)
        linktext = heading.strip("#").replace("`", "").strip()
        indent = get_indent(heading) * const.toc_indent
        xml.append(get_ncx_toc_entry(linktext, config.struct.main.filename,
                                     index + 5, anchor, indent))
    return "\n".join(xml)


def create_ncx_toc_page(path, dst):
    book_config = get_config(path)
    data = const.ncx_page % {
        "uuid": content_uuid,
        "depth": const.toc_depth,
        "title": book_config.title,
        "tocentries": get_ncx_toc_entries(book_config)}
    with open(dst, "w") as fh:
        fh.write(data.encode("utf-8"))


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
    dst_cover = get_file_path(dst_path, config.struct.cover.filename)
    create_cover_image(path=dst_path, src=const.cover_src, dst=dst_cover)
    # add title page
    dst_title = get_file_path(dst_path, config.struct.title.filename)
    create_title_page(path=dst_path, dst=dst_title)
    # add copyright page
    dst_copyright = get_file_path(dst_path,config.struct.copyright.filename)
    create_copyright_page(path=dst_path, dst=dst_copyright)
    # add dedication page
    dst_dedication = get_file_path(dst_path, config.struct.dedication.filename)
    create_dedication_page(dst=dst_dedication)
    # add acknowledgements page
    dst_acks = get_file_path(dst_path, config.struct.ack.filename)
    create_acks_page(dst=dst_acks)
    # add embedded toc
    dst_toc = get_file_path(dst_path, config.struct.html_toc.filename)
    create_toc_page(path=dst_path, dst=dst_toc)
    # add main html file
    dst_html = get_file_path(dst_path, config.struct.main.filename)
    copy_file(src=src_html, dst=dst_html)
    # add ncx toc page
    dst_ncx_toc = get_file_path(dst_path, config.struct.ncx_toc.filename)
    create_ncx_toc_page(path=dst_path, dst=dst_ncx_toc)
    # assumble epub files
    files = [dst_cover, dst_title, dst_copyright, dst_toc, dst_dedication,
             dst_acks, dst_html, dst_ncx_toc]
    create_content_opf(dst_path, dst_html)
    create_archive(dst_path, files)