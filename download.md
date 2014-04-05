---
layout: common
formats: [markdown, html, pdf, epub, mobi]
docs: [quick-start::Quick Start,
       user-guide::User Guide,
       processes-tutorial::Processes Tutorial]
download-url: https://github.com/lfe/lfe.github.io/blob/master/downloads
---

# LFE Downloads

## Source Code

You can download the source code for the LFE master branch
<a href="https://github.com/rvirding/lfe/tarball/master">here</a>.

If you'd prefer to download the latest, bleeding-edge work, you can download
the <a href="https://github.com/rvirding/lfe/tarball/develop">develop
branch</a>.

## Documentation

Some of the documentation on the <a href="/">LFE Docs Site</a> is being used to
generate content in other formats. As we create more of these, we are making
them available here:

<ul>
{% for docname in page.docs %}
  {% assign docnameparts = docname | split: '::' %}
  <li>{{ docnameparts.last }} [
    {% for format in page.formats %}
      <a
      href="{{ page.download-url }}/{{ docnameparts.first }}.{{ format }}?raw=true">{{ format }}</a>
      {% if format != page.formats.last %}
        |
      {% else %}
        ]
      {% endif %}
    {% endfor %}
    </li>
{% endfor %}
</ul>
