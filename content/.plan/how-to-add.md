+++
title = ".plan Files"
in_search_index = true
template = "plan/page.html"

[extra]
long_title = "LFEX .plan Files"
long_description = "How to add your .plan"
+++

## Steps

1. [Fork][fork] the LFE site repo
1. Create a markdown file in the [plan directory][plan-dir] with your Github user name as the filename
1. Copy the Markdown metadata from one of the other `.plan` files to your new file
1. Edit the [plan index file][plan-index] to include a link to your new `.plan` file
1. Submit a PR, merging into the `builder` branch
1. Address any review feedback
1. Share with your friends once it's published :-)

[//]: ---Named-Links---

[fork]: https://github.com/lfe/lfe.github.io/fork
[plan-dir]: https://github.com/lfe/lfe.github.io/tree/builder/content/.plan
[plan-index]: https://github.com/lfe/lfe.github.io/blob/builder/content/.plan/_index.md
