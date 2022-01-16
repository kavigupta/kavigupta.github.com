---
layout: page
title: Archive
---

{% for post in site.posts %}
{% if post.tags contains 'draft' %}
{% else %}
### [ {{ post.date | date_to_string }}: {{ post.title }} ]({{ post.url }})
{% endif %}
{% endfor %}
