---
layout: page
title: Archive
---

{% for post in site.posts %}
    {% assign images = post.content | split:"<img "%}
    {% assign imgvalue = '' %}
    {% for image in images %}
        {% if image contains 'src=' %}
        {% assign imageMarkup = image | split:'"' %}
        {% capture imgvalue %}
<img src="{{ imageMarkup[1] }}" height="100">
        {% endcapture %}
        {% break %}
        {% endif %}
    {% endfor %}
{{ imgvalue }}[ {{ post.date | date_to_string }}: {{ post.title }} ]({{ post.url }})
{% endfor %}
