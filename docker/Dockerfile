FROM php:8.3-apache

RUN apt-get update \
    && apt-get install -y --no-install-recommends asciidoctor coderay ruby-rouge \
    && rm -rf /var/lib/apt/lists/* \
    && a2enmod rewrite

COPY apache-site.conf /etc/apache2/sites-available/000-default.conf
