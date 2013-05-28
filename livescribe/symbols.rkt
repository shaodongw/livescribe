#lang racket/base

(provide symbol-table)

;;; Add more symbols from:
;;;
;;; - https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
;;; - http://www.w3schools.com/tags/ref_entities.asp
(define symbol-table
  #hash(("&quot;" . #\")
        ("&amp;" . #\&)
        ("&apos;" . #\')
        ("&lt;" . #\<)
        ("&gt;" . #\>)
        ("&lsquo;" . #\‘)
        ("&rsquo;" . #\’)
        ("&ldquo;" . #\“)
        ("&rdquo;" . #\”)))
