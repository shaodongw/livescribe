# Introduction

**Livescribe** is a [Racket](http://racket-lang.org) program, used to
convert the XML files created by
[ljdump](https://github.com/ghewgill/ljdump), or
[ljmigrate](https://github.com/ceejbot/ljmigrate), to
[Scribble](http://docs.racket-lang.org/scribble/). The input files are
categorized into either "entry", or "comment" data, the former
referring to the posts created by the owner of the journal.

At the moment, the Scribble writer is not yet written, and that what
works at the moment is extraction of entry and comment data, returned
as internal object representations of LiveJournal post information.


# Usage

```
racket livescribe/main.rkt file.xml
```


# Todo

0. Write Scribble writer.
0. Use tree system to create proper references to parent comments.
0. Write Disqus comment writer.
0. Use exceptions.
0. Create tests.
0. Enable command-line use.
0. Check if reply_count is 0.
0. Create planet2 package
