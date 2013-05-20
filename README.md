# Introduction

**Livescribe** is a program, written in
[Racket](http://racket-lang.org) to convert the XML files created by
the programs [ljdump](https://github.com/ghewgill/ljdump), or
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

1. Write Scribble writer.
2. Use tree system to create proper references to parent comments.
3. Write Disqus comment writer.
4. Use exceptions.
5. Create tests.
6. Enable command-line use.
7. Check if reply_count is 0.
8. Create planet2 package
