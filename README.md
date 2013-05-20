# Introduction

**Livescribe** is a [Racket](http://racket-lang.org) program, used to
convert XML files created by
[ljdump](https://github.com/ghewgill/ljdump), or
[ljmigrate](https://github.com/ceejbot/ljmigrate), to
[Scribble](http://docs.racket-lang.org/scribble/). Input files are
categorized into either "entry", or "comment" data, the former
referring to the posts created by the owner of the journal.

# Installation

Since Livescribe is not yet part of any of Racket's package systems,
offline installation is the only working method now. To install, run:

```
raco pkg install livescribe/
```

This command must be performed at the Git checkout level. The trailing
slash is important to tell `raco` that you are installing from a local
directory. Without it, it will try to fetch the Livescribe sources
from the internet.

# Usage

```
raco livescribe file.xml
```

A corresponding `file.scrbl` will then be present in the same
directory where `file.xml` is found.

# Caveats

0. The Scribble writer at the moment, is mostly rudimentary.

# Todo

0. Improve Scribble writer.
0. Enable HTML inlining.
0. Use tree system to create proper references to parent comments.
0. Write Disqus comment writer.
0. Create tests.
