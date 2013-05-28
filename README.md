# Introduction

**Livescribe** is a [Racket](http://racket-lang.org) program, used to
convert XML files created by
[ljdump](https://github.com/ghewgill/ljdump), or
[ljmigrate](https://github.com/ceejbot/ljmigrate), to
[Scribble](http://docs.racket-lang.org/scribble/). Input files are
categorized into either "entry", or "comment" data, the former
referring to the posts created by the owner of the journal.


# Installation

**Livescribe** is available via Racket's
[new package system](http://pkg.racket-lang.org). To install it:

```
raco pkg install livescribe
```

If that doesn't work, you may clone the GitHub repository, and install
from local disk:

```
git clone http://github.com/ebzzry/livescribe.git
raco pkg install livescribe/
```

The trailing slash is important to tell `raco` that you are installing
from a local directory. Without it, it will try to fetch the sources
from the internet.


# Usage

```
raco livescribe file.xml
```

This command creates a file named `file.scrbl` in the directory where
`file.xml` is found.

```
raco livescribe -r html file.xml
```

In addition to creating a file named `file.scrbl` as described above,
it also creates a file named `file.html`, as if by running `scribble
--html file.scrbl`. The list of available options to the `-r` switch
are available by running `raco livescribe -h`.


# Todo

0. Enable HTML inlining.
0. Enable Frog markdown output
0. Use tree system to create proper references to parent comments.
0. Write Disqus comment writer.
0. Create tests.
