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
[new package system](http://pkg.racket-lang.org).

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

To convert the file named `file.xml` to `file.scrbl`:

```
raco livescribe file.xml
```

To convert the file named `file.xml` to `file.scrbl`, and render
`file.scrbl` as an HTML file named `file.html`, as if by running
`scribble --html file.scrbl`:

```
raco livescribe -r html file.xml
```

To display the list of available command line options and switches:

```
raco livescribe -h
```

To reduce typing, you may optionally create an alias to `raco
livescribe` in your shell. Replace `.bashrc` and `.cshrc` in the
examples below, with the appropriate init file for your shell.

Sh-like shells:
```
echo 'alias livescribe="raco livescribe"' >> ~/.bashrc
```

Csh-like shells:
```
echo 'alias livescribe raco livescribe' >> ~/.cshrc
```


# Todo

0. Enable HTML inlining.
0. Enable Frog markdown output
0. Use tree system to create proper references to parent comments.
0. Write Disqus comment writer.
0. Create tests.
