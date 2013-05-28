# Introduction

**Livescribe** is a [Racket](http://racket-lang.org) program, used to
convert XML files created by
[ljdump](https://github.com/ghewgill/ljdump), or
[ljmigrate](https://github.com/ceejbot/ljmigrate), to
[Scribble](http://docs.racket-lang.org/scribble/). Input files are
categorized as either "entry", or "comment" data, referring to the
posts created by the owner of the journal, and comments by journal
viewers, respectively.


# Installation

**Livescribe** is available via Racket's
[Planet2](http://pkg.racket-lang.org).

```
raco pkg install livescribe
```

If that doesn't work, install the dependencies, and **Livescribe**
itself, from the local disk.

```
git clone http://github.com/jbclements/sxml.git
git clone http://github.com/ebzzry/livescribe.git
raco pkg install sxml/ livescribe/
```

The trailing slashes are important, to tell `raco` that you are
installing from local directories. Without it, it will try to fetch
the sources from the internet.


# Usage

To convert the file named `file.xml` to `file.scrbl`:

```
raco livescribe file.xml
```

Like above, but in addition to generating `file.scrbl`, render it to
`file.html` as well, as if by running `scribble --html file.scrbl`.

```
raco livescribe --html file.xml
```

Again, like above, but in addition to generating `file.scrbl`, render
it to `file.md` as well, as if by running `scribble --markdown
file.scrbl`.

```
raco livescribe --markdown file.xml
```


To display the list of available command line options and switches.

```
raco livescribe -h
```


# Updating

If you installed **Livescribe** using the first method described in the
section *Introduction*, you can update it by running:

```
raco pkg update livescribe
```

However, if you used the latter method, you may update it by pulling
the updates, uninstalling **Livescribe**, then installing it
again:

```
cd livescribe
git pull origin master
cd ..
raco pkg remove livescribe
raco pkg install livescribe/
```


# Miscellany

To reduce typing, you may optionally create an alias to `raco
livescribe` in your shell.

Sh-like shells:
```
echo 'alias livescribe="raco livescribe"' >> ~/.bashrc
```

Csh-like shells:
```
echo 'alias livescribe raco livescribe' >> ~/.cshrc
```

Replace `.bashrc`, and `.cshrc`, with the appropriate init file for
your shell.


# Todo

0. Enable [Frog](https://github.com/greghendershott/frog) markdown output.
0. Use tree system to create proper references to parent comments.
0. Write Disqus comment writer.
0. Create tests.
0. Enable HTML inlining.
