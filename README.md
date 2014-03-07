This is the Bristol Hackspace Quick Equipment Label Generator.

# Theory

The script (labels.scm) runs and reads the list (list-of-things.scm), and then generates:

1. A QR code for each thing (`qr-NAME.png`)
2. Several SVG files, each containing a page full of labels (`page-NUMBER.svg`)
3. PDF versions of the pages (`page-NUMBER.pdf`)
4. An index table listing all the equipment (`index.wiki`)

# Installation

The script is written in Chicken Scheme.

Install Chicken (http://www.call-cc.org) from your local package manager.

Once it's installed, install required Chicken eggs with the following commands:

```
$ chicken-install -s sxml-serializer
$ chicken-install -s miscmacros
```

And install required binary packages with your local package manager:

* [`qrencode`](http://fukuchi.org/works/qrencode/)
* [`inkscape`](http://inkscape.org/)

# Usage

To make labels and an index for all the things, run this command:

```
$ csi -script labels.scm
```

The next revision will have a facility to accept a list of thing names
on the command line, in which case it will only generate output for
those things, so you can just add new things (or revise existing
things) easily.
