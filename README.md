Datura Corsets
==============

Here is the source of the website [Datura Corsets](http://ladatura-corsets.com/).
It is the website of a one-person enterprise which creates corsets near Nantes (France).

The website is static, generated using [Hakyll](http://jaspervdj.be/hakyll/).
The build is designed for Windows, but things should work similarly on Unix systems.

## Requirements

You need `ghc` (an Haskell compiler) and `hakyll` module from cabal (see http://jaspervdj.be/hakyll/) to compile `site.hs`.

The `compile.bat` script also expect `upx` to be either in the path or in the same directory (see http://upx.sourceforge.net/).
This is only used to reduce the size of the final executable (so it is optional).

To be able to build the website, you need `convert` from ImageMagick to be either in the path or in the same directory (see http://imagemagick.org/).

## Content

As it is not synced nor open-source, I will explain how to add content.

### Articles

Articles are the main items of the website.

An article is defined by:

- **a folder name** `articles/[name]/`: this name will be used in URLs
- **a Markdown file** `articles/[name]/index.md` which contains:
	- a **title** metadata: the full name of the article
	- a **tags** metadata: a comma-separated list of tag names
	- a content: the description of the article
- **a cover picture** `articles/[name]/index.jpg`: it will be used in lists and in the article's page
- *(optional)* **one or several extra pictures** `articles/[name]/*.jpg`: they will feed a gallery in the article's page

_Images must have a **lowercase** `.jpg` extension!_

### Pages

Pages represent unique pages, like *contact* or *about*.

A page is defined by:

- **a Markdown file** `pages/[name].md` which contains:
	- a **title** metadata: the name of the page that should be in the title HTML tag
	- a content: the content of the page

**OR**

- **an HTML file** `pages/[name].html` which contains:
	- a **title** metadata: the name of the page that should be in the title HTML tag
	- a content: the HTML content of the page

**In both ways, the generated page will use the `default.html` template.**

The generated pages will be put directly under the root of project, so:

- avoid a `pages/[name].md` and a `pages/[name].html` with the same name
- use `pages/index.md` or `pages/index.html` to make the homepage

<!--
### Homepage gallery

**TODO**
-->

## License

The MIT License (MIT)
Copyright (c) 2014 David Sferruzza
 
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
