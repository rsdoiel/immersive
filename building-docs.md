
Since this is a list project it makes sense to create a simple build script in Lisp
to process things. I want to use Mustache style templates and Markdown files to
generate the site statically.

## Lisp modules that might be useful

+ [cl-mustache](https://github.com/kanru/cl-mustache) - I think this is the one used by Lisp quickdocs
	- Support SBCL and CLisp, might have to fork to support CCL and ECL
	- [CL-Mustache](http://quickdocs.org/cl-mustache/api) website
+ [3bmd](https://github.com/3b/3bmd) - provides a more configurable Markdown processor including colourization support

