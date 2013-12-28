cl-immersive
============

The goal is to get an immersive Lisp environment up and running on a Raspberry Pi Model B.
This would start out with a simple replacement shell based on _ecl_'s REPL.  Eventually
it would be cool to a graphical UI either based off zen or CLXS as well as stumpwm. Another
approach would be to create the GUI in a web browser using websockets and state sharing between
the browser (e.g. a Chromebook) and a Lisp server (e.g. the Raspberry Pi). That seems a long
way off right now so the first step is setting things up so I can hack happily in Lisp.

My choices

+ ecl for REPL
+ vi for editor (EMACS/SLIME seems like overkill on a Raspberry Pi)
+ Some sort of readline support
+ ASDF 3 and Quicklisp for packaging
+ Basic mapping of common Unix commands
    - pwd
    - cd
    - mkdir
    - git
    - ls
    - bash
    - shell


# Dev setup

## Step 1

Here is a script I used to setup my dev environment on a fresh Raspbbery Pi. You can
run it from your user account. I recommend making sure the *pi* or *root* accounts remain
available incease you break something while doing development.

The following will only need to be done once to get the right Raspbian Packages installed.

```shell
    # Basic tools
    sudo apt-get update
    sudo apt-get install git-core curl build-essential openssl libssl-dev
    sudo apt-get install autoconf # used when we get to adding support for stumpwm
    # Helpful libraries
    sudo apt-get install libgmp-dev
    sudo apt-get install libgmp3-dev
    sudo apt-get install libatomic-ops-dev
    sudo apt-get install libncurses5-dev
    sudo apt-get install libreadline-dev
    sudo apt-get install libffi-dev
```

Now fetch and build _ecl_.


```shell
  cd # go to the $HOME directory
  git clone git://git.code.sf.net/p/ecls/ecl
  cd ecl # change to the ecl directory cloned in $HOME
  ./configure --prefix=$HOME --enable-unicode=yes \
      --with-clx=yes \
      --with-asdf=yes \
      --with-x=yes \
      "CFLAGS=-mcpu=arm1176jzf-s -DAO_USE_PTHREAD_DEFS"
  make
  make install
```


It takes about ninety minutes to compile on my Raspberry Pi Model B without the clx and X
options.  Thirty minutes just to compile the *C* code and the rest for compiling
the *Lisp*. Adding the clx and X options made it take about twice that.

We can now execute _ecl_ from the command line at @$HOME/bin/ecl@ and 
get a working Lisp REPL.

## Step 2

Assembling a rich Lisp environment requires adding ASDF and Quicklisp. _ecl_ ships
with ASDF v2. ASDF v3 is now out so we want to upgrade to that. Finally we also
want install Quicklisp to have a modern way to fetch and integrate remote packages.

Getting ASDF v3 is pretty straight forward.  I ignore that ASDF 2.x comes with ecl and just
clone a copy of ASDF repo in my home directory. Then run make to create the *build/asdf.lisp*
file need to load it.

```shell
    cd # make sure I'm in my $HOME directory
    git clone git://common-lisp.net/projects/asdf/asdf.git
    cd asdf
    make
```

Now I can use it via my Lisp init (e.g. *.eclrc*)

```lisp
    ;; Load ASDF v3
    (load (concatenate 'string (ext:getenv "HOME") "/asdf/build/asdf.lisp"))
    
    ;; Load cl-immersive
    (load (concatenate 'string (ext:getenv "HOME") "/cl-immersive.lisp"))
    
```
