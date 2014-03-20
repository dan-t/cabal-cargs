cabal-cargs
===========

`cabal-cargs` is a command line program for extracting compiler relevant
arguments from a cabal file and print them out formatted so that they're
directly usable as arguments for `ghc` or `hdevtools`.

The main motivation for writing `cabal-cargs` was to get a mostly just working
default, non modified `hdevtools`.

Example: Cabal and Sandbox aware Hdevtools
==========================================

If you want to call `hdevtools check` for a source file of a cabalized project and
would like to consider all compiler relevant arguments in the cabal file - like
`hs-source-dirs`, `ghc-options`, `cpp-options` ... - and also the `cabal sandbox`,
then you could just use `cabal-cargs` the following way:

    $> hdevtools check `cabal-cargs --format=hdevtools --sourcefile=Source.hs` Source.hs

This call of `cabal-cargs` will search for a cabal file starting at the directory
of `Source.hs` upwards the directory tree. The cabal file is then searched for
a fitting section for the given source file. A section is considered fitting if
the source file is contained in a directory or sub-directory listed in `hs-source-dirs`. 

At the end the fields of the found sections are printed out in the desired format.

Normally you don't want to use `cabal-cargs` manually, but use it to initialize the
options of hdevtools. So in the case of the editor `vim` and the plugin `vim-hdevtools`
you could use something like:

    function! s:CabalCargs(args)
       let l:output = system('cabal-cargs ' . a:args)
       if v:shell_error != 0
          let l:lines = split(l:output, '\n')
          echohl ErrorMsg
          echomsg 'args: ' . a:args
          for l:line in l:lines
             echomsg l:line
          endfor
          echohl None
          return ''
       endif
       return l:output
    endfunction
    
    function! s:HdevtoolsOptions()
        return s:CabalCargs('--format=hdevtools --sourcefile=' . shellescape(expand('%')))
    endfunction
    
    autocmd Bufenter *.hs :call s:InitHaskellVars()
    
    function! s:InitHaskellVars()
       if filereadable(expand('%'))
          let g:hdevtools_options = s:HdevtoolsOptions()
       endif
    endfunction

To see if `cabal-cargs` did the right thing you can verify the hdevtools options by
calling in the vim command line:

    :let g:hdevtools_options

Example: Compiler Arguments from Cabal File
============================================

Instead of searching for the cabal file by a source file the cabal file can be given explicitly:

    $> cabal-cargs --cabalfile=Some.cabal

If an additional source file is given, then the cabal file is searched for a fitting section.

Sections
========

If you don't want any automatic finding of sections or only want to consider a
certain section, then you could do this by using the options:
* `--library`
* `--executable=name`
* `--testsuite=name`
* `--benchmark=name`

You can use multiple of these options at once and even specify multiple
e.g. executables at once: `--executable=exe1 --executable=exe2 ...`.

Fields
======

By default all fields of a section are printed out. You can constrain the
output by the option: `--only=name`. This option can be specified multiple times.

The allowed names are the field names from the cabal file, just the hyphen
replaced by an underscore e.g.: `hs-source-dirs` -> `hs_source_dirs`.

Currently supported cabal fields are:
* `hs_source_dirs`
* `ghc_options`
* `default_extensions`
* `default_language`
* `cpp_options`
* `c_sources`
* `cc_options`
* `extra_lib_dirs`
* `extra_libraries`
* `ld_options`
* `include_dirs`
* `includes`

There are further some special fields:
* `package_db`
* `autogen_hs_source_dirs`
* `autogen_include_dirs`
* `autogen_includes`

It's not quite true, that all fields are printed out if not constrained, that's
only the case for the `pure` formatting option. For the other formatting options
currently the fields `c_sources`, `extra_libraries` and `ld_options` are ignored.  

Also there's one special field for `hdevtools` which is always printed out:
the used socket file.

Flags
=====

The conditional parts of the cabal file are respected by `cabal-cargs` by taking
the default values of the flags defined in the cabal file into account.

You can overwrite the default values of the flags with the options:
* `--enable=FLAGNAME`
* `--disable=FLAGNAME` 

It's also possible to overwrite the `OS` and `Arch` values - which by default are
the ones the cabal library was build on - with the options:
* `--os=OSNAME`
* `--arch=ARCHNAME`

Formatting
==========

By default the fields are formatted for the `ghc` compiler. The available options
for `--format` are:
* `ghc`
* `hdevtools`
* `pure`

`pure` prints the values like they are present in the cabal file and is mostly
only useful in conjunction with `--only` to get the value of one cabal field.

Installation
============

It's recommended to build `cabal-cargs` in a `cabal sandbox` with: `cabal install cabal-cargs`.
