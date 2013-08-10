# Personal configuration files

As working on many different environments, I wanted to maintain common
configuration files.

I prefer Linux development environments, which gives me much freedom. I like to
use console mode(non-GUI) Emacs over GNU screen. Even if I am forced to use
Windows, usually in a big company, I can happily enjoy similar setup using
Emacs/Cygwin.

## Emacs stuff

This contains the setup to use Emacs in the similar way in pure(?) Linux,
Windows, Cygwin.

It also contains several elisp files, which I wrote as I could not find the
alternative.

### My toy elisp 

 - tce.el : Minimalist multiple development project support. User defines a
    		'module', which describes the source directory structure, how to
    		build, what files are to be used in 'etags/cscope/grep', etc. Then
    		multiple 'projects' belonging to it can be registered, where each
    		project has different source root directory.
            For example, we can define a 'linux-kernel' module and register
    		multiple projects belongs to it and easily switch between projects.

 - diffstat.el : A simple implementation of diffstat unix tool in Emacs.  This
				 mode can be used as standalone for a certain diff file but it
				 will be more useful with vc mode.
