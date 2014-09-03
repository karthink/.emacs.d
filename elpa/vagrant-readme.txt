This package lets you send vagrant commands while working within a
project containing a Vagrantfile.

It will traverse the directory tree until a Vagrantfile is found
and assume this is the box you want to work with. It can be handy
to bring a box up, (re)provision, or even ssh to without leaving
emacs.

The emacs command `vagrant-up` will run `vagrant up` in a shell,
other commands follow the pattern `vagrant-X` emacs command runs
`vagrant X` in the shell. An exception is vagrant-edit, which will
open the Vagrantfile for editing.
