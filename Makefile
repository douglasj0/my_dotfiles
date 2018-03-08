
dotfiles = $(filter-out .svn Makefile bin_mac bin_lin bin_sol MacOSX Xmodmap Xresources launchd.conf, $(wildcard *))

home-dotfiles = $(addprefix $(HOME)/.,$(dotfiles))

$(HOME)/.%: %
	[ ! -e $@ -o -L $@ ]
	$(RM) $@
	ln -s $(CURDIR)/$* $@

uninstall: $(addsuffix __unlink, $(home-dotfiles))
%__unlink:
	[ -L $* ] && $(RM) $*
