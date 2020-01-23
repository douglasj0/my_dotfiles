path=( ~ $lpath ~/bin /usr/local /usr/local/bin/X11 /usr/bin /bin
        /usr/ucb /usr/etc /usr/bin/X11 /usr/lib /usr/lib/X11
        /usr/local/bin /etc /usr/openwin/bin /usr/gnu/bin
        /usr/sbin /opt/gnu/bin /opt/local/bin
        /usr/X11/bin /opt/lotus/bin /opt/Acrobat3/bin ~/notesr4 . )

lpath=(/usr/openwin/bin/xview /usr/openwin/bin)
mychoice='openwin'

manpath=(/usr/man /usr/local/man)
export MANPATH

export LD_LIBRARY_PATH=/usr/dt/lib:/usr/openwin/lib:/opt/lotus/notes/latest/sunspa
export GR_HOME=/usr/local/bin

alias -- netscape='/opt/netscape/communicator/netscape'
alias -- la='ls -a'
alias -- lf='ls -F'
alias -- ll='ls -la'
alias -- pu='pushd'
alias -- po='popd'
alias -- t1='TERM=vt100; export TERM; stty rows 24; DISPLAY=kludge:0.0; export DISPLAY'
alias -- ow='/usr/openwin/bin/openwin'
alias -- xt='~/bin/remote.xterms'
alias -- em='emacs -nw'
alias -- xmail='/usr/openwin/bin/xview/mail'
alias -- mae='remsh prod1 ''DISPLAY=192.9.201.118:0.0; export DISPLAY; mae &'' '

psg () { ps -aef | grep $* | grep -v grep }
npp () { a2ps $* | lpr -PNext_Printer }
nppp () { a2ps -p $* | lpr -PNext_Printer }
np () { lpr -PNext_Printer $* }

export NNTPSERVER='news'
