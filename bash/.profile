#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export EDITOR="emacsclient -c"
export GOPATH="/home/skmd/.local/share/go"
export PATH=$PATH:$USERPATH:$GOPATH/bin

