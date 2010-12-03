# users generic .zshrc file for zsh(1)
# last update 2010-11-22 23:15:45

## Environment variable configuration
## Default shell configuration

#
# LANG
#
case "${OSTYPE}" in
    freebsd*|darwin*)
        export LANG=ja_JP.UTF-8
        ;;
    linux*)
        export LANG=ja_JP.UTF-8
        ;;
    cygwin*)
        export LANG=ja_JP.SJIS
        ;;
esac

#
# set prompt
#
autoload colors
colors
case ${UID} in
    0)
        PROMPT="%B%{${fg[red]}%}%/#%{${reset_color}%}%b "
        PROMPT2="%B%{${fg[red]}%}%_#%{${reset_color}%}%b "
        SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
        [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{${fg[white]}%}${HOST%%.*} ${PROMPT}"
        ;;
    *)
        PROMPT="%{${fg[red]}%}%/%%%{${reset_color}%} "
        PROMPT2="%{${fg[red]}%}%_%%%{${reset_color}%} "
        SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
        [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{${fg[white]}%}${HOST%%.*} ${PROMPT}"
        ;;
esac

# auto change directory
#
setopt auto_cd

# auto directory pushd that you can get dirs list by cd -[tab]
#
#setopt auto_pushd

# command correct edition before each completion attempt
#
#setopt correct

# compacked complete list display
#
setopt list_packed

# no remove postfix slash of command line
#
setopt noautoremoveslash

# no beep sound when complete list displayed
#
setopt nolistbeep

#
# other options
#
setopt BASH_AUTO_LIST
setopt LIST_AMBIGUOUS
setopt AUTO_PUSHD

## Keybind configuration
#
# emacs like keybind (e.x. Ctrl-a goes to head of a line and Ctrl-e goes
# to end of it)
#
bindkey -e

# historical backward/forward search with linehead string binded to ^P/^N
#
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end

## Command history configuration
#
HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=50000
setopt hist_ignore_all_dups  # 重複するコマンド行は古い方を削除
setopt hist_ignore_dups      # 直前と同じコマンドラインはヒストリに追加しない
setopt share_history         # コマンド履歴ファイルを共有する
setopt append_history        # 履歴を追加 (毎回 .zsh_history を作るのではなく)
setopt inc_append_history    # 履歴をインクリメンタルに追加
setopt hist_no_store         # historyコマンドは履歴に登録しない
setopt hist_reduce_blanks    # 余分な空白は詰めて記録
#setopt hist_ignore_space    # 先頭がスペースの場合、ヒストリに追加しない

## Completion configuration
#
autoload -U compinit
compinit -u

#
#  http://memo.officebrook.net/20100123.html
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

## Alias configuration
#
# expand aliases before completing
#
setopt complete_aliases # aliased ls needs if file/dir completions work

alias where="command -v"
alias j="jobs -l"

case "${OSTYPE}" in
    freebsd*|darwin*)
        alias ls="ls -G -w"
        ;;
    linux*)
        alias ls="ls --color"
        ;;
    cygwin*)
        alias ls="ls --color"
        ;;
esac

alias la="ls -a"
alias lf="ls -F"
alias ll="ls -la"
alias du="du -h"
alias df="df -h"
alias su="su -l"

refe_utf8() {
    refe $@ | nkf -Ew
}
alias refe='refe_utf8'
alias diff="diff --strip-trailing-cr"

 [[ $TERM = "dumb" ]] && TERM=xterm-color

case "${TERM}" in
    kterm*|xterm*)
        export LSCOLORS=gxfxcxdxbxegedabagacad
        export LS_COLORS='di=01;36:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
        export ZLS_COLORS=$LS_COLORS
        zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
        ;;
    *)
        export LSCOLORS=ExFxCxdxBxegedabagacad
        export LS_COLORS='di=01;36:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
        export ZLS_COLORS=$LS_COLORS
        zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
        ;;
esac


## path
#
case "${OSTYPE}" in
    freebsd*|darwin*)
        export PATH=/opt/local/bin:/usr/local/bin:$PATH
        export MANPATH=/opt/local/share/man:/usr/local/share/man:$MANPATH
        ;;
    linux*)
        export PATH=$HOME/.gem/ruby/1.8/bin:$PATH
        export PATH=/var/lib/gems/1.8/bin:$PATH
# default
        ;;
    *)
        export PATH=/usr/bin
#default
        ;;
esac

## editor configuration
#
export EDITOR="/usr/bin/emacs -nw"

## load user .zshrc configuration file
#
[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine

# Emacs
#
[[ $EMACS = t ]] && unsetopt zle

# 引数の検索ワードで google 検索 (日本語可)
function google() {
    local str opt
    if [ $# != 0 ]; then # 引数が存在すれば
        for i in $*; do
            str="$str+$i"
        done
        str=`echo $str | sed 's/^\+//'` # 先頭の「+」を削除
        opt='search?num=50&hl=ja&ie=euc-jp&oe=euc-jp&lr=lang_ja'
        opt="${opt}&q=${str}"
    fi
    w3m http://www.google.co.jp/$opt # 引数がなければ $opt は空
}



# ^でcd .. コマンド発行(^を入力したい場合はCtrl-Vしてから入力)
# function cdup() {
#     echo
#     cd ..
#     zle reset-prompt
# }
# zle -N cdup
# bindkey '\^' cdup

# \でcd - コマンド発行(\を入力したい場合はCtrl-Vしてから入力)
# function cdrev() {
#     echo
#     cd -
#     zle reset-prompt
# }
# zle -N cdrev
# bindkey '\\' cdrev

# cdした際にlsコマンド発行
# case "${OSTYPE}" in
#     freebsd*|darwin*)
#         function chpwd() { ls -G }
#         ;;
#     *)
#         function chpwd() { ls -v -F --color=auto }
#         ;;
# esac

# 文字列検索
function find-grep() { find . -type f -print | xargs grep -n --binary-files=without-match $@ }

# 文字列置換
function find-grep-edit() { find . -type f -exec sed -i "s/$1/$2/g" "{}" \; }

# ファイル拡張子名置換
function rep-ext() { for f in **/*.$1; do mv $f ${f%.$1}.$2; done }

# ファイル解凍
function extract() {
    case $1 in
        *.tar.gz|*.tgz) tar xzvf $1 ;;
        *.tar.xz) tar Jxvf $1 ;;
        *.zip) unzip $1 ;;
        *.lzh) lha e $1 ;;
        *.tar.bz2|*.tbz) tar xjvf $1 ;;
        *.tar.Z) tar zxvf $1 ;;
        *.gz) gzip -dc $1 ;;
        *.bz2) bzip2 -dc $1 ;;
        *.Z) uncompress $1 ;;
        *.tar) tar xvf $1 ;;
        *.arj) unarj $1 ;;
    esac
}
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=extract

# Gitのリポジトリのトップレベルにcdするコマンド
# http://d.hatena.ne.jp/hitode909/20100211/1265879271
function u()
{
    cd ./$(git rev-parse --show-cdup)
    if [ $# = 1 ]; then
        cd $1
    fi
}

# mkcd
function mkcd() {
    [ $# = 1 ] && mkdir "$1" && cd "$1"
}

# 補完機能の設定
setopt no_beep
setopt no_list_beep

# Set aliases
alias ..='cd ..'
alias bk='cd -'
alias em=emacs


