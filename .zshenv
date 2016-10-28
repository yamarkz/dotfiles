alias la='ls -la'
alias la='ls -a'
alias ll='ls -l'
alias ls='ls -lav'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'
alias vi='vim'


# sudoの後のコマンドでエイリアスを有効にする
alias sudo='sudo'
# グローバルエイリアス
alias -g L='| less'
alias -g G='| grep'
alias -g be='bundle exec'

# encoding
export LANG=ja_JP.UTF-8

bindkey '^R' history-incremental-pattern-search-backward

# C で表じゅう出力をクリップボードにコピーする
if which pbcopy > /dev/null 2>&1 ; then
  # Mac
  alias -g C='| pbcopy'
elif which xsel > /dev/null 2>&1 ; then
  # Linux
  alias -g C='| xsel --input --clipboard'
elif which putclip > /dev/null 2>&1 ; then
  # Cygwin
  alias -g C='| putclip'
fi


# User configuration
# OS 別の設定
case ${OSTYPE} in
  darwin*)
    # Mac用の設定
    export CLICOLOR=1
    alias ls='ls -G'
    ;;
  linux*)
    # Linux用の設定
    alias ls='ls -F --color=auto'
    ;;
esac

export PATH="~/.rbenv/shims:~/.rbenv/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin:/usr/local/git/bin:/usr/local/nginx/sbin"

PATH="usr/local/bin:$PATH"     # for PHP55
PATH="usr/local/sbin:$PATH"     # for php-fpm


# rbenv
eval "$(rbenv init -)"

# Node.js
export NODE_PATH=$HOME/.nodebrew/current/lib/node_modules
export PATH=$HOME/.nodebrew/current/bin:$PATH
