export LANG=ja_JP.UTF-8

# 色を使用できるようにする
autoload -Uz colors
colors

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# プロンプト
# 1行表示
# PROMPT="%~ %#"
# 2行表示
PROMPT="%{${fg[green]%}[%n@%m]%{${reset_color}%} %~
%# "


# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default

# ここで指定した文字は単語区切りとみなされる
# / も区切りと扱うので、^W でディレクトリ1つ分を削除できる
zstyle ':zle:*' word-chars "/=;@:{},|"
zstyle ':zle:*' word-style unspecified

####################################
# 補完
# 補完機能を有効にする
autoload -Uz compinit
compinit

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..

# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
	/usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'


#################################
# vcs_info
autoload -Uz vcs_info
autoload -Uz add-zsh-hook

zstyle ':vcs_info:*' formats '%F{green}(%s)-[%b]%f'
zstyle ':vcs_info:*' actionformats '%F{red}(%s)-[%b|%a]%f'

fuction _update_vcs_info_msg() {
	LANG=en_US.UTF-8 vcs_info
	RPROMPT="${vcs_info_msg_0_}"
}
add-zsh-hook precmd _update_vcs_info_msg


##############################
# オプション
# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# beep を無効にする
setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# '#' 以降をコメントとして扱う
setopt interactive_comments

# ディレクトリ名だけでcdする
setopt auto_cd

# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 同じコマンドをひすとりに残さない
setopt hist_ignore_all_dups

# スペースから始まるコマンド行はヒストリに残さない
setopt hist_reduce_blanks

# 高機能なワイルドカード展開を使用する
setopt extended_glob


###########################
# キーバインド

# ^R で履歴検索をする時に * でワイルドカードを使用できるようにする
bindkey '^R' history-incremental-pattern-search-backward


##########################
# エイリアス

alias la='ls -a'
alias ll='ls -l'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo'
# グローバルエイリアス
alias -g L='| less'
alias -g G='| grep'
alias -g st='status'
alias -g be='bundle exec'

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


########################
# OS 別の設定
case ${OSTYPE} in
	darwin*)
		# Mac用の設定
		export CLICOLOR=1
		alias ls='ls -G -F'
		;;
	linux*)
		# Linux用の設定
		alias ls='ls -F --color=auto'
		;;
esac

# Path to your oh-my-zsh installation.
#export ZSH=~/.oh-my-zsh
#ZSH_THEME="wedisagree"
#plugins=(git)
#source $ZSH/oh-my-zsh.sh

# User configuration
export PATH="~/.rbenv/shims:~/.rbenv/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin:/usr/local/git/bin:/usr/local/nginx/sbin"

# Node.js
export NODE_PATH=$HOME/.nodebrew/current/lib/node_modules
export PATH=$HOME/.nodebrew/current/bin:$PATH

# rbenv
eval "$(rbenv init -)"

#
# zplug
#
source ~/.zplug/init.zsh

zplug 'zsh-users/zsh-autosuggestions'
zplug 'zsh-users/zsh-completions'
zplug 'zsh-users/zsh-syntax-highlighting', nice:10
zplug 'mollifier/anyframe'

if ! zplug check --verbose; then
  printf 'Install? [y/N]: '
  if read -q; then
    echo; zplug install
  fi
fi

zplug load --verbose
