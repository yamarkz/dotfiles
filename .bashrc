case "${OSTYPE}" in
    darwin*)
	alias ls='ls -G'
	alias ll='ls -lG'
	alias la='ls -laG'
	;;
    linux*)
	alias ls='ls --color'
	alias ll='ls -l --color'
	alias la='ls -la --color'
	;;
esac

PS1="\033[31m\]\u: \033[32m\]\w \033[0m\] $"

alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias jsc="/System/Library/Frameworks/JavascriptCore.frameworks/Versions/A/Resources/jsc"
alias gb='git branch -a'
export PATH=/usr/local/bin:$PATH
