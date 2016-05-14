"NeoBundle Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath^=/Users/yamaguchikazuteru/.vim/bundle/neobundle.vim/

" Required:
call neobundle#begin(expand('/Users/yamaguchikazuteru/.vim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Add or remove your Bundles here:
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'flazz/vim-colorschemes'

" You can specify revision/branch/tag.
NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------)}))

" タブによる補完機能を可能にする
set nocompatible

" display number
set number

" make nobackup
set nobackup

" make noswapfile
set noswapfile

" 自動インデントする
set smartindent

" 標準タブは2
set tabstop=2

" 挿入モードでのインデント
set shiftwidth=2
set softtabstop=2
set noexpandtab
set smarttab

" タブ入力を空白文字に置き換える
set expandtab

" 内部の解釈の文字コード
set encoding=utf-8

" 内部の改行コード
set fileformat=unix

" 対応括弧の瞬間強調時間
set matchtime=4

" ヤンクをクリップボードへ送り込む
set clipboard+=unnamed

" vim上で文字を選択しただけでクリップボードコピー
set guioptions+=a

" ルーラーを表示
set ruler
set title

" とりあえずscheme指定でざっくり指定
colorscheme default

" 検索
set incsearch

" 下線
set cursorline

" ファイルタイプ検出
filetype plugin indent on

" 括弧閉じ挿入
imap { {}<LEFT>
imap [ []<LEFT>
imap ( ()<LEFT>

" クリップボード使う
set clipboard=unnamed,autoselect

" escと同じ
inoremap <C-c> <Esc>

" 検索結果ハイライト
set hlsearch

" バックスペース有効
set backspace=indent,eol,start

" 括弧の入力時にカーソルを対応する括弧の上に一定時間表示する
set showmatch
set matchtime=5

" vimコマンド履歴
set history=100

" 入力途中のvimコマンドを右下に表示
set showcmd

" バックアップファイル用ディレクトリ
set backupdir=$HOME/backup

" 大文字小文字検索無視
set ignorecase

" 検索時にファイルの最後までいったら最初に戻らないようにする
set wrapscan

" エクスプローラーで開く時のディレクトリ
set browsedir=buffer

" 保存時に行末の空白を除去する
autocmd BufWritePre * :%s/\s\+$//e

" 矩形選択自由範囲移動
set virtualedit+=block

" vimを立ち上げた時に、自動的にvim-indent-guidesをオンにする
let g:indent_guides_enable_on_vim_startup = 1


" **************************************************************
" 色設定
" **************************************************************

" sytax color on/off
syntax enable
syntax on


" **************************************************************
" 補完設定
" **************************************************************

" **************************************************************
" 各PLUGIN
" **************************************************************

" ファイルをtreeで表示してくれる
NeoBundle 'scrooloose/nerdtree'

" インデントに色をつけて見やすくする
NeoBundle 'nathanaelkane/vim-indent-guides'

" **************************************************************
" 各vim script
" **************************************************************


