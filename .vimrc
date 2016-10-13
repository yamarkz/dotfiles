set nocompatible

set runtimepath+=~/.vim/bundle/neobundle.vim/
call neobundle#begin(expand('~/.vim/bundle'))
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'Yggdroot/indentLine'


call neobundle#end()

" Required:
filetype plugin indent on

NeoBundleCheck

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
syntax enable
colorscheme solarized
set background=dark
" 検索
set incsearch
" 下線
set cursorline
" クリップボード使う
set clipboard=unnamed,autoselect
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
" スクロール
set scrolloff=0
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
" no paste
set nopaste
" no undo
set noundofile
" カラースキーマの指定
syntax on

" ファイルタイプ検出
filetype plugin indent on

let g:indentLine_color_term = 111
let g:indentLine_color_gui = '#708090'
let g:indentLine_char = '|'

" escと同じ
inoremap <C-c> <Esc>

inoremap <C-a> <Esc>^i
inoremap <C-e> <Esc>$a
nnoremap <C-a> <Esc>^i
nnoremap <C-e> <Esc>$a

nnoremap <silent><C-j> :NERDTreeToggle<CR>
nnoremap <ESC> :noh<CR>

nnoremap sn gt
nnoremap sp gT
nnoremap st :<C-u>tabnew<CR>
nnoremap sT :<C-u>Unite tab<CR>

" 括弧閉じ挿入
imap { {}<LEFT>
imap [ []<LEFT>
imap ( ()<LEFT>



""""""""""""""""""""""""""""""
" 全角スペースの表示
""""""""""""""""""""""""""""""
function! ZenkakuSpace()
  highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=darkgray
endfunction

if has('syntax')
  augroup ZenkakuSpace
  autocmd!
  autocmd ColorScheme * call ZenkakuSpace()
  autocmd VimEnter,WinEnter,BufRead * let w:m1=matchadd('ZenkakuSpace','')
augroup END
call ZenkakuSpace()
endif
"""""""""""""""""""""""""""""
