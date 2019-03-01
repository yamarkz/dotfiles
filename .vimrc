set nocompatible

set runtimepath+=~/.vim/bundle/neobundle.vim/
call neobundle#begin(expand('~/.vim/bundle'))
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'rking/ag.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build' : {
      \     'mac' : 'make',
      \     'linux' : 'make',
      \    },
      \ }
call neobundle#end()

" Required:
filetype off
filetype plugin indent off

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
" nowrap
set nowrap
" no undo
set noundofile
" NERDTree ShowHidden
let NERDTreeShowHidden = 1
" vimgrep
autocmd QuickFixCmdPost *grep* cwindow
" カラースキーマの指定
syntax on
" vim-indent-guides
let g:indent_guides_auto_colors=0
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_start_level=2
let g:indent_guides_guide_size=1
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree', 'tagbar', 'unite']
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=#444433 ctermbg=black
"autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#333344 ctermbg=darkgray
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#444433 ctermbg=black
set ts=2 sw=2 et

" ctrlp
"let g:ctrlp_cache_dir=$HOME.'/.cache/ctrlp'
"let g:ctrlp_clear_cache_on_exit=0
"let g:ctrlp_lazy_update=1
"let g:ctrlp_root_markers=['Gemfile', 'Gemfile.lock', '.gitignore']
"let g:ctrlp_max_height=20
"let g:ctrlp_custom_ignore = {
"  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
"  \ 'file': '\v\.(exe|so|dll)$',
"  \ 'link': 'some_bad_symbolic_links',
"  \ }
"let g:ctrlp_user_command = 'ag %s -l'

"" unite.vim
"The prefix key.
let g:unite_enable_start_insert=1
let g:unite_enable_ignore_case=1
let g:unite_enable_smart=1
let g:unite_source_history_yank_enable=1
let g:unite_source_rec_max_cache_files = 15000

nmap <Space> [unite]
nnoremap <silent> [unite]a :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
nnoremap <silent> [unite]p :<C-u>Unite<Space>file<CR>
nnoremap <silent> [unite]m :<C-u>Unite<Space>buffer file_mru<CR>
nnoremap <silent> [unite]d :<C-u>Unite<Space>directory_mru<CR>
nnoremap <silent> [unite]b :<C-u>Unite<Space>buffer<CR>
nnoremap <silent> [unite]r :<C-u>Unite<Space>register<CR>
nnoremap <silent> [unite]t :<C-u>Unite<Space>tab<CR>
nnoremap <silent> [unite]<CR> :<C-u>Unite<Space>file_rec/async:!<CR>
"unite.vimを開いている間のキーマッピング
autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()"{{{
  nmap <buffer> <ESC> <Plug>(unite_exit)
endfunction"}}}

"" unite-grep
" unite-grepのバックエンドをagに切り替える
let g:unite_source_grep_command='ag'
let g:unite_source_grep_default_opts='--nocolor --nogroup'
let g:unite_source_grep_recursive_opt=''
let g:unite_source_grep_max_candidates=200

"" grep検索結果の再呼出
"nnoremap <silent> [unite]r :<C-u>UniteResume search-buffer<CR>

"" 選択した文字列をunite-grep
"vnoremap /g y:Unite grep::-iHRn:<C-R>=escape(@", '\\.*$^[]')<CR><CR>

" ファイルタイプ検出
filetype plugin indent on

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
""""""""""""""""""""""""""""

""""""""""""""""""""""""""""
" .gitignore ignore_pattern
""""""""""""""""""""""""""""
function! s:unite_gitignore_source()
  let sources = []
  if filereadable('./.gitignore')
    for file in readfile('./.gitignore')
      if file !~ "^#\\|^\s\*$"
       call add(sources, file)
      endif
    endfor
  endif

  if isdirectory('./.git')
    call add(sources, '.git')
  endif
  call add(sources, '\(png\|gif\|jpeg\|jpg\)$')
  let pattern = escape(join(sources, '|'), './|')
  call unite#custom#source('file_rec', 'ignore_pattern', pattern)
  call unite#custom#source('file_rec/async', 'ignore_pattern', pattern)
  call unite#custom#source('grep', 'ignore_pattern', pattern)
endfunction
call s:unite_gitignore_source()
"""""""""""""""""""""""""""
