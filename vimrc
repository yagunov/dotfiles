set nocompatible

" Display relative line numbers
set number
set relativenumber
"set ruler

" Enable syntax highlightning:
syntax on

" Use 256 colors
set t_Co=256
let base16colorspace=256
colorscheme colorsbox-material

" Use jk to switch from insert to normal mode
inoremap jk <Esc>
set timeoutlen=75

" Incremental search
set incsearch
" Highlight all matches
set hlsearch
" Make search case-insensitive til you use upper case
set ignorecase
set smartcase

" Enable fuzzy file finding
set path+=**
" Display all matching files when we tab complete
set wildmenu

" Move selected text
vnoremap <C-j> :m '>+1<CR>gv=gv
vnoremap <C-k> :m '<-2<CR>gv=gv

" Use X clipboard
set clipboard+=unnamedplus
