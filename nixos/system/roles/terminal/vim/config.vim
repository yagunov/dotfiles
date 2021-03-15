" Configure appearance
colorscheme PaperColor

" Display relative line numbers
set number
set relativenumber

" Make search case-insensitive until you use upper case
set ignorecase
set smartcase

" Use spaces instead of tabs
set expandtab
set shiftwidth=4
set tabstop=4

" Display long lines as signle line
set nowrap

" Use X clipboard
set clipboard+=unnamedplus

" Custom keybindings (inspired by Spacemacs)
let mapleader = " "
nnoremap <leader>ft :NERDTreeToggle<CR>
nnoremap <leader>ff :Files<Space>
nnoremap <leader>bb :Buffers<CR>
nnoremap <leader>ss :BLines<Space>
nnoremap <leader>pf :Files<CR>
nnoremap <leader>/ :Rg<Space>
xmap s <Plug>VSurround

" Better visual selection shifting
vnoremap < <gv
vnoremap > >gv

" Format Nix files on save
augroup fmt
  autocmd!
  autocmd BufWritePre *.nix undojoin | Neoformat
augroup END
