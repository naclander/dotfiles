" Set Pathogen Path
execute pathogen#infect()
set nu
set cursorline
syntax on
set hlsearch
set colorcolumn=80
"call pathogen#infect()
nmap <F7> :TagbarToggle<CR>
filetype  plugin on
"clang_complete options. More info in :help clang_complete
"let g:clang_user_options = '|| exit 0'
"let g:clang_complete_copen = 0
"let g:clang_complete_auto = 1
"Complete options (disable preview scratch window)
set completeopt=menu,menuone,longest
"Tab width to 4
set tabstop=4
set ai
"curser in the middle of the page when pgdown and pgup
"set scrolloff=999
" Show line number, cursor position.
set ruler
" Search as you type.
set incsearch
" set visual mode on mouse rollover
set mouse=a
" set paste and no paste mode
set pastetoggle=<C-p>
" copy in visual mode
map <C-c> "+y<CR>"
"number of spaces to indent when pressing > or <
set sw=4
" swap ; and : so that we don't always have to press shit+; to get to : mode
nnoremap ; :
nnoremap : ;
"Set <C-h> to gundo shortcut
nnoremap <C-h> :MundoToggle<CR>
" Maintain undo history between sessions
set undofile
set undodir=~/.vim/undodir
" Skip up and down by 10 lines
map <C-j> 10gj
map <C-k> 10gk
" Support for C++11 stuff in Syntastic vim plugin
let g:syntastic_cpp_compiler = '-std=c++11'
syntax enable
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
"Spell check
:set spell spelllang=en_us
" Tab navigation ( S is shift)
nnoremap <S-h> :tabprevious<CR>
nnoremap <S-l>   :tabnext<CR>
" Set tags file
set tags=./tags,tags;
" split on the right
set splitright
"airline status line
set laststatus=2
" map escape to exit neovim terimanl
tnoremap <Esc> <C-\><C-n>
" vim devicons 
set encoding=utf8
let g:airline_powerline_fonts = 1
"add line to ctrp
let g:ctrlp_extensions = ['buffertag', 'tag', 'line', 'dir']

if (empty($TMUX))
  if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

colorscheme one
let g:airline_theme='one'
let g:one_allow_italics = 1
set background=dark

"Convert tabs to spaces
set expandtab
set tabstop=4

" Sort imports
xmap e :<c-u>ImportSort<cr>

