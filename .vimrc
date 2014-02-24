set nu
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
" copy in viusal mode
map <C-c> "+y<CR>"
"number 0f spaces to indent when pressing > or <
set sw=4
" swap ; and : so that we don't always have to press shit+; to get to : mode
nnoremap ; :
nnoremap : ;
