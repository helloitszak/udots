" ================ General Config ====================
set number                      "Line numbers are good
set backspace=indent,eol,start  "Allow backspace in insert mode
set history=1000                "Store lots of :cmdline history
set showcmd                     "Show incomplete cmds down the bottom
set showmode                    "Show current mode down the bottom
set showmatch                   "Show matching brackets

" Disable blink on gui cursor, visual block insert is a line
set guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
set visualbell                  "No sounds
set autoread                    "Reload files changed outside vim
set hidden                      "This lets buffers exist without a window
set virtualedit=onemore         "give extra space at end of line
set magic
set fcs=vert:│
" set t_Co=256                    "pretty colors
set termguicolors
set timeout timeoutlen=300 ttimeoutlen=1
set relativenumber

let mapleader=","
let g:mapleader=","
let maplocalleader=","
let g:mapleader=","

" ================ Turn Off Swap Files ==============
set noswapfile
set nobackup
set nowb


" ================ Persistent Undo ==================
" Keep undo history across sessions, by storing in file.
" Only works all the time.
" if has('persistent_undo')
"   silent !mkdir ~/.vim/backups > /dev/null 2>&1
"   set undodir=~/.vim/backups
"   set undofile
" endif


" ================ Indentation ======================
set autoindent
set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab

filetype plugin indent on
syntax enable

" Display tabs and trailing spaces visually
set list
set listchars=tab:▸\ ,trail:•
set nowrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points

" ================ Scrolling ========================
set scrolloff=8         "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

" ================ Completion =======================
set wildmode=list:longest
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
" set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing
" set wildignore+=*vim/backups*
" set wildignore+=*sass-cache*
" set wildignore+=*DS_Store*
" set wildignore+=vendor/rails/**
" set wildignore+=vendor/cache/**
" set wildignore+=*.gem
" set wildignore+=log/**
" set wildignore+=tmp/**
" set wildignore+=*.png,*.jpg,*.gif
set completeopt=preview,menuone

" ================ Folds ============================
set foldmethod=indent   "fold based on indent
set foldnestmax=3       "deepest fold is 3 levels
set nofoldenable        "dont fold by default

" ================ Searching ========================
set ignorecase " Case insensitive search
set smartcase  " Non-case sensitive search
set incsearch  " Incremental search
set hlsearch   " Highlight search results

" ================ Mouse ========================
set mousehide
set mouse=a

" ================ Clipboard ========================
if has('unnamedplus')
  set clipboard=unnamedplus
else
  set clipboard=unnamed
endif

" ================ GUI ========================
" set guioptions-=r
" set guioptions-=L
" set guioptions-=T " disable macvim
" set guifont=Inconsolata\ XL:h17,Inconsolata:h20,Monaco:h17


" PLUGINS
exec 'source' stdpath('config') . '/vim-plug/plug.vim'
call plug#begin(stdpath('data') . '/plugged')

Plug 'iCyMind/NeoSolarized'
Plug 'easymotion/vim-easymotion'
Plug 'preservim/nerdcommenter'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()


colorscheme NeoSolarized



" EasyMotion
map f <Plug>(easymotion-f)
map t <Plug>(easymotion-t)
map F <Plug>(easymotion-F)
map T <Plug>(easymotion-T)

" Quickly get rid of highlights
nnoremap <silent> <leader>/ :nohls<CR>

" disable shitty manpage lookup mode
nnoremap K k
vnoremap K k

" >type visual
nnoremap Q @

" Remap nerdcommenter bindings
let g:NERDCreateDefaultMappings = 0
nmap <leader>cc <plug>NERDCommenterToggle

" Airline
let g:airline_theme='solarized'
let g:airline_mode_map = {
      \ 'n'  : 'N',
      \ 'i'  : 'I',
      \ 'R'  : 'R',
      \ 'v'  : 'V',
      \ 'V'  : 'VL',
      \ 'c'  : 'CMD',
      \ '' : 'VB',
      \ }
