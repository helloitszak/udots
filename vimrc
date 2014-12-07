set nocompatible "be IMproved

if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
end

call neobundle#rc(expand('~/.vim/bundle'))

try
  source ~/.vimrc.local
catch
endtry


" ================ General Config ====================
set number                      "Line numbers are good
set backspace=indent,eol,start  "Allow backspace in insert mode
set history=1000                "Store lots of :cmdline history
set showcmd                     "Show incomplete cmds down the bottom
set showmode                    "Show current mode down the bottom
set showmatch                   "Show matching brackets
set gcr=a:blinkon0              "Disable cursor blink
set visualbell                  "No sounds
set autoread                    "Reload files changed outside vim
set hidden                      "This lets buffers exist without a window
set virtualedit=onemore         "give extra space at end of line
set magic
set fcs=vert:│
set t_Co=256                    "pretty colors
set timeout timeoutlen=300 ttimeoutlen=1
set relativenumber

" syntax on                       "turn on syntax highlighting

" Change leader to a comma because the backslash is too far away
let mapleader=","
let g:mapleader=","
let maplocalleader=","
let g:mapleader=","

" =============== Vundle Initialization ===============
" Use NeoBundle plugin to manage all other plugins
if filereadable(expand("~/.vim/bundles.vim"))
  source ~/.vim/bundles.vim
endif

" ================ Turn Off Swap Files ==============
set noswapfile
set nobackup
set nowb

" ================ Persistent Undo ==================
" Keep undo history across sessions, by storing in file.
" Only works all the time.
if has('persistent_undo')
  silent !mkdir ~/.vim/backups > /dev/null 2>&1
  set undodir=~/.vim/backups
  set undofile
endif

" ================ Indentation ======================
set autoindent
set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab

filetype plugin on
filetype indent on

" Display tabs and trailing spaces visually
set list
set listchars=tab:▸\ ,trail:•
set nowrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points

" ================ Folds ============================
set foldmethod=indent   "fold based on indent
set foldnestmax=3       "deepest fold is 3 levels
set nofoldenable        "dont fold by default

" ================ Completion =======================
set wildmode=list:longest
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif
set completeopt=preview,menuone

" ================ Scrolling ========================
set scrolloff=8         "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

" ================ Clipboard ========================
if has('unnamedplus')
  set clipboard=unnamedplus
else
  set clipboard=unnamed
endif

" ================ GUI ========================
set guioptions-=r
set guioptions-=L
set guioptions-=T " disable macvim
set guifont=Inconsolata\ XL:h17,Inconsolata:h20,Monaco:h17

" ================ Searching ========================
set ignorecase " Case insensitive search
set smartcase  " Non-case sensitive search
set incsearch  " Incremental search
set hlsearch   " Highlight search results

" ================ Mouse ========================
set mousehide
set mouse=a

" ================ THEME ========================
colorscheme jellybeans


" ================ Airline ======================
set laststatus=2 "This makes the airline always show up
let g:airline_left_sep = '⮀'
let g:airline_left_alt_sep = '⮁'
let g:airline_right_sep = '⮂'
let g:airline_right_alt_sep = '⮃'
let g:airline_branch_prefix = '⭠ '
let g:airline_readonly_symbol = '⭤'
let g:airline_linecolumn_prefix = '⭡'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#whitespace#enabled = 1
let g:airline_mode_map = {
      \ 'n'  : 'N',
      \ 'i'  : 'I',
      \ 'R'  : 'R',
      \ 'v'  : 'V',
      \ 'V'  : 'VL',
      \ 'c'  : 'CMD',
      \ '' : 'VB',
      \ }

" ================ Syntastic ======================
let g:syntastic_enable_signs = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_loc_list_height = 5
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [] }
let g:syntastic_html_checkers = ['handlebars']
let g:syntastic_error_symbol = '✗' " Hat tip http://git.io/SPIBfg
let g:syntastic_warning_symbol = '⚠'
let g:syntastic_full_redraws = 1

" ================ Fugitive ======================
nnoremap <Leader>gc :Gcommit -v<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gca :Gcommit -a -v<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gp :Git push<CR>
 " Mnemonic, gu = Git Update
nnoremap <Leader>gu :Git pull<CR>
nnoremap <Leader>gd :Gdiff<CR>
" Start git command
nnoremap <leader>gi :Git<space>
" Undo the last commit
command! Gcundo :Git reset HEAD~1

" ================ Tagbar ================
nmap <leader>t :TagbarToggle<CR>

" ================ Session ================
let g:session_autosave = 0
let g:session_autoload = 0
nnoremap <leader>os :OpenSession<CR>

" ================ NERDTree ================
nmap <C-i> :NERDTreeToggle<CR>

" ============== EasyMotion ================
hi link EasyMotionTarget WarningMsg
hi link EasyMotionShade Comment

let g:EasyMotion_do_mapping = 0
map f <Plug>(easymotion-f)
map t <Plug>(easymotion-t)
map F <Plug>(easymotion-F)
map T <Plug>(easymotion-T)

" ====== FOKN COOL GENERIC KEYMAPS BRO =====
" ==== Leader Keymaps

" Open Quickfix
nnoremap <silent> <leader>f :botright copen<CR>

" Horizontal and Vertical Splits
nnoremap <silent> <leader>hs :split<Bar>:wincmd j<CR>
nnoremap <silent> <leader>vs :vsplit<Bar>:wincmd l<CR>

" Maximize current split
nnoremap <silent> <leader>m <C-w>_<C-w><Bar>

" Previous Window
nnoremap <silent> <leader>, :wincmd p<CR>

" Get rid of distracting search highlights
noremap <silent> <leader>/ :nohls<CR>

" Highlight search word under cursor without jumping to next
nnoremap <leader>h *<C-O>

nnoremap <leader>r :call g:ToggleNuMode()<CR>
function! g:ToggleNuMode()
  if(&rnu == 1)
    set nornu
  else
    set rnu
  endif
endfunction

" ==== Normal Mode Remaps

" Easier redo
nnoremap U :redo<CR>

" jump to beginning and end of lines easier
noremap <C-H> ^
noremap <C-L> $

" disable shitty manpage lookup mode
nnoremap K k

" save the keystrokes, no more shift!
" Shift-; is already in muscle memory. No sense relearning it
" nnoremap ; :
" vnoremap ; :

" yank entire buffer
nnoremap gy :%y+<cr>

" select entire buffer
nnoremap vy ggVG

" make Y behave like other capitals
nnoremap Y y$

" +/-: Increment Numbers
nnoremap + <c-a>
nnoremap - <c-x>

" create newlines without entering insert mode
nnoremap go o<Esc>k
nnoremap gO O<Esc>j

" Equal Size Windows
nnoremap <silent> g= :wincmd =<CR>

" Swap Windows
nnoremap <silent> gx :wincmd x<CR>

" >type visual
nnoremap Q @@

nnoremap p gp
nnoremap d "_d
nnoremap dd dd
nnoremap c "_c

nmap \ <Leader>c<space>

" Become a god?
nnoremap <up> <c-w>+
nnoremap <down> <c-w>-
nnoremap <left> <c-w><
nnoremap <right> <c-w>>


" ==== Normal Mode Ctrl Mappings
" Go to Previous Buffer
nnoremap <c-b> <c-^>

" Ctrl-/: a more powerful '/'
nmap <c-_> [unite]l

" ==== Visual Mode Mappings
" >doc lookup mode
vnoremap K k


" ==== Visual Mode Keymaps
" Yank and go to end of selection
xnoremap y y']
" Paste in visual mode should not replace the default register with deleted
" text
xnoremap p "_dP

" Delete into blackhole register to not clobber the last yank. To Cut use x
" instead
xnoremap d "_d

" \: Toggle Comment
xmap \ <Leader>c<space>

" Enter: Highlight visual selection
xnoremap <silent> <CR> y:let @/ = @"<cr>:set hlsearch<cr>

xnoremap > >gv
xnoremap < <gv

xmap <Tab> >
xmap <s-tab> <

" ==== Autocommands
augroup MyAutoCmd
augroup END


" ==== Unite
nnoremap [unite] <Nop>
nmap <space> [unite]

" General fuzzy search
nnoremap <silent> [unite]<space> :<C-u>Unite
  \ -buffer-name=files buffer file_mru bookmark file_rec/async<CR>


" Quick registers
nnoremap <silent> [unite]r :<C-u>Unite -buffer-name=register register<CR>

"Quick buffer and mru
nnoremap <silent> [unite]u :<C-u>Unite -buffer-name=buffers file_mru buffer<CR>

"Yank history
nnoremap <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<CR>
" Quick outline
nnoremap <silent> [unite]o :<C-u>Unite -buffer-name=outline -vertical outline<CR>

" Quick sessions (projects)
nnoremap <silent> [unite]p :<C-u>Unite -buffer-name=sessions session<CR>

" Quick sources
nnoremap <silent> [unite]a :<C-u>Unite -buffer-name=sources source<CR>

" Quickly switch lcd
nnoremap <silent> [unite]d
      \ :<C-u>Unite -buffer-name=change-cwd -default-action=cd directory_mru directory_rec/async<CR>

" Quick file search
nnoremap <silent> [unite]f :<C-u>Unite -buffer-name=files file_rec file/new<CR>

" Quick grep from cwd
nnoremap <silent> [unite]g :<C-u>Unite -buffer-name=grep grep:.<CR>

" Quick help
nnoremap <silent> [unite]h :<C-u>Unite -buffer-name=help help<CR>

" Quick line using the word under cursor
nnoremap <silent> [unite]l :<C-u>UniteWithCursorWord -buffer-name=search_file line<CR>

" Quick MRU search
nnoremap <silent> [unite]m :<C-u>Unite -buffer-name=mru file_mru<CR>

" Quick find
nnoremap <silent> [unite]n :<C-u>Unite -buffer-name=find find:.<CR>

" Quick commands
nnoremap <silent> [unite]c :<C-u>Unite -buffer-name=commands command<CR>

" Quick bookmarks
nnoremap <silent> [unite]b :<C-u>Unite -buffer-name=bookmarks bookmark<CR>

" Fuzzy search from current buffer
" nnoremap <silent> [unite]b :<C-u>UniteWithBufferDir
" \ -buffer-name=files -prompt=%\  buffer file_mru bookmark file<CR>

" Quick commands
nnoremap <silent> [unite]; :<C-u>Unite -buffer-name=history -default-action=edit history/command command<CR>

" Custom Unite settings
autocmd MyAutoCmd FileType unite call s:unite_settings()
function! s:unite_settings()
  " nmap <buffer> <ESC> <Plug>(unite_exit)
  imap <buffer> <ESC> <Plug>(unite_insert_leave)
  nmap <buffer> <ESC> <Plug>(unite_exit)
  " imap <buffer> <c-j> <Plug>(unite_select_next_line)
  imap <buffer> <c-j> <Plug>(unite_insert_leave)
  nmap <buffer> <c-j> <Plug>(unite_loop_cursor_down)
  nmap <buffer> <c-k> <Plug>(unite_loop_cursor_up)
  nmap <buffer> <tab> <Plug>(unite_loop_cursor_down)
  nmap <buffer> <s-tab> <Plug>(unite_loop_cursor_up)
  imap <buffer> <c-a> <Plug>(unite_choose_action)
  imap <buffer> <Tab> <Plug>(unite_insert_leave)
  imap <buffer> jj <Plug>(unite_insert_leave)
  imap <buffer> <C-w> <Plug>(unite_delete_backward_word)
  imap <buffer> <C-u> <Plug>(unite_delete_backward_path)
  imap <buffer> '     <Plug>(unite_quick_match_default_action)
  nmap <buffer> '     <Plug>(unite_quick_match_default_action)
  nmap <buffer> <C-r> <Plug>(unite_redraw)
  imap <buffer> <C-r> <Plug>(unite_redraw)
  inoremap <silent><buffer><expr> <C-s> unite#do_action('split')
  nnoremap <silent><buffer><expr> <C-s> unite#do_action('split')
  inoremap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
  nnoremap <silent><buffer><expr> <C-v> unite#do_action('vsplit')

  let unite = unite#get_current_unite()
  if unite.buffer_name =~# '^search'
    nnoremap <silent><buffer><expr> r     unite#do_action('replace')
  else
    nnoremap <silent><buffer><expr> r     unite#do_action('rename')
  endif

  nnoremap <silent><buffer><expr> cd     unite#do_action('lcd')

  " Using Ctrl-\ to trigger outline, so close it using the same keystroke
  if unite.buffer_name =~# '^outline'
    imap <buffer> <C-\> <Plug>(unite_exit)
  endif

  " Using Ctrl-/ to trigger line, close it using same keystroke
  if unite.buffer_name =~# '^search_file'
    imap <buffer> <C-_> <Plug>(unite_exit)
  endif
endfunction
