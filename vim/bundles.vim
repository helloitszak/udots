NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }

" Unite!
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'Shougo/unite-help'
NeoBundle 'Shougo/unite-session'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'thinca/vim-unite-history'
NeoBundle 'mileszs/ack.vim'

" Code Completion
" Shougo/neocomplcache.vim
NeoBundle 'Valloric/YouCompleteMe', {
\ 'build' : {
\     'mac' : './install.sh --clang-completer --system-libclang --omnisharp-completer',
\     'unix' : './install.sh --clang-completer --system-libclang --omnisharp-completer',
\     'windows' : './install.sh --clang-completer --system-libclang --omnisharp-completer',
\     'cygwin' : './install.sh --clang-completer --system-libclang --omnisharp-completer'
\    }
\ }
NeoBundle 'tpope/vim-endwise'

" UI
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'bling/vim-airline'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'shougo/vimfiler.vim'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'kshenoy/vim-signature'
NeoBundle 'nathanaelkane/vim-indent-guides'

" General VIMprovements
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-surround'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'editorconfig/editorconfig-vim'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'mbbill/undotree'
NeoBundle 'jszakmeister/vim-togglecursor'
NeoBundle 'godlygeek/tabular'

" tpope/vim-unimpared (maybe)
" Chiel92/vim-autoformat (maybe)
" tpope/vim-speeddating (maybe)
" Shougo/vimshell.vim (maybe)
" troydm/easybuffer.vim (maybe)

" Filetypes
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'tpope/vim-rails'

NeoBundle 'tpope/vim-markdown'
NeoBundle 'tpope/vim-haml'
NeoBundle 'slim-template/vim-slim'
NeoBundle 'juvenn/mustache.vim'
NeoBundle 'hail2u/vim-css3-syntax'

NeoBundle 'chrisbra/csv.vim'
NeoBundle 'rodjek/vim-puppet'
NeoBundle 'mutewinter/vim-tmux'
NeoBundle 'mutewinter/nginx.vim'
NeoBundle 'mutewinter/taskpaper.vim'
NeoBundle 'mustache/vim-mustache-handlebars'

NeoBundle 'othree/javascript-libraries-syntax.vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'itspriddle/vim-jquery'

NeoBundle 'jnwhiteh/vim-golang'

NeoBundle 'tpope/vim-fireplace'

" Motions
NeoBundle 'lokaltog/vim-easymotion'
NeoBundle 'goldfeld/vim-seek'

NeoBundleCheck
