" more configs in /etc/vimrc and /usr/share/vim/vimfiles

set nocompatible
" set relativenumber
set number
set ruler
set scrolloff=5 " keep 5 lines of context above and below cursor

set t_Co=256
set background=dark

filetype indent on
set wrap linebreak
set autoindent
set smartindent

set expandtab
set tabstop=4 
set sw=4
 
set wildmenu           " better command-line completion
set showcmd            " Show (partial) command in status line.
set showmatch          " Show matching brackets.
set ignorecase         " Do case insensitive matching
set smartcase          " Do smart case matching
set incsearch          " Incremental search
" set autowrite          " Automatically save before commands like :next and :make
" set hidden             " Hide buffers when they are abandoned
set mouse=nv            " Enable mouse usage, but not in insert mode

set history=20

set ttyfast   " this might improve performance, iono

" this might fix the slow O problem
set ttimeoutlen=100

" Clear highlighting from screen
nnoremap <silent> <C-l> :nohlsearch<CR><C-l>
set hlsearch

" colours after 80 chars
" set colorcolumn=81
" set columns=81
" whatever
" end of new stuff, aug 21 2014

" these three lines are for the solarized colour scheme
let g:solarized_termcolors = 1
let g:solarized_termtrans = 1
colorscheme solarized

if has("syntax")
    syntax on
"     set textwidth=79
else
    set spell
    "set wrapmargin=14
endif

nnoremap Y y$
nnoremap <space> :
nnoremap Q :

" command causes vim to explode when i reload $MYVIMRC
" ! forces overwrite of whatever the command was before
command! Q qall
command! W w

"this stuff is also set in /usr/share/vim/vimfiles/archlinux.vim
"allow backspacing over everything in insert mode
set backspace=indent,eol,start
" Suffixes that get lower priority when doing tab completion for filenames.
" These are files we are not likely to want to edit or read.
" set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.png,.jpg

" these files are always ignored by vim because i won't ever want to edit them.
set wildignore=*.o,*.obj,*.hi,*.png,*.jpg


" by default, vim keeps backup files in the same dir as the working file.
" much better to keep them somewhere else, yes?
set backupdir=~/.vim/backups
set directory=~/.vim/backups

" persistent undo, allows undoing after exiting vim
if has('persistent_undo')
    set undodir=~/.vim/undo
    set undofile

    " disable persistent undo for files stored in /tmp or ~/tmp
    au BufWritePre /tmp/* setlocal noundofile
    au BufWritePre ~/tmp/* setlocal noundofile
endif

" compile and display a latex file
" noremap <c-b> :! pdflatexandevince % <CR> <CR>

" abbreviations, these are pretty cool
abbreviate #d #define
abbreviate #i #include
abbreviate Wall {-# OPTIONS_GHC -Wall #-}
abbreviate LANGUAGE {-# LANGUAGE #-}

" these two functions are to integrate dmenu
" thanks dude: http://leafo.net/posts/using_dmenu_to_open_quickly.html
" Strip the newline from the end of a string
function! Chomp(str)
    return substitute(a:str, '\n$', '', '')
endfunction

" open file under cursor in new tab
noremap gf gf

" Find a file and pass it to cmd
function! DmenuOpen(cmd)
    let fname = Chomp(system("find . | dmenu -i -l 20 -p " . a:cmd))
"     let fname = Chomp(system("ls -A1 | dmenu -i -l 20 -p " . a:cmd))
    if empty(fname)
        return
    endif
    execute a:cmd . " " . fname
endfunction

noremap <c-t> :call DmenuOpen("tabe")<cr>
" noremap <c-e> :call DmenuOpen("e")<cr>

" open help in a vertical split on the left
autocmd FileType help wincmd L

" automatically comment out lines 
au FileType haskell,vhdl,ada let b:comment_leader = '-- '
au FileType c,cpp,java,javascript let b:comment_leader = '// '
au FileType bash,zsh,sh,python,perl,make,conf,gitcommit let b:comment_leader = '# '
au FileType vim let b:comment_leader = '" '
au FileType tex let b:comment_leader = '% '
au FileType fortran,xdefaults let b:comment_leader = '! '
if !exists("b:comment_leader") 
    let b:comment_leader = '# ' " a sane default 
endif
noremap <silent> g/ :sil s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:noh<CR>
noremap <silent> g- :sil s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:noh<CR>
" nnoremap g/ :<C-B>sil <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:noh<CR>
" nnoremap g- :<C-B>sil <C-E>s/^/V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:noh<CR>

" highlight current line number 
hi clear LineNr
hi clear CursorLine
hi clear CursorLineNR
hi LineNr ctermfg=magenta
hi CursorLineNR cterm=bolditalic ctermfg=93
set cursorline
set nocursorcolumn

" highlights for the tab bar
hi clear TabLine
hi clear TabLineFill
hi clear TabLineSel
hi TabLineSel cterm=bolditalic ctermfg=124
hi TabLine ctermfg=brown

" different higlhights for st
if $TERM == "xterm-256color"
    set background=light
    hi clear LineNr
    hi clear CursorLine
    set hlsearch
endif

nnoremap <F4> :diffu<CR>
nnoremap <F12> :so $MYVIMRC<CR>

" save whenever focus is lost
au FocusLost * silent! wa

nnoremap <F2> :r! earlget 

" if writing mail, set the spellchecker to F7 (-e for email syntax)
autocmd FileType mail :nnoremap <F7> :w<CR>:!aspell -e -c %<CR>:e<CR> 
