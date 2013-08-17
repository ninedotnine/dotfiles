set number
set t_Co=256
set background=dark
highlight LineNr ctermfg=brown
" set hlsearch
if has("syntax")
    syntax on
    "set textwidth=80
else
    set spell
    "set wrapmargin=14
endif

filetype indent on
set wrap linebreak
"set autoindent
set smartindent

" this doesn't seem to work...
"map <MiddleMouse> <Nop>
nnoremap Q q
nnoremap Y y$
"imap <c-t> the 

" these are all set in /etc/vimrc anyway...
"set expandtab
"set tabstop=4
"set sw=4

"allow backspacing over everything in insert mode
"set backspace=indent,eol,start

" by default, vim keeps backup files in the same dir as the working file.
" much better to keep them somewhere else, yes?
"set backupdir=~/backups/vim

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
"set showcmd            " Show (partial) command in status line.
"set showmatch          " Show matching brackets.
"set ignorecase         " Do case insensitive matching
"set smartcase          " Do smart case matching
"set incsearch          " Incremental search
"set autowrite          " Automatically save before commands like :next and :make
"set hidden             " Hide buffers when they are abandoned
"set mouse=a            " Enable mouse usage (all modes)

" these two functions are to integrate dmenu
" thanks dude: http://leafo.net/posts/using_dmenu_to_open_quickly.html
" Strip the newline from the end of a string
function! Chomp(str)
    return substitute(a:str, '\n$', '', '')
endfunction

" Find a file and pass it to cmd
function! DmenuOpen(cmd)
    let fname = Chomp(system("find . | dmenu -i -l 20 -p " . a:cmd))
    "let fname = Chomp(system("ls -1 | dmenu -i -l 20 -p " . a:cmd))
    if empty(fname)
        return
    endif
    execute a:cmd . " " . fname
endfunction

noremap <c-t> :call DmenuOpen("tabe")<cr>
noremap <c-e> :call DmenuOpen("e")<cr>
