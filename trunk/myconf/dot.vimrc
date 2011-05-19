""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible " get out of horrible vi - compatible mode
filetype on " detect the type of file
filetype plugin on " load filetype plugins
filetype indent on
set history=1000 " How many lines of history to remember
set clipboard+=unnamed " turns out I do like is sharing windows clipboard
set viminfo+=! " make sure it can save viminfo
set isk+=_,$,@,%,#,- " none of these should be word dividers,so make them not be
set autowrite
set autoread
set autochdir
set nobackup
set gdefault
"set helplang=cn
set copyindent
set showbreak=\ \ \ \ \  "indicator for wrapped lines
set shellslash

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim UI
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set wmh=0 "This sets the minimum window height to 0
set lsp=0 " space it out a little more (easier to read)
set wildmenu " turn on wild menu
set wildmode=list:longest,full
set ruler " Always show current positions along the bottom
set cmdheight=1 " the command bar is 2 high
"set number " turn on line numbers
set lz " do not redraw while running macros (much faster) (LazyRedraw)
set hid " you can change buffer without saving
set switchbuf=useopen
set backspace=2 " make backspace work normal
set whichwrap=b,s,<,>,[,],h,l  " backspace and cursor keys wrap to
set mouse=a " use mouse everywhere
set shortmess=atI " shortens messages to avoid 'press a key' prompt
set report=0 " tell us when anything is changed via :...
" make the splitters between windows be blank
"set fillchars=vert:\,stl:\,stlnc:\
"donot show toolbar,menubar and tabbar
set guioptions-=b
set guioptions-=T "get rid of toolbar
"set guioptions-=m "get rid of menu
"set guioptions-=e "remove the gui tabbar
"avoid windows explain alt combining keybinds
set winaltkeys=no
if version>=700
	set pumheight=10 "set popup menu hight
	set showtabline=2
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Visual Cues
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set showcmd
set showmatch " show matching brackets
set matchpairs=(:),{:},[:],<:>
set mat=5 " how many tenths of a second to blink matching brackets for
set nohlsearch " do not highlight searched for phrases
set incsearch " BUT do highlight as you type you search phrase
set ignorecase smartcase
"set listchars=tab: \ | \ ,trail: .,extends: > ,precedes: < ,eol: $ " what to show when I hit :set list
"set lines=41 " 80 lines tall
"set columns=160 " 160 cols wide
set so=5 " Keep 10 lines (top/bottom) for scope
set novisualbell " don't blink
set noerrorbells " no noises
set titlestring=%F
set statusline=%k(%02n)%t%m%r%h%w\ \[%{&ff}:%{&fenc}:%Y]\ \[line=%04l/%04L\ col=%03c/%03{col(\"$\")-1}]\ [%p%%]
set laststatus=2 " always show the status line
"set cursorline

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Text Formatting/Layout
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set fo=tcrqn " See Help (complex)
set si " smartindent
set tabstop=4 " tab spacing (settings below are just to unify it)
set softtabstop=4 " unify
set shiftwidth=4 " unify
set noexpandtab " real tabs please!
"set nowrap
set wrap " do not wrap lines
set smarttab " use tabs at the start of a line,spaces elsewhere
