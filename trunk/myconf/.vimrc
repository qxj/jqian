fun! s:NextHelpBar()
	let s:tagHelpBar = '|[^|]*|'
	"let s:tagHelpStar = '\*[^*]*\*'
	"let link_pos = search('\('.s:tagHelpStar.'\|'.s:tagHelpBar.'\)', 'w')
	let link_pos = search('\('.s:tagHelpBar.'\)', 'w')
	if link_pos == 0
		echohl ErrorMsg | echo 'No hyperlinks' | echohl None
	else
		echo
	endif
endfun

fun! s:PreviousHelpBar()
	let s:tagHelpBar = '|[^|]*|'
	"let s:tagHelpStar = '\*[^*]*\*'
	"let link_pos = search('\('.s:tagHelpStar.'\|'.s:tagHelpBar.'\)', 'w')
	let link_pos = search('\('.s:tagHelpBar.'\)', 'bw')
	if link_pos == 0
		echohl ErrorMsg | echo 'No hyperlinks' | echohl None
	else
		echo
	endif
endfun

function! Filetype_c()
	set cindent complete=.,w,b,u,k
	set dictionary+=$VIMHOME/wordlists/c.list
	set cino+=:0 "dont' indent case:
	set cino+=g0 "indent c++ public private etc...
	if has("win32")
		compiler bcc
	endif
endfunction

function! Filetype_cpp()
	set cindent complete=.,w,b,u,k
	set dictionary+=$VIMHOME/wordlists/cpp.list
	set cino+=:0 "dont' indent case:
	set cino+=g0 "indent c++ public private etc...
	if has("win32")
		compiler bcc
	endif
endfunction

function! Filetype_perl()
    setlocal cindent
	setlocal dictionary=$VIMHOME/wordlists/perl.list
endfunction

function! Filetype_mail()
	setlocal textwidth=76
endfunction

function! Filetype_ml()
    setlocal shiftwidth=2
    setlocal softtabstop=2
	setlocal dictionary=$VIMHOME/wordlists/xml.list
endfunction

function! Filetype_tex()
    if (! filereadable('Makefile'))
        setlocal makeprg=latex\ %
    endif
    setlocal tw=80 
    setlocal autoindent
endfunction

function! Filetype_lisp()
	setlocal dictionary=$VIMHOME/wordlists/lisp.list
endfunction

function! Filetype_html()
	setlocal dictionary=$VIMHOME/wordlists/xml.list
	setlocal indentkeys-=o,O,*<Return>,<>>,<bs>
endfunction

function! Filetype_help()
    noremap <buffer> <TAB>	:call <SID>NextHelpBar()<cr>
    noremap <buffer> <S-TAB>	:call <SID>PreviousHelpBar()<cr>
endfunction	
function! Term_keymap()
	set <F13>=[s
	set <F14>=<F9>
	set <F15>=[g
	set <F16>=[u
	set <F17>=[v
	set <F18>=[o
	map <F13> <C-F9>
	map <F14> <M-F9>
	map <F15> <S-F9>
	map <F16> <C-F11>
	map <F17> <C-F12>
	map <F18> <C-F5>
endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible " get out of horrible vi - compatible mode
filetype on " detect the type of file
filetype plugin on " load filetype plugins
filetype indent on
set history=1000 " How many lines of history to remember
set clipboard+=unnamed " turns out I do like is sharing windows clipboard
if has("win32")
	set ffs=dos,unix,mac " support all three,in this order
else
	set ffs=unix,dos,mac " support all three,in this order
endif
set viminfo+=! " make sure it can save viminfo
set isk+=_,$,@,%,#,- " none of these should be word dividers,so make them not be
set autowrite
set autoread
set autochdir
set nobackup
set gdefault
"set helplang=cn
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"new
set copyindent
set showbreak=\ \ \ \ \  "indicator for wrapped lines
set shellslash
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Theme/Colors 
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax on " syntax highlighting on 
if (has("gui_running"))
	set background=dark " we are using a dark background
	set nowrap
	"set guifont=DejaVu\ Sans\ Mono\ 10
	"set guifont=iMandysoft-CodeFont-III:h9:cOEM
	"set guifont=Lucida_Console:h9:cANSI
	"set guifont=Bitstream_Vera_Sans_Mono:h9:cANSI
	"set guifont=ProggyClean:h9:cANSI
    set guifont=Consolas:h11
 	colorscheme morning
	"colorscheme Dim
	"colorscheme darkblue
	"colorscheme baycomb
	"colorscheme desert
	"colorscheme biogoo
	"colorscheme oceandeep
	"colorscheme wintersday
	"colorscheme desertedocean
	"colorscheme navajo
	"colorscheme evening
else
	set paste "this option is useful when using Vim in a terminal
	set wrap
	colo ron
endif

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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"file encoding
" set fileencodings=ucs-bom,utf-8,prc,latin1
"
" {{{  multi-encoding setting
if has("multi_byte")
"set bomb
set fileencodings=ucs-bom,utf-8,cp936,big5,euc-jp,euc-kr,latin1
" CJK environment detection and corresponding setting
if v:lang =~ "^zh_CN"
" Use cp936 to support GBK, euc-cn == gb2312
set encoding=cp936
set termencoding=cp936
set fileencoding=cp936
elseif v:lang =~ "^zh_TW"
" cp950, big5 or euc-tw
" Are they equal to each other?
set encoding=big5
set termencoding=big5
set fileencoding=big5
elseif v:lang =~ "^ko"
" Copied from someone's dotfile, untested
set encoding=euc-kr
set termencoding=euc-kr
set fileencoding=euc-kr
elseif v:lang =~ "^ja_JP"
" Copied from someone's dotfile, untested
set encoding=euc-jp
set termencoding=euc-jp
set fileencoding=euc-jp
endif
" Detect UTF-8 locale, and replace CJK setting if needed
if v:lang =~ "utf8$" || v:lang =~ "UTF-8$"
set encoding=utf-8
set termencoding=utf-8
set fileencoding=utf-8
endif
else
echoerr "Sorry, this version of (g)vim was not compiled with multi_byte"
endif
" }}}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Folding
"    Enable folding,but by default make it act like folding is off,because folding is annoying in anything but a few rare cases
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set foldenable " Turn on folding
"set foldmethod=indent " Make folding indent sensitive
set foldmethod=manual " Make folding indent sensitive
set foldlevel=100 " Don't autofold anything (but I can still fold manually)
set foldopen-=search " don't open folds when you search into them
set foldopen-=undo " don't open folds when you undo stuff
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Win Manager
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:winManagerWidth=35 " How wide should it be( pixels)
let g:winManagerWindowLayout='FileExplorer' " What windows should it
"let g:winManagerWindowLayout='TagList,FileExplorer|BufExplorer' " What windows should it
let g:persistentBehaviour=0 "vim will quit if only the explorers window are the one left


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Buffer Explorer
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let bufExplorerDefaultHelp=0
let bufExplorerDetailedHelp=0
let bufExplorerMaxHeight=15

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TagList
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"for Windows only
"You'd better to put ctags.ext into vim runtime dir, to avoid vim cannot find it.
if has("win32")
	set path=.
	"set path+=H:\\Mercury_Rel6.2_M610\\mercury_delivery\\src\\Application\\DeltaMms\\**
	set path+=H:\\Mercury_Rel6.2_M610\\mercury_delivery\\src\\Application
	set path+=H:\\Mercury_Rel6.2_M610\\mercury_delivery\\src\\Apoxi
	set path+=H:\\Mercury_Rel6.2_M610\\mercury_delivery\\src\\Mmi
	let Tlist_Ctags_Cmd='ctags.exe' " Location of ctags
else
	set path=.
	set path+=/usr/include/**,/usr/lib/qt-3.1/include/**
	set tags=/data/home/linxd/src/linux-2.6.17.8/tags
endif
let Tlist_Sort_Type="name" " order by

let Tlist_Compact_Format=1
"If which is one part of winManager, no necessary to set Tlist_Exit_OnlyWindow.
let Tlist_Exit_OnlyWindow=1 " if you are the last,kill yourself

" Automatically close the folds for the non-active files in the taglist window
let Tlist_File_Fold_Auto_Close=1
let Tlist_Enable_Fold_Column=0 " Do not show folding tree

"Show Tlist menu and mouse right-click menu, supported after taglist.vim 4.0 beta
let Tlist_Show_Menu=0

"Display the tags for only one file in the taglist window
let Tlist_Show_One_File=1

"If which is one part of winManager, no necessary to set Tlist_Auto_Open.
"Display tag prototypes or tag names in the taglist window
let Tlist_Display_Tag_Scope=1
let Tlist_Close_On_Select=1
let Tlist_Display_Prototype=1
let Tlist_GainFocus_On_ToggleOpen=1
let Tlist_Highlight_Tag_On_BufEnter=1
let Tlist_Process_File_Always=0
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Matchit
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let b:match_ignorecase=1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" A.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:alternateExtensions_CPP="HPP,hpp,H,h,inc"
let g:alternateExtensions_cpp="hpp,HPP,H,h,inc"
let g:alternateExtensions_C="H,h,HPP,hpp,inc"
let g:alternateExtensions_c="h,H,HPP,hpp,inc"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Cscope
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"if has("cscope")
"set csprg=/ usr/local/bin/cscope
"set csto=0
"set cst
"set nocsverb
" add any database in current directory
"if filereadable("cscope.out")
"cs add cscope.out
" else add database pointed to by environment
"else
"cs add $VIM/cscope.out
" endif
"elseif $CSCOPE_DB != ""
"cs add $CSCOPE_DB
"endif
"set csverb
"endif
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Autocommands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Maximize gVim window after startup
"autocmd GUIEnter * simalt ~x
"autocmd BufEnter * let &titlestring = hostname() . ':' . expand('%f')
autocmd BufEnter * : syntax sync fromstart " ensure every file does syntax highlighting (full)
"auto jump to the directory where this file exisits
"autocmd BufEnter * lcd%:p:h 

augroup filesetting
autocmd FileType perl			call Filetype_perl()
autocmd FileType c				call Filetype_c()
autocmd FileType cpp			call Filetype_cpp()
autocmd FileType html           call Filetype_ml()
autocmd FileType ocaml          call Filetype_ml()
autocmd FileType xml            call Filetype_html()
autocmd FileType css            call Filetype_ml()
autocmd FileType tex            call Filetype_tex()
autocmd FileType lisp			call Filetype_lisp()
autocmd FileType mail			call Filetype_mail()
autocmd Filetype help			call Filetype_help()
augroup END

augroup filetypedetect
autocmd! BufRead *.nfo set encoding=cp437
autocmd! BufRead *.jsp set encoding=utf8
autocmd! BufRead *.otl setfiletype vo_base
autocmd! BufRead ~/mail/*        setlocal filetype=mail
autocmd! BufRead /tmp/mutt*      setlocal filetype=mail
autocmd! BufRead ~/.signature*   setlocal filetype=mail
autocmd! BufRead ~/.mutt/*       setlocal filetype=muttrc
autocmd! BufRead ~/.sawfish/custom setlocal filetype=lisp
autocmd! BufRead *.*html*        setlocal filetype=html
autocmd! BufRead *.blosxom       setlocal filetype=html
autocmd! BufRead *.css*          setlocal filetype=css
augroup END

" When editing a file,always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
\ if line("'\"") > 0 && line("'\"") <= line("$") |
\ exe "normal g`\"" |
\ endif

if version>=700
	if has("autocmd") && exists("+omnifunc")
		autocmd Filetype *
		\ if &omnifunc=="" |
		\ setlocal omnifunc=syntaxcomplete#Complete |
		\ endif
	endif
endif

"template
"autocmd BufNewFile *.c          0read ~/.vim/skel/skel.c
"autocmd BufNewFile *.cpp        0read ~/.vim/skel/skel.cpp
"autocmd BufNewFile *.java       0read ~/.vim/skel/skel.java
"autocmd BufNewFile *.plx        0read ~/.vim/skel/skel.plx

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"key Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
noremap <Space> <PageDown>
noremap <S-Space> <PageUp>
noremap j gj
noremap k gk

" switch lines
nmap <C-Down> :<C-u>move.+1<CR>
nmap <C-Up> :<C-u>move.-2<CR>
imap <C-Down> <C-o>:<C-u>move.+1<CR>
imap <C-Up> <C-o>:<C-u>move.-2<CR>
vmap <C-Down> :move '>+1<CR>gv
vmap <C-Up> :move '<-2<CR>gv

"jone line in insert mode
inoremap <C-j> <C-o>J

"cursor move
inoremap <C-a> <Home>
inoremap <C-e> <End>
"ex mode"
cnoremap <A-B> <S-Left>
cnoremap <A-F> <S-Right>
cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <C-D> <Del>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-N> <Down>
cnoremap <C-P> <Up>
cnoremap >fn <C-R>=expand('%:p')<CR>
cnoremap >fd <C-R>=expand('%:p:h').'/'<CR>
cnoremap >vim $VIMHOME/
cnoremap >ftp $VIMHOME/ftplugin/
cnoremap >tp  $VIMHOME/plugin/
cnoremap >rc $VIM/_vimrc
noremap <m-s-e> :e<space>**/*
noremap <m-e> :e<space>
noremap <m-g> :vimgrep<space>
noremap <m-s-n> :n<space>**/*

nnoremap  <leader>sf :vimgrep <C-R><C-W> %<CR>
nnoremap  <leader>sr :vimgrep <C-R><C-W> <C-R>=expand('%:p:h').'/**/*'<CR>

"windows compatible
nnoremap <C-S> :update<CR>
inoremap <C-S> <C-o>:update<CR>
vnoremap <C-S> <C-C>:update<CR>
nnoremap <C-Z> u
inoremap <C-Z> <C-o>u
vnoremap p <Esc>:let current_reg=@"<CR>gvdi<C-R>=current_reg<CR><Esc>
"tab opt
nnoremap <M-h> :tabp<CR>
nnoremap <M-l> :tabn<CR>
nnoremap <C-t> :tabnew<CR>
nnoremap <C-w> :tabclose<CR>
nnoremap <C-Tab> gt

nnoremap <M-j> <c-w>w
inoremap <M-j> <C-o><c-w>w

nnoremap <M-k> <c-w>W
inoremap <M-k> <C-o><c-w>W

nnoremap <C-F4>:confirm bd<CR>
inoremap <C-F4> <C-o>:confirm bd<CR>

nnoremap <M-n> :bn<CR>
inoremap <M-n> <C-o>:bn<CR>

nnoremap <M-p> :bp<CR>
inoremap <M-p> <C-o>:bp<CR>

nnoremap <M-d> :Tlist<CR>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"for winManager
"noremap <c-w><c-f> <ESC>:FirstExplorerWindow<cr>
"noremap <c-w><c-b> <ESC>:BottomExplorerWindow<cr>
"noremap <c-w><c-t> <ESC>:WMToggle<cr>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <silent> <F1> :He<CR>
nnoremap <silent> <F2> :MRU<CR>
"nnoremap <silent> <F3> :SelectBuf<CR>
nnoremap <silent> <S-F3> :BufExplorer<CR>
nnoremap <silent> <F4> :FSplit<CR>
nnoremap <silent> <S-F4> :YRShow<CR>
nnoremap <silent> <F5> :CalendarH<CR>

if has("win32")
	nnoremap <S-F6> :source $VIM/_vimrc<CR>
	if version >= 700
		nnoremap <silent>  <F6> :tabe $VIM/_vimrc<CR>
	else
		nnoremap <silent>  <F6> :e $VIM/_vimrc<CR>
	endif
else
	nnoremap <S-F6> :source $HOME/.vimrc<CR>
	if version >= 700
		nnoremap <silent>  <F6> :tabe $HOME/.vimrc<CR>
	else
		nnoremap <silent>  <F6> :e $HOME/.vimrc<CR>
	endif
endif
nnoremap <silent>  <F7> :cp<CR>
nnoremap <silent>  <F8> :cn<CR>
nnoremap <silent>  <F10> :QFix<CR>
nnoremap <silent>  <F11> :TlistToggle<CR>
autocmd FileType c,cpp nnoremap <silent> <buffer>  <F12> mm:%!astyle --style=ansi --convert-tabs -O -p -L -T<CR> :update<CR>'m
autocmd FileType c,cpp inoremap <silent> <buffer>  <F12> <Esc>mm:%!astyle --style=ansi -T<CR> :update<CR>'m
nnoremap <silent>  <S-F1> :set tags=./tags,tags,h:/Gemini_Rel4.1/gemini_delivery/src/tags<CR>
nnoremap <silent>  <C-F1> :set tags=./tags,tags,h:/Mercury_Rel6.2_M610/mercury_delivery/src/tags<CR>

" Toggle spell check
" For VIM7 only
if version >= 700
	nmap <silent>  <C-F11> :set spell!<CR>
	imap <silent>  <C-F11> <C-o>:set spell!<CR>
endif

" Toggle line number
nmap <silent>  <C-F12> :set nu!<CR>
imap <silent>  <C-F12> <C-o>:set nu!<CR>
"
"key mapping end
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"custom plugin settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"for calendar
let g:calendar_weeknm=1 " WK01
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"c support
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:C_AuthorName     ='Julian'
"let g: C_AuthorRef='Mn'
let g:C_Email='junist@gmail.com'
let g:C_Company='FreeLand.'
"set dictionary="$VIMHOME/wordlists/c-c++-keywords.list"
"set dictionary+='$VIMHOME/wordlists/k+r.list'
"set dictionary+='$VIMHOME/wordlists/stl_index.list'
if has("win32")
	let g:C_ObjExtension=".obj"
	let g:C_ExeExtension=".exe"
	let g:C_CCompiler="bcc32.exe"
	let g:C_CplusCompiler="bcc32.exe"
	let g:C_CFlags="-6 -A -v -c"
	let g:C_LFlags="-6 -A -v"
	let g:C_Libs="-lm"
	let g:C_CExtension="c"
	let g:C_Comments="no"
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"hi_pern
let loaded_matchparen=0
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"cppomnicpplete
"
let CppOmni_ShowScopeInAbbr = 1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"netrw
"let g:netrw_scp_cmd="pscp - q"
let g:netrw_liststyle = 0
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"xml.vim
"let xml_use_xhtml = 1   
"let xml_no_html = 1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"MRU
let MRU_Max_Entries = 20
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"lookup file
let g:LookupFile_PreservePatternHistory = 0
let g:LookupFile_ShowFiller = 0
let g:LookupFile_PreserveLastPattern = 0
let g:LookupFile_AlwaysAcceptFirst = 1
let g:LookupFile_FileFilter = '^\.#\|\d+\.\d+$'
let g:LookupFile_TagExpr = '"H:/Gemini_Rel4.1/filenametags"'
"large file
let g:LargeFile = 10	"file size bigger than 30M will be treated as large file
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"utl.vim
let g:utl_config_highl = 'on'
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"abbr
iabbr teh the
iabbr #d #define
iabbr #b /************************************************
iabbr #e ************************************************/
iabbr rt return TRUE;
iabbr rf return FALSE;
iabbr jt <c-r>=strftime("%Y-%m-%d")<cr>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"personal settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! Fcvs :source $VIM/cvsfix.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! ClosePair(char)
	if getline('.')[col('.') - 1] == a:char
		return "\<Right>"
	else
		return a:char
	endif
endf 

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if !(has("gui_running"))
	call Term_keymap()
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" make ` functions <C-O> in insert mode
inoremap ` <C-O>
nnoremap ` i`<ESC>
" when you want to comment some code
" I//<ESC>, move to next line, V(visual-mode),j(jump to the end of code),
" then press ., complete
vnoremap . :normal .<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" my setting:
syntax  on
syntax  enable

set nomodeline
set nobackup
" set writebackup
" set backupdir=~/.vim/bak
set nocindent
set incsearch
set nocompatible
set ignorecase
set ruler
set noautoindent
set showcmd
set shiftwidth=4
set softtabstop=4
set foldmethod=syntax
set foldnestmax=0
vnoremap p <Esc>:let current_reg = @"<CR>gvs<C-R>=current_reg<CR><Esc>
highlight Comment ctermfg=darkcyan

filetype plugin indent on

