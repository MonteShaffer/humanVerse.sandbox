## http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
		## last one is HTML, I can download it ... AS A DEMO, once everything is DONE ...
	
## MAP to colors() ... names
## MAP to GIST codes 

cat("\x1B[38;5;249m\x1B[48;5;220mr:5 g:4 b:0 \x1B[49m\x1B[39m")
		# 249m ??? color index 
		# https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
		# cat("\x1B[38;5;31m\x1B[48;5;42mr:5 g:4 b:0 \x1B[49m\x1B[39m")
		## BRIGHT vs BOLD
		## https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797?permalink_comment_id=4127256#gistcomment-4127256
		
		# \033[38;2;<R>;<G>;<B>;48;2;<on_R>;<on_G>;<on_B>m
		# The first three numbers are the foreground color, and the last three are the background color.
		
		# cat("\033[38;2;255;0;255;48;2;0;255;0m", "hello", sep="");
		# this above works in DEBIAN, no reset so stays that color ...
		# 
		# cat("\033[38;2;255;0;255;48;2;0;255;0m", "hello", "\x1B[49m\x1B[39m", sep="");
		# cat("\033[38;2;255;0;255;48;2;0;255;0m", "\n\n", "hello", "\n", "\x1B[49m\x1B[39m", "\n\n", sep="");
		## ;2; VERSION works on DEBIAN, not RSTUDIO WINDOWS
		## ;5; THIS NEEDS A MAP OF COLORS ... 
		# cat("\033[38;5;31m", "\033[38;5;42m", "\n\n", "hello", "\n", "\x1B[49m\x1B[39m", "\n\n", sep="");
		# # Set style to bold, red foreground.
		# cat("\x1b[1;31mHello");
		# Set style to dimmed white foreground with red background.
		# cat("\x1b[2;37;41mWorld);
		
		## https://stackoverflow.com/questions/27159322/rgb-values-of-the-colors-in-the-ansi-extended-colors-index-17-255#:~:text=The%20color%20range%20of%20a,are%20the%20%22bright%22%20colors.
		## colors 1-8, 1-16 ARE platform specific ... can I get their RGB/HEX ranges
		## colors 255 + GREAYS are not 
		## library(crayon) has done the work, NO?
		## TABLE...
		## https://www.ditig.com/256-colors-cheat-sheet
		## https://vim.fandom.com/wiki/Xterm256_color_names_for_console_Vim
		## http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
		## last one is HTML, I can download it ... AS A DEMO, once everything is DONE ...
		## # ... (\#{1}.{6}) ... REPLACE BY ... \1\n
		## HEX REG 
		# https://regex101.com/r/TsnjUG/1
		
		
				
		
		msg = str.replace("{humanVerse}", 
							paste0("purple", "{human", "\x1B[38;2;255;0;0m", "green", "Verse}", "/green"), 
							msg);




ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
                     "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
                     "|\\x{001b}[A-M]");
					 
					 
has_style <- function(string) {
  grepl(ansi_regex, string, perl = TRUE)
}

strip_style <- function(string) {
  gsub(ansi_regex, "", string, perl = TRUE, useBytes = TRUE)
}


cat.init() = function()
	{
	# load objects, where to attach?
	
	# [ESC]
	# colors8, colors16, colors256
	# <i><b><fg color="#ff0000"><bg color="black"></bg></fg></b></i><reset />
	# use HTML-like tags ... 
	# <esc custom="a" more="b"></esc>  # a generic ESC event 
	# COLOR is attached to either <fg> or <bg>
	
	
	
	}



# ESC[38;2;{r};{g};{b}m ... set foreground (fg) color [NOT OPEN/CLOSE]
# https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
# ESC[48;2;{r};{g};{b}m
# will any RGB work, or do I have to use these ANSI
# 8 + 216 + grays ... websafe 

## CLOSE is actually a RESET sequence for the element 
## ESC == "\033" OCTAL ... hex "\x1B" also works, unicode = "\u001b"

## \x1B[38;2;255;0;0m
## cat("\033[38;5;249m\033[48;5;220mr:5 g:4 b:0 \033[49m\033[39m")
## cat("\x1B[38;5;249m\x1B[48;5;220mr:5 g:4 b:0 \x1B[49m\x1B[39m")

cat.checkColor = function(ESC = "\u001b")
	{
	
	}


cat.prepString = function() {}

cat.withColor = function()
	{
	cat(...);
	}
	
# no need for cat.color ... just call cat ...
# cat(open,"monte",close, sep="")

color.cat = function(str, color, options = c("fg", "bg", "italic")) {}
## see head( crayon::styles() )
## str = paste0(styles()$underline$open, "monte", styles()$underline$close ); cat(str);
## names(styles())
## key = "underline"; 
## testcat = function(key) { open = styles()[[key]]$open; close = styles()[[key]]$close; str = paste0("\t",key,"\t",  open, " -->monte<-- ", close ); cat("\n", str, "\t", "OPEN: ", open, "\t", "CLOSE: ", close, "\n"); }
## for(key in names(styles())) { testcat(key); }
## here is ANSI stuff ... code list
## https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
# fgcodes <- c(paste0('\x1b[38;5;', 0:255, 'm'), '\x1b[39m')
# bgcodes <- c(paste0('\x1b[48;5;', 0:255, 'm'), '\x1b[49m')
		# could "cat" have a color?
		# https://www.r-project.org/nosvn/pandoc/crayon.html
		# It is easy to define your own themes:
		# only works in RStudio, not RGui (windows)
		# works in RGui on Debian ... 
		# Does the current R session support ANSI colors?
		# num_ansi_colors() > 1L
		# has_hyperlink
		## Are we in a terminal? No?
		# if (!isatty(stdout())) { return(FALSE) }
		# ansi_colors_256()
		# show_ansi_colors()
		# styles()
		
							
		