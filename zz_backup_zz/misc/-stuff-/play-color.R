





# we need SMART ... if #11223344 ... then alpha = TRUE ... 


check.hex = function(hexstr, ..., prepend="0x", case="upper")
	{
	hexstr = dots.addTo(hexstr, ...)
	hexstr = str.trim(str.replace(c("0x","#"), "", hexstr));
	# rather than checking, let's remove and add leading "0x"
	hexstr = paste0(prepend, str.trim(tolower(hexstr)));
	hexstr;	
	}
	
# dechex() - Decimal to hexadecimal
# bindec() - Binary to decimal
# octdec() - Octal to decimal
# base_convert() -  
# base_convert(string $num, int $from_base, int $to_base): string
# https://stackoverflow.com/questions/64378066/how-can-i-convert-between-numeral-systems-in-r
# https://stackoverflow.com/a/64380219/184614
# BASE MATHS with above ... I just want simple "character" to number ...
	
	
	
# dec2hex = function(..., to.length=to.length) { base.convert(..., from="DEC", to="HEX", to.length=to.length); }







# hex2dec
# dec2oct
# oct2dec
# dec2bin
# bin2dec

# bin2hex ... do all COMBOS ...
	
	

# # maybe go back and README on colors and do color.convert ... to/fro on RGB

# # library BMS? 

	
# # bin2hex, hex2bin, and so on ...
 




# imagemagick has get nearest color function ...


# color.id("#cc00cc") ... uses min SS^2 in plotrix
# not cosine

 
 
# Sophia will have several important modules named in homage to 'oldschool' *AMAZING WARP CORE* technologies that were great: Cairn [a la WordPerfect 5.1/5.2], Lotus-V [Lotus 1-2-3], Chatty [chat rooms: IRC, CompuServe, AOL to SKYPE/VIBER/Telegram/WhatsApp/ZOOM], Nappy [Napster {encode/decode A/V}, Screencasting/Streaming ... Spotify, Youtube?], Carmen [whereis 'Carmen SanDiego': is this Mappy: openstreets/Google Earth], Oracle [file management, search engine, dictionary access {1828/1844/1913; Urban; Guarani 1600, Egyptian 1800}, and so on], Maria [whenis: calendar/planner/scheduler time and date management Franklin/Covey], Floppy [content sharing: Dropbox, Drives, NextCloud, OwnCloud], Angie [hosting: Angelfire, Geocities], Slacker [Reddit / SO / Quora / Chegg => Cheff => Che-LOCO], Sparrow [Pirate themed], and Iris [Homage to Horticulturalist/RA a la Photoshop 6.0].  TWELVE modules, feels correct!  Built on DAMP:  Debian, Apache, MariaDB [mysql], and PHP (localhost or deployed) with git version control (gitlabs?) and powered by R.  All for the low cost of FREE-99-FOREVER.  No ads, no interruptions, ever.  It is time! 