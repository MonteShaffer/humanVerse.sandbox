

# DATA
	# loop over teams ... teams ... 
	# HTML or CSV
		# HTML ... parse it
	
# NOTEBOOK ... do the pair-wise analyses ... 
	
# MATRIX
	# assumptions about "weighting" and truncation
		# rank by team ... separately from a global NHL rank
		# minimum time on ice
		# CF/min or CA/min ... 
		# prop ... 
# DONE 		


NHL.getURLTemplates = function()
	{
	templates = list();
		templates$linecombo = "https://frozenpool.dobbersports.com/frozenpool_linecombo.php?select={position}&teamf={team}&games={season}%3AR%3A99&period={period}&situation={strength}"
	templates;
	}
	
NHL.parseURL = function(which="linecombo", team="DAL", position="TF", season="2020-2021", period="ALL", strength="EV")
	{
	urls = NHL.getURLTemplates();
	url = urls[[which]]
	
	url = str_replace("{team}", 	team, url);
	url = str_replace("{position}", position, url);
	url = str_replace("{season}", 	season, url);
	url = str_replace("{period}", 	period, url);
	url = str_replace("{strength}", strength, url);
	
	url;
	}



NHL.getTeams = function()
	{
	url = NHL.parseURL("linecombo");
	
	html = getRemoteAndCache(url);
	html.str = readStringFromFile(html);

	start = '<select name="teamf" id="teamf" class="form-control">';
	end = '</select>';

	html.info = parse.sliceDiceContent(html.str, start=start, end=end, strip=FALSE);

	tmp = explodeMe("</option>", html.info);
	teams = c();
	for(row in tmp)
	  {
	  row = trimMe(strip_tags(row));
	  if(row != "" && row != "Select Team F")
		{
		teams = c(teams, row);
		}
	  }
	teams;
	}

