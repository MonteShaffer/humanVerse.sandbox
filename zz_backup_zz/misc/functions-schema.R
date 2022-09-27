
library(readxl);

# xlsfile = "Q:/_git_/github/DataWar/CRAN/misc/CRANv2.xlsx";
# outpath = "Q:/_git_/github/DataWar/CRAN/v2020/SQL/"; createDirectoryRecursive(outpath);
# mysql.XLStoInsert(xlsfile, outpath);
mysql.XLStoInsert = function(xlsfile, outpath="SQL")
	{
	if(dir.exists(outpath)) 
		{ 
		basedir = outpath; 
		} else 	{ 
				basedir = paste0( dirname(xlsfile), "/", outpath);				
				}
				
				
		dir.schema 		= cleanup.local( paste0(basedir, "/", "01-database-schema", "/") );
							createDirectoryRecursive(dir.schema);
		dir.insert 		= cleanup.local( paste0(basedir, "/", "02-database-insert", "/") );
							createDirectoryRecursive(dir.insert);
		dir.copy 		= cleanup.local( paste0(basedir, "/", "02-database-copy", "/") );
							createDirectoryRecursive(dir.copy);
		dir.indexing 	= cleanup.local( paste0(basedir, "/", "03-database-indexing", "/") );
							createDirectoryRecursive(dir.indexing);
							
		all.schema 		= paste0(dir.schema, "-ALL-.sql");
		all.insert 		= paste0(dir.insert, "-ALL-.sql");
		all.indexing 	= paste0(dir.indexing, "-ALL-.sql");
			

	templates = list();
	templates$create 	= '
						CREATE TABLE IF NOT EXISTS `{mytable}` (
																{my.columns}
																);';

	templates$column 	= '
						`{mycolumn}` {mytype} {myauto} {mysigned} {mynull} {mydefault} {mycomma}
							';
							
	
	templates$alter  	= '
						ALTER TABLE `{mytable}`
												{my.alters}
						COMMIT; ';
						
	templates$index  	= '
						ADD KEY `{index.key}` (`{index.key}`) ';

	templates$primary  	= '
						ADD PRIMARY KEY (`{primary.key}`) ';
						
		
	tabs = readxl::excel_sheets(xlsfile);
	for(tab in tabs)
		{
		my.sheet = as.data.frame(  readxl::read_excel(xlsfile, sheet=tab) );
		
		
		
		
		
		
		rinfo = my.sheet[1, ];
		if(is.null(rinfo$variable)) { break; }
		printPaste0(" -------- TAB :: ", tab, "\n");
		
		
		create = templates$create;
			create = str_replace("{mytable}", tab, create);
			
		alter  = templates$alter;
			alter = str_replace("{mytable}", tab, alter);
		
		mycols = "";
		myalts = "";
		
		nr = nrow(my.sheet);
		for(i in 1:nr)
		  {
		  rinfo = my.sheet[i, ];
			
			mycol = templates$column;
				# `{mycolumn}` {mytype} {myauto} {mysigned} {mynull} {mydefault} {mycomma}
				mycol = str_replace("{mycolumn}", 	rinfo$variable, mycol);
				mycol = str_replace("{mytype}", 	rinfo$type, mycol);
					myauto = ""; if(!is.null(rinfo$auto)) { myauto = "AUTO_INCREMENT"; }
				mycol = str_replace("{myauto}", 	myauto, mycol);
					mysigned = ""; if(!is.null(rinfo$signed)) { mysigned = "UNSIGNED"; }
				mycol = str_replace("{mysigned}", 	mysigned, mycol);
					mynull = "NOT NULL"; if(!is.null(rinfo$null)) { mynull = "NULL"; }
				mycol = str_replace("{mynull}", 	mynull, mycol);
					mydefault = ""; if(!is.null(rinfo$default)) { mydefault = paste0("DEFAULT ", rinfo$default); }  # ZEROFILL
				mycol = str_replace("{mydefault}", 	mydefault, mycol);
					mycomma = ""; if(i != nr) { mycomma = " , "; }  
				mycol = str_replace("{mycomma}", 	mycomma, mycol);
				
			mycols = paste0( mycols, mycol, "\n");	

			myalt = "";
			if(!is.na(rinfo$key))
				{
				myalt = templates$primary;
				myalt = str_replace("{primary.key}", rinfo$variable, myalt);
				} else	{
						if(!is.na(rinfo$key))
							{
							myalt = templates$index;
							myalt = str_replace("{index.key}", rinfo$variable, myalt);
							}
						}
			
			myalts = paste0( myalts, myalt, "\n");
			
		  }
		  
		create = str_replace("{my.columns}", mycols, create);
		file.schema = paste0(dir.schema, tab, ".sql");
		
		writeLine(create, file=file.schema, append = FALSE);
		writeLine(create, file=all.schema,  append = TRUE);
		
		
		alter = str_replace("{my.alters}", myalts, alter);
		file.indexing = paste0(dir.indexing, tab, ".sql");
		
		if(trimMe(myalts) != "")
			{		
			writeLine(alter, file=file.indexing, append = FALSE);
			writeLine(alter, file=all.indexing,  append = TRUE);
			}
		  
		}
	# inserts to be done later ...
	
	}
	