library(openNLP);
library(NLP);
library(tm);
library(quanteda);
library(SentimentAnalysis);

##################### nlp.str #####################

NLP.cleanupHTMLentities = function(str, find="fs", replace="rs") 
  {
  fs = c("&lsquo;", "&rsquo;", "&mdash;");  # mdash is not a hyphen.
  rs = c("'"      , "'"      , " -- ");       # ASCII replaces
  cs = c("^[^"    , "^]^"    , " ^-^ ");      # custom replaces   
  nfs = length(fs);
  for(i in 1:nfs)
    {
    myf = eval(parse(text = find));
    myr = eval(parse(text = replace));
    
    str = gsub(myf[i],myr[i], str, fixed=TRUE);
    }
  str;
  }


NLP.cleanupAndPerformReadability = function(str)
{
 
  #str = gsub("{``}",'"',str,fixed=TRUE);
  #str = gsub("``",'"',str,fixed=TRUE);
  #str = gsub("`","'",str,fixed=TRUE);
  str = gsub("—"," -- ",str,fixed=TRUE);
  # \u0097
  
  # Micro$oft 
  str = gsub("’","'",str,fixed=TRUE);
  str = gsub("”",'"',str,fixed=TRUE);
  str = gsub("“",'"',str,fixed=TRUE);
  periods = length(strsplit(str,".",fixed=TRUE)[[1]]);
  str = NLP.removePunctuationFromString(str);
  str = removeWhiteSpace(str);
  
  
  str.vec  = strsplit(str," ",fixed=TRUE)[[1]];


  str.s = countSyllablesInWord(str.vec);
  str.r = computeReadability(periods,str.s);  # one sentence

  
  
str.r;
}



NLP.prepStringGram = function(str, rm.pipe=FALSE, extra=NULL)
  {
  str = gsub("^[^","'",str,fixed=TRUE);
  str = gsub("^]^","'",str,fixed=TRUE);
  str = gsub("^-^"," | ",str,fixed=TRUE);
  str = gsub("-"," ",str,fixed=TRUE);  # should wood-cutter become wood cutter or woodcutter
  str = gsub("\r\r\n"," | ",str,fixed=TRUE);
  str = gsub("\r\n"," | ",str,fixed=TRUE);
  str = gsub("\n"," | ",str,fixed=TRUE);
  
  
  str = gsub("’","'",str,fixed=TRUE);
  str = gsub("”",'"',str,fixed=TRUE);
  str = gsub("“",'"',str,fixed=TRUE);
  
  ne = length(extra);
  if(ne > 0)
    {
    for(i in 1:ne)
      {
      str = gsub(extra[i]," | ",str,fixed=TRUE);
      }
    }
  
  if(rm.pipe)
    {
    str = gsub("|"," ",str,fixed=TRUE);
    }
  str = removeWhiteSpace(str); 
  str;
  }





NLP.cleanupTitles = function(strvec)
  {
  n = length(strvec);
  res = c();
  for(j in 1:n)
    {
    str = strvec[j];
    
    fi = c("-"," ",",");
    for(f in fi)
      {
      str = gsub(f,".",str,fixed=TRUE);
      }
    for(i in 1:5)
      {
      str = gsub("..",".",str,fixed=TRUE);  
      }
      #str;
    res = c(res,str);
    }
  res;
  }

 

NLP.prepareOneStoryGRIMM = function(df.grimm, path.to.grimm,
                          title, title.f,
                          my.stopwords = NULL
                          )
  {
  timer.start = as.numeric(Sys.time());
      df.story = subsetDataFrame(df.grimm, "title", "==", title);
        chap.n = df.story$chap.n[1];
      out.txt = paste0(path.to.grimm, title.f, ".txt");
  stop.key = paste(my.stopwords, collapse="-");
  # no stopwords, no md5 key, just a hyphen
  stop.md5 = md5(stop.key);  if(stop.key == "") { stop.md5 = ""; }
      out.rds = paste0(path.to.grimm, title.f, "-", stop.md5 , ".rds"); # stats summary
      out.start = gsub(".rds",".has-started", out.rds);
      
  print(out.rds);    
  if(!file.exists(out.rds))  
    {
    Sys.sleep(runif(1,0.2,1.3));
    print(out.start);
    if(file.exists(out.start)) { return(FALSE); } # skip
    writeLine("hello there friend!", out.start, append=FALSE);
    
      my.story = paste0(df.story$para.text, collapse=" \r\n ");
      if(!file.exists(out.txt))
        {
        writeLine(my.story, out.txt, append=FALSE);
        }
  
      words.r = getRawWords(my.story);
  
      timer.txt = as.numeric(Sys.time());
      elapsed = round( (timer.txt - timer.start), 2);
      print(paste0("story: ... ", title.f, " ... [txt] in ",elapsed," secs"));
      
      info.s = list();
        info.s$general = list();
        info.s$general$sentiment = NLP.doSentimentAnalysis(my.story); # expensive
        info.s$general$readability = as.data.frame(t(unlist(NLP.cleanupAndPerformReadability(my.story))));
        info.s$general$punctuation = NLP.countPunctuation(my.story);
        info.s$general$case = as.data.frame(t(unlist(NLP.countWordsInString(my.story))));
        info.s$general$PP = NLP.countPersonalPronouns(words.r);
        info.s$general$GENDER = NLP.countGenderLanguage(words.r);
        info.s$general$CUSTOM = NLP.countCustomWordList(words.r);
      
      timer.general = as.numeric(Sys.time());
      elapsed = round( (timer.general - timer.txt), 2);
      print(paste0("story: ... ", title.f, " ... [general] in ",elapsed," secs"));
      
        info.s$sentences = buildNgrams(my.story,5, my.stopwords = my.stopwords);   
      
      timer.ngrams = as.numeric(Sys.time());
      elapsed = round( (timer.ngrams - timer.general), 2);
      print(paste0("story: ... ", title.f, " ... [ngrams] in ",elapsed," secs"));
      
      # save File
      
      my.result = info.s;
        saveRDS(my.result, out.rds);
      } else { my.result = readRDS(out.rds); }


  timer.end = as.numeric(Sys.time());
  elapsed = round( (timer.end - timer.start), 2);
  print(paste0("story: ... ", title.f, " ... [--TOTAL--] in ",elapsed," secs"));
  my.result;
  }


NLP.summarizeGENDER = function(GENDER)
  {
  # one-row dataframe
  # CROSS-TABS
  genders = c("male", "female");
  numbers = c("singular","plural");
  
  keys = "ALL";
  vals = sum(GENDER$count, na.rm=TRUE);
  for(gender in genders)
    {
    sub = subsetDataFrame(GENDER, "gender", "==", gender);
    keys = c(keys, gender);
    vals = c(vals, sum(sub$count, na.rm=TRUE) );
    }
  for(number in numbers)
    {
    sub = subsetDataFrame(GENDER, "number", "==", number);
    keys = c(keys, number);
    vals = c(vals, sum(sub$count, na.rm=TRUE) );
    }
  
  df = as.data.frame( rbind(NULL,vals));
    rownames(df) = NULL;
    colnames(df) = paste0("GENDER.",keys);
  df;
  }

NLP.summarizePP = function(PP)
  {
  # one-row dataframe
  # CROSS-TABS
  persons = c("first","second","third");
  numbers = c("singular","plural");
  
  keys = "ALL";
  vals = sum(PP$count, na.rm=TRUE);
  for(person in persons)
    {
    sub = subsetDataFrame(PP, "person", "==", person);
    keys = c(keys, person);
    vals = c(vals, sum(sub$count, na.rm=TRUE) );
    }
  for(number in numbers)
    {
    sub = subsetDataFrame(PP, "number", "==", number);
    keys = c(keys, number);
    vals = c(vals, sum(sub$count, na.rm=TRUE) );
    }
  
  df = as.data.frame( rbind(NULL,vals));
    rownames(df) = NULL;
    colnames(df) = paste0("PP.",keys);
  df;
  }




NLP.summarizeMatrix.POS = function(which="ALL", df.grimm,
					path.to.grimm, my.stopwords = NULL, 
					nfeature=".tags", ngrams=5
                              )
  {
  titles = unique(df.grimm$title);
  titles.f = cleanupTitles(titles);
    nt = length(titles);
    
  if(is.character(which)) 
    {
    if(which == "ALL") 
      { 
      idx = 1:nt; 
      } else { 
              idx.t = which(titles == which);
              idx.f = which(titles.f == which);
              
              idx = unique( c(idx.t, idx.f) )[1];
              }
    } else { idx = which; } # numeric
    
    ## idx = 20; # "HANSEL AND GRETEL";
  ni = length(idx);
  
  all.features = c();
  s.list = list();
  for(i in 1:ni)
    {
	# single story, we don't need to loop over sentences?  
    my.idx = idx[i];
    title = titles[my.idx];
    title.f = titles.f[my.idx];
  
    one = prepareOneStory(df.grimm, path.to.grimm, 
                          title, title.f,
                          my.stopwords = my.stopwords
                          );
    
    # names(one$general);
    myfeatures = one$sentences$grams[[nfeature]][[ngrams]];
	all.features = unique(c(all.features,myfeatures));
	
    s.list[[i]] = myfeatures;	
	}
## this will be slow
  s.df = NULL;
  naf = length(all.features);
  for(i in 1:ni)
    {
	print(titles[i]);
	myfeatures = s.list[[i]];
	row = numeric(naf);
	
	nf = length(myfeatures);
	for(j in 1:nf)
		{
		idx = which(myfeatures[j] == all.features);
		row[idx] = 1 + row[idx];
		}  
    s.df = rbind(s.df, row);
    }
    
  s.df = as.data.frame(s.df);
	colnames(s.df) = all.features;
	rownames(s.df) = titles;
  
  s.df;
  }



NLP.summarizeCustom = function(which="ALL", df.grimm)
  {
  titles = unique(df.grimm$title);
  titles.f = cleanupTitles(titles);
    nt = length(titles);
    
  if(is.character(which)) 
    {
    if(which == "ALL") 
      { 
      idx = 1:nt; 
      } else { 
              idx.t = which(titles == which);
              idx.f = which(titles.f == which);
              
              idx = unique( c(idx.t, idx.f) )[1];
              }
    } else { idx = which; } # numeric
    
    ## idx = 20; # "HANSEL AND GRETEL";
  ni = length(idx);
  
  s.df = NULL;
  for(i in 1:ni)
    {
    my.idx = idx[i];
    title = titles[my.idx];
    title.f = titles.f[my.idx];
	
	df.story = subsetDataFrame(df.grimm, "title", "==", title);
	my.story = paste0(df.story$para.text, collapse=" \r\n ");
	words.r = getRawWords(my.story);
	words.c = countCustomWordList(words.r);
	
	my.count = as.numeric(words.c$count);
	my.words = words.c$word;
	
	s.df = rbind(s.df, my.count);
    }
    
  s.df = as.data.frame(s.df);
	rownames(s.df) = titles[idx];
	colnames(s.df) = my.words;
  
  s.df;
  }

 
NLP.summarizeGeneral = function(which="ALL", df.grimm, path.to.grimm,
                            my.stopwords = NULL
                              )
  {
  titles = unique(df.grimm$title);
  titles.f = cleanupTitles(titles);
    nt = length(titles);
    
  if(is.character(which)) 
    {
    if(which == "ALL") 
      { 
      idx = 1:nt; 
      } else { 
              idx.t = which(titles == which);
              idx.f = which(titles.f == which);
              
              idx = unique( c(idx.t, idx.f) )[1];
              }
    } else { idx = which; } # numeric
    
    ## idx = 20; # "HANSEL AND GRETEL";
  ni = length(idx);
  
  s.df = NULL;
  for(i in 1:ni)
    {
    my.idx = idx[i];
    title = titles[my.idx];
    title.f = titles.f[my.idx];
  
    one = prepareOneStory(df.grimm, path.to.grimm, 
                          title, title.f,
                          my.stopwords = my.stopwords
                          );
    
    # names(one$general);
    
    count.sentences = nrow(one$sentences$sentiment);
    

      
    my.sentiment = as.data.frame( rbind(NULL,one$general$sentiment));
        rownames(my.sentiment) = NULL;
    colnames(my.sentiment) = c("S.POS","S.NEG");
      
    
    row = cbind(my.idx, title, 
                    count.sentences,
                    one$general$case,
                    one$general$readability
                  );
    
    # the other was from raw text, this is from "clean" text
      clean.FK = unlist(computeFleshKincaid(row[3], row[4], row[10]));
    new.FK = as.data.frame( rbind(NULL,  clean.FK ));
        rownames(new.FK) = NULL;
    colnames(new.FK) = c("FRE.N","FKGL.N");
    
    row = cbind(row, new.FK,
                    one$general$punctuation,
                    summarizePP(one$general$PP),
                    summarizeGENDER(one$general$GENDER),
                    my.sentiment);
  
    s.df = rbind(s.df, row);
    }
    
  s.df = as.data.frame(s.df);
  rownames(s.df) = NULL;
  
  s.df;
  }



NLP.element.exists <- function(var, element)
{
  tryCatch({
    if(length(var[[element]]) > -1)
      return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
} 
 
NLP.parseGutenberg.GRIMM = function(path.to.grimm,
                        file.stem = "fairytales",
                        txt.file.remote = "https://www.gutenberg.org/files/2591/2591-0.txt",
                        html.file.remote = "https://www.gutenberg.org/files/2591/2591-h/2591-h.htm",
                        my.local.path = path.to.gutenberg
      )
  {
  if(!element.exists(local.data.path))
    {
    # must exist for current grabHTML to work
    .GlobalEnv$local.data.path = my.local.path;
    }
  
  txt.file.local = paste0(path.to.grimm, file.stem, ".txt");
    my.txt = grabHTML(txt.file.local, txt.file.remote);
  html.file.local = paste0(path.to.grimm, file.stem, ".html");
    my.html = grabHTML(html.file.local, html.file.remote);
  
  cache.file.local = paste0(path.to.grimm, file.stem, ".rds");
  
  timer.start = as.numeric(Sys.time());


if(!file.exists(cache.file.local))
{
  df.grimm = NULL;
chap.n = 0;
para.n = 1;
title = "THE BROTHERS GRIMM FAIRY TALES";
what  = "-INTRO-";

### let's trim around gutenberg stuff
start = "*** START OF THIS PROJECT GUTENBERG EBOOK GRIMMS' FAIRY TALES ***";
end = "End of Project Gutenberg";

tmp = strsplit(my.html, end, fixed=TRUE)[[1]];     # str(tmp);
tmp1 = strsplit(tmp[1],  start, fixed=TRUE)[[1]];  # str(tmp1);

n.html = tmp1[2];

# there is one note ...  <div class="mynote">
start = '<div class="mynote">';
end = '</div>';

tmp = strsplit(n.html, start, fixed=TRUE)[[1]];     # str(tmp);
f.html = tmp[2];
tmp1 = strsplit(tmp[3],  end, fixed=TRUE)[[1]];     # str(tmp1);





note = cleanupHTMLentities(tmp1[1],replace="cs");
note = strip_tags(note);
# note = removeWhiteSpace(note);
note = trimMe(note);
type = "note";

row = c(chap.n, what, title, para.n, type, note);
df.grimm = rbind(df.grimm, row);
  chap.n = 1+chap.n;
  para.n = 1;


### h2
tmp = strsplit(f.html,"<h2>", fixed=TRUE)[[1]];
idx.stories = 2:63

for(i in idx.stories)
  {
  story = tmp[i];
  what = paste0("STORY-",chap.n);
    story = cleanupHTMLentities(story,replace="cs");
    #story = removeWhiteSpace(story);
    story = trimMe(story);
  # story title
  stmp = strsplit(story, "</h2>", fixed=TRUE)[[1]]; 
  story.title = trimMe(stmp[1]);
  
  story = trimMe(stmp[2]);
  # let's keep paragraphs for a minute ...  <p> and <pre>
  # because of location of <pre>, I will explode on ending </p>
  # we will count <pre> as a paragraph with a poem flag ... # NOT PERFECT: it doesn't allow for multiple poems within a paragraph, see JORINDA.
  ptmp = strsplit(story, "</p>", fixed=TRUE)[[1]]; 
  
  # paragraphs = list();
  n.p = length(ptmp);
  k = 1;
  for(j in 1:n.p)  # i = 3; j = 12
    {
    para = ptmp[j];
    qtmp = strsplit(para, "<p>", fixed=TRUE)[[1]]; 
      poem = trimMe(strip_tags(qtmp[1]));
      if(is.na(poem)) { poem = ""; }
      if(poem != "")
        {
        # pinfo = list("type" = "poem", "text" = poem);
        # paragraphs[[k]] = pinfo;
        row = c(chap.n, what, story.title, k, "poem", poem);
        df.grimm = rbind(df.grimm, row);
        k = 1 + k;
        }
      prose = trimMe(strip_tags(qtmp[2]));
      if(is.na(prose)) { prose = ""; }
      if(prose != "")
        {
        prose = removeWhiteSpace(prose); # poems will keep line breaks
        # pinfo = list("type" = "prose", "text" = prose);
        # paragraphs[[k]] = pinfo;
        row = c(chap.n, what, story.title, k, "prose", prose);
        df.grimm = rbind(df.grimm, row);
        k = 1 + k;
        }
    ###
    }
  ### end of single story ... 
  chap.n = 1+chap.n;
  }


df = as.data.frame(df.grimm);
  colnames(df) = c("chap.n", "chap.type", "title", "para.n", "para.type", "para.text");
  rownames(df) = paste0(df$chap.n,"-",df$para.n);
df$chap.n = as.numeric(df$chap.n);
df$para.n = as.numeric(df$para.n);

df.grimm = df;
saveRDS(df.grimm, cache.file.local);
    } else { df.grimm = readRDS(cache.file.local); }

timer.end = as.numeric(Sys.time());
timer.elapse = timer.end-timer.start;
print(paste0("Time elapsed: ", round(timer.elapse,2) ));
  
df.grimm;
  
  }
  
  
##################### nlp.stack #####################

NLP.rowMaxColumn = function(df)
  {
  nrow = nrow(df);
  rmax = numeric(0);
  for(i in 1:nrow)
    {
    row = df[i, ];
    rmax[i] = which.max(row); # downward biased
    }
  rmax;
  }

NLP.rowMax = function(df)
  {
  nrow = nrow(df);
  rmax = numeric(0);
  for(i in 1:nrow)
    {
    row = df[i, ];
    rm = max(row, na.rm=TRUE);
    rmax[i] = rm;
    }
  rmax;
  }
  
##################### nlp.pos #####################  

NLP.subsetPOS = function(apos, sub=c(1))
  {
  # just grab indexes found in sub ... 
  # because of custom structure, not so easy
  true.idx = c();
  n.apos = length(apos);
  remaining = length(sub);
  for(i in 1:n.apos)
    {
    row.apos = as.data.frame(apos[i]);
    if(is.element(row.apos$id, sub))
      {
      true.idx = c(true.idx, i);
      remaining = remaining - 1;
      if(remaining == 0) { break; }
      }
    }
  apos[true.idx];
  }


NLP.simplifyPOS = function(my.table, tags.info)
  {
  tags = tags.info$tags;
  tags.keyed = tags.info$tags.keyed;
  keys = sort(names(tags));
  n.keys = length(keys)
  count.keys = list();
  for(i in 1:n.keys)
    {
    key = keys[i];
    count.keys[[key]] = 0;
    }
  nt = nrow(my.table);
  for(j in 1:nt)
    {
    row = my.table[j,];
    tag = row$tags;
    count = row$count;
    key = tags.keyed[[tag]];
    if(!is.null(key))
      {
      count.keys[[key]] = count + count.keys[[key]];
      }
    }
  my.counts = unlist(count.keys);
  my.names = names(my.counts);
  
  n.table = as.data.frame(t(my.counts));
    colnames(n.table) = my.names;
  #n.table = as.data.frame(cbind(my.names, my.counts));
    #colnames(n.table) = c("keys", "count");
    #rownames(n.table) = NULL;
  
  n.table;
  }


NLP.tabularizePOS = function(apos,sub=NULL)
  {
  if(!is.null(sub))
    {
    apos = NLP.subsetPOS(apos,sub);
    }
  tags = sapply(apos$features, `[[`, "POS");
  # print(tags);
  #my.table = as.data.frame( sort(table(tags),decreasing = TRUE) );
    #colnames(my.table) = c("tags","count");
  
  tags.table = sort(table(tags),decreasing = TRUE); 
  
  if(length(tags.table) == 1)
    {
    my.table = as.data.frame( matrix(c(tags[1],as.numeric(tags.table[1])),nrow=1) );
    } else {
            my.table = as.data.frame( tags.table );
            }
    # print(my.table);
    colnames(my.table) = c("tags","count");
    
    my.table$tags = as.character(my.table$tags);
  my.table;
  }

NLP.performPOS = function(str)  # preferably on a single sentence ...
  {
  # ";" counts as colon ...
  # str = "My tale is done, there runs a mouse; whosoever catches it, may make himself a big fur cap out of it.";
  sentence.init = Maxent_Sent_Token_Annotator();
  word.init     = Maxent_Word_Token_Annotator();
  sentence.word.a   = annotate(str, list(sentence.init, word.init));
  
  pos.init = Maxent_POS_Tag_Annotator(); # Maxent_POS_Tag_Annotator(probs=TRUE);
  sentence.word.apos = annotate(str, pos.init, sentence.word.a);
  
  sentences.apos = subset(sentence.word.apos, type=="sentence");
  
  words.apos = subset(sentence.word.apos, type=="word");
  my.table = NLP.tabularizePOS(words.apos);
  # tags = sapply(words.apos$features, `[[`, "POS");
  # my.table = as.data.frame( sort(table(tags),decreasing = TRUE) );
  #   colnames(my.table) = c("tags","count");
  #   my.table$tags = as.character(my.table$tags);
    
  list("table" = my.table, "words" = words.apos, "sentences" = sentences.apos);
  } 

NLP.getRawWords = function(str, lower=TRUE)
  {
  str = NLP.removePunctuationFromString(str);
  if(lower){ str = tolower(str); }
  str = removeWhiteSpace(str);
  strsplit(str," ",fixed=TRUE)[[1]];
  }
   
  
NLP.countPunctuation = function(sentence)
  {
  myCounts = c();

  countMe = c(".",";",":",",","?","!","^[^","^]^");
  labelMe = c("P.per","P.semi","P.colon","P.comma","P.quest","P.exclam","P.left","P.right");

  cn = length(countMe);
  countR = c();
  for(i in 1:cn)
    {
    tmp = strsplit(sentence,countMe[i],fixed=TRUE)[[1]];
    countR[i] = length(tmp);
    }
  names(countR) = labelMe;
  countR = as.data.frame(t(countR));
  
  countR$P.apost = countR$P.right - countR$P.left; # apostrophes ... some poetry elemenents may have LSQ RSQ and technically be an apostrophe, see... ‘O’er hill and o’er dale
  # ‘I’ll tell you what, husband,’ ... grabbing "conversation" may be a bit tricky ...
  
  countR;
  }


NLP.removePunctuationFromString = function(str)
  {
  countMe = c(".",";",":",",","?","!","^[^","^]^");
  cn = length(countMe);
  for(i in 1:cn)
    {
    str = gsub(countMe[i],"",str,fixed=TRUE);
    }
  str = gsub("-"," ",str,fixed=TRUE); # let's break apart hyphenated words
  str = removeWhiteSpace(str);
  str;
  }



NLP.countWordsInString = function(str)  # preferably a sentence, this is large ...
  {
  str = NLP.removePunctuationFromString(str);
  words = strsplit(str," ",fixed=TRUE)[[1]];
  count.words = wc = length(words); # word count
  
  
  count.lower = count.upper = count.ucfirst = 0;

  details = list( "lower" = c(), 
                  "upper" = c(), 
                  "ucfirst" = c());
  
  ####syllables = list();
  
  for(i in 1:wc)
    {
    word = words[i];
    ####syllables[[i]] = countSyllablesInWord(word);
    # https://en.wikipedia.org/wiki/Gunning_fog_index
    
    word.lower = tolower(word);
    word.upper = toupper(word);
    
    if(word == word.lower) 
      { 
      count.lower = 1 + count.lower; 
      details$lower = c(details$lower,word);
      } else if(word == word.upper) 
          { 
          count.upper = 1 + count.upper; 
          details$upper = c(details$upper,word);
          } else { 
                  count.ucfirst = 1 + count.ucfirst; 
                  details$ucfirst = c(details$ucfirst,word);
                  }
    }

  # count.sentences = 1;
  
  # list("words" = words, "details" = details, "syllables" = syllables,
  #     "readability" = computeReadability(count.sentences, syllables),
  #     "count.words" = count.words, "count.lower" = count.lower, 
  #     "count.upper" = count.upper, "count.ucfirst" = count.ucfirst);

  list("count.words" = count.words, "count.lower" = count.lower, 
      "count.upper" = count.upper, "count.ucfirst" = count.ucfirst);
  }




NLP.countCustomWordList = function(words, all.words = buildCustomWordList())
  {
  words.lower = tolower(words);
  # bag of words, order doesn't matter
  # words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
  #   colnames(words.table) = c("word","count");
     
  if(length(words) == 1)
    {
    words.table = as.data.frame( matrix(c(words[1],1),nrow=1) );
    } else {
            words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
            }
    colnames(words.table) = c("word","count");
    
  my.grimm = NULL;
  for(a in 1:length(all.words))
    {
    # could do wildcard ...
    lookup = subsetDataFrame(words.table, "word", "==", all.words[a]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(all.words[a],lookup.count);
    my.grimm = rbind(my.grimm,row);
    }   
  
  cnames = c("word","count");  # "group" would be nice
  
  my.grimm = as.data.frame(my.grimm);
    rownames(my.grimm) = NULL;
    colnames(my.grimm) = cnames;
  
  my.grimm$count = as.numeric(my.grimm$count);
  
  my.grimm;
  }

buildCustomWordList = function(grimms = c("wolf/wolves", "bear/s", "fox/es", "bird/s", "forest/s", "iron/s", "fish/ed/es/fishing/fisherman/fishermen/fishpond/s",  "tree/s", "house/s", "garden/s", "bread/s", "water/s", "hand/s", "pure", "lock/s", "finger/s", "toe/s", "leg/s", "arm/s", "head/s", "foot/feet/footed", "bar/s", "stomach/e/belly", "walk/s/ed", "run/runs/ran", "loaf/loaves", "search/searching/searches/searched", "sea/s", "prophet/s/prophecy/prophecies", "small/large", "emperor/s", "pope/s", "fly/flies/flew", "child/children", "dwarf/dwarves", "witch/es", "moon/s", "raven/s", "cake/s", "sweet/s", "apple/s", "lake/s", "axe/s", "wood/s", "hill/s", "hat/s", "goose/geese", "box/es", "wheel/s", "tower/s", "boil/boiled/boils", "lamb/s", "tail/s", "grove/s", "field/s", "voice/s", "cheese", "coat/s", "rose/s/y", "bushe/s", "feast/s", "wake/s/awaken/rise/n/arose", "enemy/enemies", "better/best", "worse/worst", "pocket/s", "coffin/s", "hungry/hunger/weary/weariness", "cage/s", "chicken/s", "light/s", "dark/darken/darkness", "board/s", "candle/s", "eye/s", "girdle/s", "sleep/asleep/sleeps/rest/rests/rested/restful", "round/s", "circle/s/ed", "square/s/ed", "eat/ate/eaten/eats/", "fire/s", "yard/s", "breast/s", "pigeon/s", "cook/cooked/cooking/cooks", "wild", "beast/s", "devour/s", "god/less", "winter/s", "yes", "no", "soldier/s", "prison/s", "prisoner/s", "grass/es", "rope/roped/ropes", "shoulder/s", "art/s", "cunning/think/thought", "friend/ly/s", "leaf/leaves", "whistle/whistling/whistled/whistles", "year/s", "month/s", "week/s", "dream/s", "gay/happy/sad/unhappy", "sword/s", "air/s", "sing/s/singing/sung", "bone/s", "silk", "well/s",  "day/s",  "evening/s", "morning/s",  "night/s",  "castle/s",  "door/s", "marble/s", "gold/golden/silver/pearls/jewels",  "good/peace", "wicked/evil", "cow/s", "goat/s", "bee/s", "milk/honey", "window/s", "marry/marries/married", "kid/s", "bed/s", "key/s", "giant/s", "today/tomorrow/yesterday", "wish/wished/wishes", "priest/s/priesthood", "sun/s", "frog/s", "fog/s", "summer/s", "spring/s", "fall/s", "autumn/s", "young/younger/youngest", "old/older/oldest/eldest", "leather", "peasant/s", "shepherd", "mayor/s", "hole/s", "barrel/s", "straw", "devil", "servant/s", "sheep", "flock", "cottage/s", "bedchamber/s", "room/s", "kitchen", "courtyard/s", "cabbage/s", "chain/s", "hot/cold/warm", "spirit/s", "red/blue/white/green/s/black/yellow/purple", "blood/bleeding/bleeds", "pig/s", "horse/s", "dog/s", "die/death/dying/died/dies/dead", "kill/killed/kills", "birth/born", "fallen", "handsome/handsomest /beautiful/lovely", "glass", "ball/s", "mouse/mice/mouses/", "cat/s", "mountain/s", "pot/s", "fat/s", "on/off",  "live/lives/life/lived/alive", "early/late", "shoe/s", "stone/s", "pebble/s", "drawer/s", "table/s", "plate/s", "bowl/s", "piece/s", "body/bodies", "salt/pepper", "left/right", "top/bottom", "cut/s", "wine/s", "home/s", "knife/knives", "fork/forks", "spoon/s/spoonful", "cloth/es/clothing", "treasure/s", "guest/s",  "war/wars/battle/battles/battlefield", "animal/s", "pillow/s", "food/feed/breakfast/s/lunch/es/dinner/s/supper/s", "beam/s", "robber/s", "monster/s", "tooth/teeth", "music/musician", "donkey/s", "bit/bite/bites/bitten", "club/s", "stick/s", "storm/s/y", "shelter/s/ed", "drink/drunk/drinks/drunken", "meadow/s", "branch/es", "ring/s",  "cry/cries/cried/weep/wept/", "snow/s/snowflake/s", "rain/s", "heart/s", "love/lover/loved/loves", "country/countryside", "land/s", "little/big", "duck/s", "or/and/but", "also/too", "thirsty/drink", "wren/s", "nest/s", "egg/s",  "one/two/three/four/five/six/seven/eight/nine/ten/eleven/twelve/thirteen", "ones/twos/threes/fours/fives/sixes/sevens/eights/nines/tens/elevens/twelves/thirteens", "church/es", "lip/s", "half/whole", "first/second/third/fourth/fifth/sixth/seventh/eighth/ninth/tenth/eleventh/twelfth/thirteenth",  "hundred/s", "thousand/s", "heaven/s", "heavy", "wind/y", "poor/rich", "mile/s" ))
  {
  all.words = c();
  for(g in 1:length(grimms))
    {
    sub.g = strsplit(grimms[g],"/",fixed=TRUE)[[1]];
    n.g = length(sub.g);
    sub.previous = NULL;
    for(i in 1:n.g)
      {
      s = sub.g[i];
      if(i > 1)
        {
        s.n = nchar(s);
        if(s.n < 3) # we have a token-stem
          {
          all.words = c(all.words, paste0(sub.previous,s)); # append
          } else {
                  all.words = c(all.words, s); # next one
                  sub.previous = s;
                  }
        } else {
                all.words = c(all.words, s); # first one
                sub.previous = s;
                }
      }
    #
    }
  all.words;
  }


NLP.setupTags = function()
  {
  # https://www.sketchengine.eu/penn-treebank-tagset/ 
# https://stackoverflow.com/questions/1833252/java-stanford-nlp-part-of-speech-labels#1833718  
  
  # all.tags = NLP::Penn_Treebank_POS_tags;
  # all.tags[,1:2];
  stop.tags = c(":", "."); # hard stops ... 
  # https://catalog.ldc.upenn.edu/docs/LDC95T7/cl93.html
  # ? NLP::Penn_Treebank_POS_tags
  #   
  # do I have all.tags?
  skip.tags = c(",", "``", "''","$","(",")","-","LS");
  # "," comma (e.g., Oxford comma) maybe is a pause ...
  # update classifier to separate intent? Or maybe it already is...
  # "CC" as FANBOYS ... https://www.thoughtco.com/coordinating-conjunction-grammar-1689929
  tags = list(
          "noun" = c("NN", "NNS"),
          "pron" = c("PP", "WP"),
          "proper" = c("NNP", "NNPS"),
          "verb" = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ", # similar to :: https://arxiv.org/pdf/1104.2086.pdf
                      "VH", "VHD", "VHG", "VHN", "VHP", "VHZ", 
                      "VV", "VVD", "VVG", "VVN", "VVP", "VVZ"), # some of these are not in this implementation
          "aux" = c("TO", "MD"),
                      # "to" as preposition or infinitive marker
                      # modal auxiliary:: can, could, may, might, must, ought, shall, should, will, and would.
          "adj" = c("JJ","JJR","JJS"),
          "adv" = c("RB", "RBR", "RBS", "WRB"),
          "number" = c("CD"),
          "conj" = c("CC"), # conjunction, coordinating
          "foreign" = c("FW"),
          "det" = c("DT", "PDT", "WDT"),  # WH-determiner
          "prep" = c("IN"), # preposition or conjunction, subordinating ... why not separate?
          "intj" = c("UH"),
          "there" = c("EX"),
          "poss" = c("POS", "PPZ", "WP$", "PRP$")
              );
  tags.keyed = NLP.keyMyTags(tags);  # reverse keyed, faster lookup
  
  
  tags.info = list("stop" = stop.tags, "skip" = skip.tags,
                    "tags" = tags, "tags.keyed" = tags.keyed);
  
  tags.info;
  }

NLP.keyMyTags = function(tags)
  {
  nam = names(tags);
  res = list();
  for(na in nam)
    {
    ta = tags[[na]];
    for(tag in ta)
      {
      res[[tag]] = na;
      }
    }
  res;
  }
  
   

NLP.countGenderLanguage = function(words) # words is vector, in order
  {
  words.lower = tolower(words);
  # bag of words, order doesn't matter
  # words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
  #   colnames(words.table) = c("word","count");
  if(length(words) == 1)
    {
    words.table = as.data.frame( matrix(c(words[1],1),nrow=1) );
    } else {
            words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
            }
    colnames(words.table) = c("word","count");
    
  my.gender = NULL;

  g.person = "male";
  g.number = "singular";
  genders = c("he", "him", "his", "himself", "man", "boy", "king", "prince", "son", "father", "dad", "daddy", "fatherhood", "brother", "godfather", "gentleman", "huntsman", "fisherman", "groom", "husband");
  
  for(g in 1:length(genders))
    {
    lookup = subsetDataFrame(words.table, "word", "==", genders[g]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(g.person,g.number,genders[g],lookup.count);
    my.gender = rbind(my.gender,row);
    }
  
  g.person = "male";
  g.number = "plural";  # plural may be capturing possessive "father's"?
  genders = c("men", "boys", "kings", "princes", "sons", "fathers", "dads", "daddies", "brothers", "brethren", "brotherhood", "godfathers", "gentlemen", "huntsmen", "fishermen", "husbands");
  
  for(g in 1:length(genders))
    {
    lookup = subsetDataFrame(words.table, "word", "==", genders[g]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(g.person,g.number,genders[g],lookup.count);
    my.gender = rbind(my.gender,row);
    }
  
  g.person = "female";
  g.number = "singular";
  # witch ... 
  genders = c("she", "her", "herself", "woman", "girl", "queen", "princess", "daughter", "mother", "mom", "mommy", "motherhood", "sister", "godmother", "lady", "maid", "maiden", "bride");
  
  for(g in 1:length(genders))
    {
    lookup = subsetDataFrame(words.table, "word", "==", genders[g]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(g.person,g.number,genders[g],lookup.count);
    my.gender = rbind(my.gender,row);
    }
  
  g.person = "female";
  g.number = "plural";
  genders = c("women", "girls", "queens", "princesses", "daughters", "mothers", "moms", "mommies", "sisters", "sisterhood", "godmothers", "ladies", "maids", "maidens");
  
  for(g in 1:length(genders))
    {
    lookup = subsetDataFrame(words.table, "word", "==", genders[g]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(g.person,g.number,genders[g],lookup.count);
    my.gender = rbind(my.gender,row);
    }
  
  
  
  
  
    
  cnames = c("gender","number","word","count");
  
  my.gender = as.data.frame(my.gender);
    rownames(my.gender) = NULL;
    colnames(my.gender) = cnames;
  
  my.gender$count = as.numeric(my.gender$count);
  
  my.gender;
  }

NLP.countPersonalPronouns = function(words) # words is vector, in order
  {
  words.lower = tolower(words);
  # bag of words, order doesn't matter
  if(length(words) == 1)
    {
    words.table = as.data.frame( matrix(c(words[1],1),nrow=1) );
    } else {
            words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
            }
    colnames(words.table) = c("word","count");
    
  my.pronouns = NULL;
  
  pr.person = "first";
  pr.number = "singular";
  pronouns = c("i", "me", "my", "mine", "myself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "first";
  pr.number = "plural";
  pronouns = c("we", "us", "our", "ours", "ourselves");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  
  pr.person = "second";
  pr.number = "singular";
  pronouns = c("you", "you", "your", "yours", "yourself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "second";
  pr.number = "plural";
  pronouns = c("you", "you", "your", "yours", "yourselves");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "third";
  pr.number = "singular";
  pronouns = c("he", "him", "his", "his", "himself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "third";
  pr.number = "singular";
  pronouns = c("she", "her", "her", "hers", "herself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  pr.person = "third";
  pr.number = "singular";
  pronouns = c("it", "it", "its", "-NA-", "itself");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  pr.person = "third"; 
  pr.number = "plural";
  pronouns = c("they", "them", "their", "theirs", "themself", "themselves");
  for(p in 1:length(pronouns))
    {
    lookup = subsetDataFrame(words.table, "word", "==", pronouns[p]);
    lookup.count = 0;
    if(dim(lookup)[1] == 1) { lookup.count = lookup$count; }
    row = c(pr.person,pr.number,pronouns[p],lookup.count);
    my.pronouns = rbind(my.pronouns,row);
    }
  
  
    pn = dim(my.pronouns)[1];
  rnames = c("subject","object","determiner","independent","reflexive");
    rn = length(rnames);
    gn = (floor(pn/rn));
  rnames.all = c();  
  for(g in 1:gn)
    {
    rnames.all = c(rnames.all, paste0(rnames,"-",g));
    }
    rnames.all = c(rnames.all, paste0("reflexive.b-",g));
    
    length(rnames.all);
  cnames = c("person","number","word","count");
  
  my.pronouns = as.data.frame(my.pronouns);
    # rownames(my.pronouns) = rnames.all;
    rownames(my.pronouns) = NULL;
    colnames(my.pronouns) = cnames; 
  
  my.pronouns$count = as.numeric(my.pronouns$count);
  
  my.pronouns;
  }
  

NLP.countVowelsInString = function(strs)
  {
  res = c();
  for(str in strs)
    {
    vowels = c("a","e","i","o","u","y");
    
    str.vec = strsplit(str,"")[[1]];
    str.vec;
    
    is.vowel = is.element(tolower(str.vec), vowels);
    n.vowels = sum(is.vowel);
    n.vowels;
    res = c(res, n.vowels);
    }
  res;
  }

NLP.computeReadability = function(n.sentences, syllables=NULL)
  {
  n.words = length(syllables);
  n.syllables = 0;
  isDataFrame = is.data.frame(syllables);
  if(isDataFrame) { n.words = nrow(syllables); }
  # dataframe is a list of a certain class
  for(i in 1:n.words)
    {
    if(isDataFrame) 
      { 
      my.syllable = syllables[i,];
      } else {
              my.syllable = syllables[[i]];
              }
    if(is.na(my.syllable$syllables)) { my.syllable$syllables = 0;}
    n.syllables = my.syllable$syllables + n.syllables;
    }
  
  if(n.sentences == 0) { stop("zero sentences not allowed!"); }
  if(n.words == 0) { stop("zero words not allowed!"); }
  
  fk = NLP.computeFleshKincaid(n.sentences, n.words, n.syllables);
  
  list("FRE" = fk$FRE, "FKGL" = fk$FKGL, "syllables" = n.syllables, "words" = n.words); 
  }

NLP.computeFleshKincaid = function(n.sentences, n.words, n.syllables)
  {
  # Flesch Reading Ease (FRE):
  FRE = 206.835 - 1.015 * (n.words/n.sentences) - 84.6 * (n.syllables/n.words);
  # Flesh-Kincaid Grade Level (FKGL):
  FKGL = 0.39 * (n.words/n.sentences) + 11.8 * (n.syllables/n.words) - 15.59; 
  # FKGL = -0.384236 * FRE - 20.7164 * (n.syllables/n.words) + 63.88355;
  # FKGL = -0.13948  * FRE + 0.24843 * (n.words/n.sentences) + 13.25934;
  
  list("FRE" = FRE, "FKGL" = FKGL);
  }

 
NLP.countSyllablesInWord = function(words)
  {
  # quanteda::nsyllable ... uses CMU, no syllabification 
  
  my.syllables = quanteda::nsyllable(words);
  my.vowels = countVowelsInString(words);
  
  res = as.data.frame(cbind(words,my.syllables, my.vowels));    
    colnames(res) = c("words","syllables","vowels");
    rownames(res) = NULL;
  res$syllables = as.numeric(res$syllables);
  res$vowels = as.numeric(res$vowels);
  
  res;
  }

# https://en.wikipedia.org/wiki/Gunning_fog_index
# THIS is a CLASSIFIER PROBLEM ...
# https://stackoverflow.com/questions/405161/detecting-syllables-in-a-word
countSyllablesInWordMonte = function(words)
  {
  #word = "super";
  n.words = length(words); 
  result = list();
  for(j in 1:n.words)
    {
    word = words[j];
    
    word = gsub("[^[:alnum:] ]", "", word);
    
    vowels = c("a","e","i","o","u","y");
    
    word.vec = strsplit(word,"")[[1]];
    word.vec;
    
    n.char = length(word.vec);
    
    is.vowel = is.element(tolower(word.vec), vowels);
    n.vowels = sum(is.vowel);
    
    
    # nontrivial problem 
    if(n.vowels <= 1)
      {
      syllables = 1;
      str = word;
      } else {
              # syllables = 0;
              previous = "C";
              # on average ? 
              str = "";
              n.hyphen = 0;
        
              for(i in 1:n.char)
                {
                my.char = word.vec[i];
                my.vowel = is.vowel[i];
                if(my.vowel)
                  {
                  if(previous == "C")
                    {
                    if(i == 1)
                      {
                      str = paste0(my.char, "-");
                      n.hyphen = 1 + n.hyphen;
                      } else {
                              if(i < n.char)
                                {
                                if(n.vowels > (n.hyphen + 1))
                                  {
                                  str = paste0(str, my.char, "-");
                                  n.hyphen = 1 + n.hyphen;
                                  } else {
                                           str = paste0(str, my.char);
                                          }
                                } else {
                                        str = paste0(str, my.char);
                                        }
                              }
                     # syllables = 1 + syllables;
                     previous = "V";
                    } else {  # "VV"
                          # assume what  ?  vowel team?
                          str = paste0(str, my.char);
                          }
            
                } else {
                            str = paste0(str, my.char);
                            previous = "C";
                            }
                #
                }
        
              syllables = 1 + n.hyphen;
              }
  
      result[[j]] = list("syllables" = syllables, "vowels" = n.vowels, "word" = str);
      }
  
  # if(n.words == 1) { result[[1]]; } else { result; }
  result;
  }

  
NLP.prepStopWords = function(localfile)
  {
  str = trimMe(readStringFromFile(localfile));
  rows = explodeMe("\n", str);
  for(i in 1:length(rows))
    {
    rows[i] = trimMe(rows[i]);
    }
  rows;
  }  
  
##################### nlp #####################

 
  
NLP.buildNgrams = function(str, n=5, 
                    do.pos = TRUE,
                    verbose = FALSE,
                    do.stemming = TRUE, # tm::stemDocument ... not hard to ADD
                    do.variants = FALSE, # TODO  
                                      # final.rows = cbind(i, final.words, final.tags);
                                      # above would allow for modifier inflation ...
                                      # would have to look forward/backward to do
                    inflate = FALSE, # n-grams and modifiers
                                    # necessarily a 5 gram will have its children
                    my.stopwords = NULL # my.stopwords = stop.snowball
                                      ) # preferably one sentence at a time, already with a stop break
  {
timer.start = as.numeric(Sys.time());
########## start of function #######  
  
  # first 3 are required ...
  gram.types = c("words", ".tags", "|simple",
                        "words.tags", "words|simple",
                "word+",
                        "word+.tags", "word+|simple"
                                    );
  # if POS exists, we can perform the variants by switching modifiers ...
  grams = NLP.initGrams(n,gram.types);
  ####  TODO ::: after walk 
  # if we run across punctuation, we treat as a stop ...
  # what to do about contractions can't and quotations ?
### str = my.story;
  # str = "Can't you get anything right?";
  str = NLP.prepStringGram(str, TRUE);
  # if(do.pos)
    {
    my.pos = NLP.performPOS(str);
    }
  
  # test = "Monte's friend and son Alexander can't read well but he is not yet five years old.";
  # t.pos = performPOS(test);
  # substr(test, 6,7); ## drop POS from choices as a word
  # https://stackoverflow.com/questions/28720174/negation-handling-in-nlp
  # substr(test, 34,35);    ## ca   [MD] 
  # substr(test, 36,38);    ## n't  [RB]
  ##  CC is "and" and "but"
  ##  RB is "not" and "n't" and "yet"
  # substr(test, 60,62);    ## not  [RB]
  # substr(test, 68,71);    ## five  [CD]
  
  
  my.sentences   = my.pos$sentences;
  my.words       = my.pos$words;
  n.sentences    = length(my.sentences);

  tags.info = NLP.setupTags();
                    
  # I could replace a stop-word with its generic POS
  # list$pos

  
  case = NULL;
  punctuation = NULL;
  PP = NULL;
  GENDER = NULL;
  CUSTOM = NULL;
  
  readability = NULL;
  sentiment = NULL;
  mytags = list();  # raw, we don't know if we have all the keys, and if they are unique
  mytags.s = list(); # simple "keys"  adj
  final = NULL;
  
########## start of sentences #######   
  for(i in 1:n.sentences)
    {
    print(paste0("Sentence [",i,"] of ",n.sentences));
    flush.console();
    
    my.sentence = as.data.frame(my.sentences[i]);
    
      my.id = my.sentence$id;
      idx.start =  my.sentence$start;
      idx.end   =  my.sentence$end;
    s.sentence = substr(str, idx.start, idx.end);
 
  print(paste0("-----------------> ",s.sentence)); 
  flush.console();
    
        my.words.idx = my.sentence$features[[1]]$constituents;
    mytags[[i]] = NLP.tabularizePOS(my.words,my.words.idx); # still in unique data format
    # mytags.s[[i]] = simplifyPOS(mytags[[i]],tags,tags.keyed);
    mytags.s[[i]] = NLP.simplifyPOS(mytags[[i]],tags.info);
    
    # words from POS may be different than actual words; e.g., "can't"
    words.r = NLP.getRawWords(s.sentence); 
    
    
      sinfo = NLP.doSentimentAnalysis(s.sentence); # expensive
    sentiment = rbind(sentiment, c(i, sinfo) );
        rinfo.s = NLP.countSyllablesInWord(words.r);
        # print(rinfo.s);
        rinfo = NLP.computeReadability(1,rinfo.s);
        # countSyllablesInWord words
    readability = rbind(readability, c(i, unlist(rinfo)) );
        info.c = NLP.countWordsInString(s.sentence);
    case = rbind(case, c(i, unlist(info.c)) ); 
        info.p = NLP.countPunctuation(s.sentence);
    punctuation = rbind(punctuation, c(i, unlist(info.p)) );
        info.pp = NLP.countPersonalPronouns(words.r);
    PP = rbind(PP, cbind(i, rownames(info.pp), info.pp) );
        info.ge = cbind(i, NLP.countGenderLanguage(words.r));
    GENDER = rbind(GENDER, info.ge );
        info.gr = cbind(i, NLP.countCustomWordList(words.r));
    CUSTOM = rbind(CUSTOM, info.gr );

    
    r = 1;
    # we will use these to compare ...  
    # no punctuation, lower case ... no knowledge of POS
    
    nw = length(my.words.idx);
    w = 1;
    #for(w in 1:nw)
    # "Can't you get anything right?" ... 67% positive
    my.stack = NLP.initStack(gram.types);
    final.words = c(); # full list, unstacked ... these would allow to later matching
    final.tags = c();  # full list, unstacked
            # grams = initGrams(gram.types);       
    pos.stopped = TRUE;
    word.stopped = FALSE;
    has.skipped = FALSE;
    left.quote = FALSE;
    right.quote = FALSE;
##################################################    
    while(w <= nw)
      {
      
      
########## start of while #######      
      
      my.word.idx = my.words.idx[w];
     
      my.word = as.data.frame( subset(my.words, id==my.word.idx) );
        my.feature = my.word$features[[1]]$POS;
          f.word = paste0(".",my.feature);
          p.word = paste0("|",tags.info$tags.keyed[[my.feature]]);
          s.word = substr(str, my.word$start, my.word$end);
          # "asleep." in sentence 58
          # POS "period" should be taken care of here ... bug
          s.word = gsub(".", "", s.word, fixed=TRUE);
          
          r.word = words.r[r];
          r.len = nchar(r.word);
              if(is.na(my.word.idx)) { break; }
              if(is.na(r.word)) { break; }

      skip.me = (s.word == "'" && ( has.skipped || pos.stopped || word.stopped) );
      if(skip.me)
        {
        if(charAt(r.word,1) == "'")
          { 
          r.word = substr(r.word,2,r.len);
          left.quote = TRUE;  # "'what" in sentence 4 of HANSEL
          }
        }
      if(left.quote)
        {
        if(charAt(r.word,1) == "'")
          {
          r.word = substr(r.word,2,r.len);
          }
        }
      if(charAt(r.word,r.len) == "'") # "ourselves'" in sentence 5 of HANSEL
          { 
          r.word = substr(r.word,1,(r.len-1));
          right.quote = TRUE;  
          }
                                      
      
      
      
if(verbose) 
{
print(paste0("[",w,"] ... word: [",s.word, "] r: [",r.word,"] --> f: [",f.word,"] p: [",p.word,"] ==> ", tm::stemDocument(r.word) ));
flush.console();
}    
      if(is.element(my.feature, tags.info$skip ) || skip.me)  # skip this ...
        {
        w = 1 + w;
        has.skipped = TRUE;
if(verbose) 
{
        print("SKIPPING");
        print(s.word);
}  
        } else {
                has.skipped = FALSE;
      
      
      
      
              if(is.element(my.feature, tags.info$stop ))
                {
                # hard stop ... 
                ginfo = NLP.updateGrams(grams, my.stack, "ALL", tags.info, do.stemming);
                  grams = ginfo$grams;
                my.stack = NLP.initStack(gram.types);
                pos.stopped = TRUE;
                } else {
                        pos.stopped = FALSE;
                        r = 1 + r;
                        if(!is.element(my.feature, tags.info$tags$proper)) { s.word = tolower(s.word); } # NNP are proper nouns # lower case ...
                        n.word = s.word;
                        if(tolower(s.word) != tolower(r.word))
                          {
                          # "can't" is now "cant" [words.r] ... and linked to POS "ca" and "n't"
                          # we skip one of the idx and update "cant" back to "can't"
                          ## # stop("monte"); 
                          #w = 1 + w;
                            next.word.idx = my.words.idx[w+1];
                            next.word = as.data.frame( subset(my.words, id==next.word.idx) );
                            if(nrow(next.word) == 0) 
                              { 
                              #if(verbose)
                                {
                                print(paste0("we are not finding a next word, using: ", n.word));
                                }
                              } else {
                            
                                          if(verbose)
                                          {
                                          #print("monte");
                                          print(next.word); 
                                          }
                                      next.s.word = substr(str, next.word$start, next.word$end);
                                          if(verbose)
                                          {
                                          print(paste0("====> ", s.word, " ... ", next.s.word, " *** ",  paste(next.word$features, collapse=" :: ")));
                                          #print(element.exists(next.word$features[[1]]$POS));
                                          }
                                      next.feature = next.word$features[[1]]$POS;
                                        next.f.word = paste0(".",next.feature);
                                        next.p.word = paste0("|",tags.info$tags.keyed[[next.feature]]);
                                        
                                    # "I'll" ...
                                    n.word = paste0(s.word,next.s.word);  
                                    
                                    f.word = paste0(f.word,"-",next.f.word);
                                    p.word = paste0(p.word,"-",next.p.word);
                            ###stop("monte");  
                                    # append -feautre to previous
                                    w = 1 + w; # let's skip ... 
                                    }
                          }
                        
                        
                        # let's try and keep "auto-pop" from happening
                        # I could just let "stop" words do it's thing 
                        # and at the end do the combos ... so let's say it is 13 long
                        # I could just build all 5-grams and pairs at ends 
                        # do it this way for now ...
                        ginfo = NLP.updateGramsIfMax(n, grams, my.stack, "ALL", tags.info, do.stemming);
                            grams    = ginfo$grams;
                            my.stack = ginfo$my.stack;
                        
                        # what if it is a stop word? ... 
                        # that will only update the regular stack
                        # not the POS stacks
                        final.words = c(final.words, n.word);
                        
                        if(is.element(n.word, my.stopwords))
                            {
                            for(gram.type in c("words","words.tags","words|simple",
                                               "word+","word+.tags","word+|simple"))
                              {
                              ginfo = NLP.updateGrams(grams, my.stack, gram.type, tags.info, do.stemming);
                                grams    = ginfo$grams;
                                my.stack = ginfo$my.stack;
                              my.stack = NLP.resetStackElement(my.stack, gram.type);
                              }
                            word.stopped = TRUE;
                            
                            } else {
                                    word.stopped = FALSE;
                                    my.stack[["words"]] = NLP.pushVector(n.word, my.stack[["words"]]$vec);
                                    my.stack[["words.tags"]] = NLP.pushVector(paste0(n.word,f.word), my.stack[["words.tags"]]$vec);
                                    my.stack[["words|simple"]] = NLP.pushVector(paste0(n.word,p.word), my.stack[["words|simple"]]$vec);
                                    
                                    t.word = tm::stemDocument(n.word);
                                    # print(paste0("stemmed: ", t.word));
                                    my.stack[["word+"]] = NLP.pushVector(t.word, my.stack[["word+"]]$vec);
                                    my.stack[["word+.tags"]] = NLP.pushVector(paste0(t.word,f.word), my.stack[["word+.tags"]]$vec);
                                    my.stack[["word+|simple"]] = NLP.pushVector(paste0(t.word,p.word), my.stack[["word+|simple"]]$vec);
                                    }
                        # gram.types[gram.types != "words"]
                        final.tags = c(final.tags, f.word);
                        my.stack[[".tags"]] = NLP.pushVector(f.word, my.stack[[".tags"]]$vec);
                        my.stack[["|simple"]] = NLP.pushVector(p.word, my.stack[["|simple"]]$vec);
                                    
                        
                        
                        
                        
                        # ... soft stop will update my.stack but not my tags
                        # use push/pop when > 5 ?
                        # https://stackoverflow.com/questions/28687806/a-better-way-to-push-and-pop-to-from-lists-in-r
                        # push/pop list vs vector ... just NULL and append to vecotr?
                        
                        ### stack full = update grams, otherwise just update stack ...
                        # is.stop = 
                  
                  
                  
                        }
                w = 1 + w;
                }

## print(paste0("[",w,"] ... word: [",s.word, "] r: [",r.word,"] --> f: [",f.word,"] p: [",p.word,"]"));
########## end of while ####### 
        
        }
      # hard stop ... end of sentence, just to be certain ... 
      # If a POS . this was already taken care of
      ginfo = NLP.updateGrams(grams, my.stack, "ALL", tags.info, do.stemming);
        grams = ginfo$grams;
      my.stack = NLP.initStack(gram.types);
      
#       story = 30
#       "Sentence [3] of 58"
# [1] "-----------------> 'Kate!"
      # print(final.tags);
      # print(final.words);
      
      if(length(final.words) > 0)
      {
      if(length(final.words) == length(final.tags))
      {
      final.rows = cbind(i, final.words, final.tags);
      final = rbind(final, final.rows);
      }
      }
      # print(final.rows);
      
      
      #stop("final");
    
      ### end of sentences ...
      }
  
  final = as.data.frame(final);
    rownames(final) = NULL;
    colnames(final)[1] = c("sentence");
  
  sentiment = as.data.frame(sentiment);
    rownames(sentiment) = NULL;
    colnames(sentiment) = c("sentence", "positive", "negative");
  readability = as.data.frame(readability);
    rownames(readability) = NULL;
    colnames(readability)[1] = c("sentence");
  
  case = as.data.frame(case);
    rownames(case) = NULL;
    colnames(case)[1] = c("sentence");
  
  punctuation = as.data.frame(punctuation);
    rownames(punctuation) = NULL;
    colnames(punctuation)[1] = c("sentence");
  
  PP = as.data.frame(PP);
    rownames(PP) = NULL;
    colnames(PP)[1] = c("sentence");
    colnames(PP)[2] = c("row.type");
  
  
  GENDER = as.data.frame(GENDER);
    rownames(GENDER) = NULL;
    colnames(GENDER)[1] = c("sentence");
  
  CUSTOM = as.data.frame(CUSTOM);
    rownames(CUSTOM) = NULL;
    colnames(CUSTOM)[1] = c("sentence");
  
  
  # case = NULL;
  # punctuation = NULL;
  # PP = NULL;
  # GENDER = NULL;
  # CUSTOM = NULL;
  
   
timer.end = as.numeric(Sys.time());
elapsed = round( (timer.end - timer.start), 2);
  #print(paste0("[-- EVERYTHING --] in ",elapsed," secs"));
  # pause = c(".",";",":",",","?","!","^[^","^]^");  # Let's only do this if POS exists
  # can I simplify POS, more basic?
  
  list("grams" = grams, "final" = final, "time" = elapsed,
        "sentiment" = sentiment, "readability" = readability,
        "mytags" = mytags, "mytags.s" = mytags.s,
        "case" = case, "punctuation" = punctuation,  "PP" = PP,
        "GENDER" = GENDER, "CUSTOM" = CUSTOM
        );
  }













truncateWordVector = function(words, cut = 3) # words is vector, in order
  {
  words.lower = tolower(words);
  # bag of words, order doesn't matter
  words.table = as.data.frame( sort(table(words.lower),decreasing = TRUE));
    colnames(words.table) = c("word","count");
  
  comparison = words.table$count > cut;  
  new.table = words.table[comparison,]; 
  new.table$word;  # just return the vector of words that meet criteria ...
  }




NLP.doSentimentAnalysis = function(str, qdap=FALSE)
  {
  # https://opendatascience.com/sentiment-analysis-in-r-made-simple/
  sentiment = SentimentAnalysis::analyzeSentiment(str);
  
  
  # convertToBinaryResponse(sentiment)$SentimentGI 
  # convertToBinaryResponse(sentiment)$SentimentHE
  # convertToBinaryResponse(sentiment)$SentimentLM
  # convertToBinaryResponse(sentiment)$SentimentQDAP
  
  # seems to be a subtraction of negative and positive, 
  # so let's return both
  
  pos = 0;
  neg = 0;
  
  keys = c("GI","HE","LM"); # ,"QDAP");  # "Can't you get anything right?"
  if(qdap) { keys = c("GI","HE","LM","QDAP"); }
  
  for(key in keys)
    {
    pos = pos + sentiment[[paste0("Positivity",key)]];
    neg = neg + sentiment[[paste0("Negativity",key)]]; 
    }
  
  # let's norm to 1, so it is 50-50 for perfect balance
  pos.norm = pos / (pos + neg);
  neg.norm = neg / (pos + neg);
  
  # doesn't capture "dark"?
  c(pos.norm, neg.norm);
  }



############# NLP.STACK


rowMaxColumn = function(df)
  {
  nrow = nrow(df);
  rmax = numeric(0);
  for(i in 1:nrow)
    {
    row = df[i, ];
    rmax[i] = which.max(row); # downward biased
    }
  rmax;
  }

rowMax = function(df)
  {
  nrow = nrow(df);
  rmax = numeric(0);
  for(i in 1:nrow)
    {
    row = df[i, ];
    rm = max(row, na.rm=TRUE);
    rmax[i] = rm;
    }
  rmax;
  }





# # https://stackoverflow.com/questions/28687806/a-better-way-to-push-and-pop-to-from-lists-in-r
                
pushList = function(nlist, veclist, n=length(vlist))
  {
  
  }
popList = function(nlist, veclist, n=length(vlist))
  {
  
  }

# https://stackoverflow.com/questions/2805102/how-is-pushing-and-popping-defined
# vec = 1: 10;
# popVector(vec)
# popVector(vec, method="LIFO-LILO")
NLP.popVector = function(vec, idx=1, method="FIFO")  # Inf would work for unlimited stack
  {
  if(method=="FIFO")   # QUEUING
    {
    val = vec[idx];  
    vec = vec[-c(idx)];
    } else {
            n = length(vec) + 1 - idx;
            val = vec[n];
            vec = vec[-c(n)];
            }
  list("val" = val, "vec" = vec, "popped" = NULL); # updated ...
  }

# vec = 1: 10;
# popVector(pushVector(13, vec)$vec)
# pushVector(13, vec, n.max=5)
# pushVector(13, vec, n.max=5, method="LIFO-LILO")
NLP.pushVector = function(val, vec, n.max=1+length(vec), method="FIFO")
  {
  # n.max is max size, so vals popped may return ...
  n = length(vec);
  popped = NULL;
  if(method=="FIFO")  # in this model, new values are added to end
    {
    if(n < n.max)
      {
      vec = c(vec,val);
      } else {
              vec = c(vec,val);
              nn = 1 + n;
              nd = nn - n.max;
              if(nd > 0)
                {
                popped = vec[1:nd];
                vec = vec[(1+nd):nn];
                }
              }
    } else {        # in this model, new values are added to beginning
            if(n < n.max)
              {
              vec = c(val,vec);
              } else {
                      vec = c(val,vec);
                      nn = 1 + n;
                      if(nn > (1+n.max))
                        {
                        popped = vec[(1+n.max):nn];  # off the end ?
                        vec = vec[1:n.max];
                        }
                      }
            }
  list("vec" = vec, "popped" = popped, "val"=val); 
  }

 
NLP.initGrams = function(n,gram.types)
  {
  grams = list();
  
  for(gram.type in gram.types)
    { 
    grams[[gram.type]] = list();
    for(i in 1:n) 
      {
      grams[[gram.type]][[i]] = character();
      }
    }
    # grams[[i]] = list("disjoint" = list(), 
    #                   "inflated" = list(),
    #                   "variants" = list(),  
    #                   "pos" = list()
    #   
    #               );  # add stemmed? variants? inflation?
    #                   # replace with GENERIC if stop word
    
  grams;
  }
    

    # my.stack = c();   # words  ... we will STEM at last minute ...
    # my.stack.t = c(); # tags  ... NN
    # my.stack.ts = c(); # tags.simple  ... noun
NLP.initStack = function(gram.types)
  { 
  my.stack = list();
  for(gram.type in gram.types)
    {
    my.stack[[gram.type]] = list("vec" = NULL, "popped" = NULL, "val" = NULL);
    }
  my.stack;
  }

NLP.resetStackElement = function(my.stack, gram.type)
  {
  my.stack[[gram.type]] = list("vec" = NULL, "popped" = NULL, "val" = NULL);
  my.stack;
  }
 
# gvec = c("monte","says","hi","to","his","son","alex");
# gramCombinations(gvec)
NLP.gramCombinations = function(gvec, nv = length(gvec), sep="*")
  { 
 # print(gvec);
  # return all permutations ... maintain order ... create as a list of each length
  result = list();
  result[[1]] = gvec;
  if(nv > 2)
    {
    # for(c in 2:(nv-1))
    for(c in 2:nv)
      {
      # nvec = combn(1:nv, c);  # numeric ... we will subset if they are not adjacent
      cvec = combn(gvec, c);  # loop over columns, store previous, if new, we add ...
      #cvec;
      
      res = paste0(cvec[,1],collapse=sep);
      previous = cvec[1,1];
      nc = ncol(cvec);
      if(nc > 1)
        {
        for(col in 2: nc)
          {
          current = cvec[1,col];
          if(current != previous)
            {
            res = c(res, paste0(cvec[,col],collapse=sep) );
            previous = current;
            }
          }
        }
      #res;
      
      
      
      result[[c]] = res;
      }
    }
    # result[[nv]] = paste0(gvec,collapse=sep);
    # print(result[[nv]]);
  result;
  }

# if updateGrams is called, that means the current stack is good-to-go
# therefore, we will popVector
NLP.updateGrams = function(grams, my.stack, which="ALL", tags.info, do.stemming=TRUE)  
  {
  gram.types = names(my.stack);
  if(which != "ALL") { gram.types = gram.types[ which(gram.types == which) ];}
  for(gram.type in gram.types)  # gram.type = gram.types[1]
    {
    # do something here ... 
    # print(gram.type);
    my.vec = my.stack[[gram.type]]$vec;
    #nv = length(my.vec);  # if 5, we want to return, 5,4,3,2,1 ... disjoint combos
                          # or we could just return 5 ... EASIEST, largest gram found
    
    if(!is.null(my.vec)) # back-to-back stop words?
      {
      my.str = NLP.gramCombinations(my.vec); 
      ns = length(my.str);
      for(i in 1:ns)
        {
        nsd = length(my.str[[i]]);
        for(j in 1:nsd)
          {
          # print(my.str[[i]][j]);
          grams[[gram.type]][[i]] = c(grams[[gram.type]][[i]], my.str[[i]][j]);
          }
        }
      
      # popVector
      my.stack[[gram.type]] = NLP.popVector(my.stack[[gram.type]]$vec);
      }
    }
  list("grams" = grams, "my.stack" = my.stack);
  }



NLP.updateGramsIfMax = function(n, grams, my.stack, which="ALL", tags.info, do.stemming)
  {
  # n is maximum number of grams ...
  gram.types = names(my.stack);
  if(which != "ALL") { gram.types = gram.types[[which]];}
  for(gram.type in gram.types)
    {  
    # do  something here ... 
    my.vec = my.stack[[gram.type]]$vec;
    if(length(my.vec) == n) # can't be greater than?
      {
      ginfo = NLP.updateGrams(grams, my.stack, which=gram.type, tags.info, do.stemming);  
        grams    = ginfo$grams;
        my.stack = ginfo$my.stack;
      }
    
    }
  
  list("grams" = grams, "my.stack" = my.stack);
  }
                    

# if(length(my.stack$vec) == n)
#                   {
#                   ginfo = updateGrams(grams, my.stack, "default", tags.info, do.stemming); #
#                   grams = ginfo$grams;
#                   pinfo = popVector(my.stack$vec);
#                   my.stack$vec = pinfo$vec; pval = pinfo$val;
#                   }





