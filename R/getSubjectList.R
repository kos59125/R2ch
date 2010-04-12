## スレッド一覧をデータフレームとして取得します。
r2ch.getSubjectList <- function(server.uri, bg20 = FALSE)
{
	if (length(server.uri) > 1)
	{
		warning("only the first element will be used");
	}

	server.uri <- as.character(server.uri[1]);
	uri.fragment <- strsplit(server.uri, "/")[[1]];
	host <- uri.fragment[3];
	bbs.id <- uri.fragment[4];
	subject.txt.uri <- sprintf("http://%s/%s/subject.txt", host, bbs.id);
	subject <- r2ch.getURI(subject.txt.uri);
	
	dat <- NULL;
	name <- NULL;
	count <- NULL;
	
	separator <- r2ch.specification["separator"];
	subject <- lapply(subject, r2ch.splitBytes, separator);

	for (i in 1 : length(subject))
	{
		row <- subject[[i]];
		
		dat.current <- r2ch.rawToChar(row[[1]]);
		if (bg20)
		{
			dat.current <- strtrim(dat.current, nchar(dat.current) - nchar(".dat"));
			dat.current <- sprintf("http://bg20.2ch.net/test/r.so/%s/%s/%s/", host, bbs.id, dat.current);
		}
		else
		{
			dat.current <- sprintf("http://%s/%s/dat/%s", host, bbs.id, dat.current);
		}
		dat <- c(dat, dat.current);
		
		bytes <- row[[2]];
		parenthesis <- max(r2ch.matches(bytes, "("));
		name.current <- bytes[1 : (parenthesis - 2)];
		name.current <- r2ch.rawToChar(name.current);
		name <- c(name, name.current);
		
		count.current <- bytes[(parenthesis + 1) : (length(bytes) - 1)];
		count.current <- r2ch.rawToChar(count.current);
		count.current <- as.integer(count.current);
		count <- c(count, count.current);
	}

	return(data.frame(DAT = dat, Subject = name, Count = count));
}
