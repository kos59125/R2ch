# The MIT License
#
# Copyright (c) 2010 RecycleBin.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

#' スレタイ一覧
#' 
#' スレッド一覧をデータフレームとして取得します。
#' 
#' @param server.uri
#'    鯖 URI
#' @param
#'    bg20 鯖を利用するか
#' 
#' @details
#' 以下のデータフレームが返ります。
#' 
#' \code{DAT}   DAT ファイルの URI。
#' 
#' \code{Subject}   スレッドのタイトル。
#' 
#' \code{Count}	スレッドのレス数。
#' 
#' @export
getSubjectList <- function(server.uri, bg20 = FALSE)
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
	
	separator <- "<>";
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
