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

## 板一覧をデータフレームとして取得します。
r2ch.getBoardList <- function(bbsmenu.uri = "http://menu.2ch.net/bbsmenu.html")
{
	if (length(bbsmenu.uri) > 1)
	{
		warning("only the first element will be used");
	}

	bbsmenu <- r2ch.getURI(bbsmenu.uri[1]);
	bbsmenu <- lapply(bbsmenu, r2ch.rawToChar);
	
	category <- NULL;
	name <- NULL;
	uri <- NULL;
	
	category.current <- NULL;
	category.regex <- "<BR><BR><B>.+</B>";
	board.regex <- "<A HREF=http://\\w+\\.2ch\\.net/\\w+/>.+</A>";
	board.uri.regex <- substring(board.regex, 1, nchar(board.regex) - nchar(".+</A>"));
	for (i in 1 : length(bbsmenu))
	{
		category.match <- regexpr(category.regex, bbsmenu[i]);
		if (category.match > 0)
		{
			category.start <- category.match + nchar("<BR><BR><B>");
			category.end <- (category.match - 1) + attr(category.match, "match.length") - nchar("</B>");
			category.current <- substring(bbsmenu[i], category.start, category.end);
			next();
		}
		else if (is.null(category.current))
		{
			next();
		}
		
		board.match <- regexpr(board.regex, bbsmenu[i]);
		if (board.match < 0)
		{
			next();
		}
		
		board.uri.match <- regexpr(board.uri.regex, bbsmenu[i]);  # 定義から必ずマッチする。
		tag.start.length <- attr(board.uri.match, "match.length");
		board.name.start <- board.match + tag.start.length;
		board.name.end <- (board.match - 1) + attr(board.match, "match.length") - nchar("</A>");
		board.uri.start <- board.uri.match + nchar("<A HREF=");
		board.uri.end <- (board.uri.match - 1) + tag.start.length - nchar(">");
		board.uri <- substring(bbsmenu[i], board.uri.start, board.uri.end);
		board.name <- substring(bbsmenu[i], board.name.start, board.name.end);
		
		category <- c(category, category.current);
		name <- c(name, board.name);
		uri <- c(uri, board.uri);
	}

	return(data.frame(Category = category, Name = name, URI = uri));
}
