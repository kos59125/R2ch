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

#' スレ
#' 
#' スレッドをデータフレームとして取得します。
#' 時刻は 1970 年 1 月 1 日午前 9 時 0 分 0 秒からの経過秒数として表されます。
#' 
#' @param dat.uri
#'    DAT ファイルの URI
#' 
#' @details
#' 以下のデータフレームが返ります。
#' 
#' \code{Subject}   スレッドのタイトル。
#' 
#' \code{Number}	レス番号。
#' 
#' \code{Trip}	トリップ。
#' 
#' \code{Email}	メールアドレス。
#' 
#' \code{ID}	ID。
#' 
#' \code{BE}	BE ID。
#' 
#' \code{DateTime}	投稿日時。 1970-01-01 09:00:00 (UTC) からの経過秒数。
#' 
#' \code{Message}	本文。
#' 
#' @export
getThread <- function(dat.uri)
{
	if (length(dat.uri) > 1)
	{
		warning("only the first element will be used");
	}

	dat.uri <- toString(dat.uri[1]);

	parseDateTime <- function(x)
	{
		format <- "%Y/%m/%d(%a) %H:%M:%S";
		if (nchar(x) == 14)
		{
			format <- "%y/%m/%d %H:%M";
		}
		if (nchar(x) == 17)
		{
			format <- "%y/%m/%d %H:%M:%S";
		}
		## Windows で tz = "JST" を指定すると unkown timezone と警告がでる。
		## strptime, as.POSIXct, as.POSIXlt
		## strftime は正常に動く
		return(as.POSIXct(strptime(x, format)));
	}

	thread <- r2ch.getURI(dat.uri);
	
	name <- NULL;
	trip <- NULL;
	email <- NULL;
	datetime <- NULL;
	id <- NULL;
	be <- NULL;
	message <- NULL;
	
	separator <- "<>";
	thread <- lapply(thread, r2ch.splitBytes, separator);

	newline <- r2ch.charToRaw("\n");
	number <- 1 : length(thread);
	for (i in number)
	{
		row <- thread[[i]];
		
		raw.name <- r2ch.splitBytes(row[[1]], "</b>");
		name.current <- raw.name[[1]];
		name.current <- r2ch.rawToChar(name.current);
		name <- c(name, name.current);
		
		## FIXME: fushianasan !omikuji あたりを考慮していないので必ずしもトリップとは限らない。
		trip.current <- NA;
		if (length(raw.name) > 1)
		{
			trip.current <- raw.name[[2]];
			trip.current <- trip.current[-(length(trip.current) : (length(trip.current) - 3))];
			trip.current <- r2ch.rawToChar(trip.current);
		}
		trip <- c(trip, trip.current);
		
		email.current <- row[[2]];
		email.current <- r2ch.rawToChar(email.current);
		email <- c(email, email.current)
		
		raw.datetime <- row[[3]];
		raw.datetime <- r2ch.rawToChar(raw.datetime);
		raw.datetime <- strsplit(raw.datetime, " ")[[1]];
		datetime.current <- sprintf("%s %s", raw.datetime[1], raw.datetime[2]);
		datetime.current <- parseDateTime(datetime.current);
		datetime <- c(datetime, datetime.current);

		id.current <- NA;
		## Over 1000 Thread だと 3 番目は ID じゃないので。
		if (length(raw.datetime) >= 3 && strtrim(raw.datetime[3], 3) == "ID:")
		{
			id.current <- raw.datetime[3];
		}
		id <- c(id, id.current);
		
		be.current <- NA;
		if (length(raw.datetime) >= 4)
		{
			be.current <- raw.datetime[4];
		}
		be <- c(be, be.current);
		
		message.current <- row[[4]];
		message.current <- r2ch.splitBytes(message.current, "<br>");
		message.current <- lapply(message.current, function(s) return(c(s[-c(1, length(s))], newline)));
		message.current <- lapply(message.current, r2ch.removeTags);
		message.current <- lapply(message.current, r2ch.decodeHTML);
		message.current <- unlist(message.current);
		message.current <- r2ch.rawToChar(message.current[-length(message.current)]);
		message <- c(message, message.current);
	}
	
	## 最初の行にしかスレタイが書かれていない。
	subject <- r2ch.rawToChar(thread[[1]][[5]]);

	return(data.frame(Subject = subject, Number = number, Name = name, Trip = trip, Email = email, ID = id, BE = be, DateTime = datetime, Message = message));
}
