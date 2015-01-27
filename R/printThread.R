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

#' レス出力
#' 
#' スレッドの内容をコンソールに出力します。
#' 1 回の出力でレスが完了しない場合， Enter を押すたびに次のレスが出力されます。
#' 出力を中断したい場合は q をタイプしてから Enter を押します。
#' 
#' @param thread
#'    スレッドデータフレーム。
#' @param index
#'    出力するレス番号。
#' @param by
#'    1 回に出力するレス数。
#' @param format
#'    レスの出力フォーマット関数。
#' 
#' @export
printThread <- function(thread, index = 1 : nrow(thread), by = 1,
		format = function(post)
		{
			email <- sprintf("[%s]", post$Email);
			## tz = "JST" だと時間関数で警告が出たので origin で調節した。
			## see getThread
			datetime <- strftime(as.POSIXct(post$DateTime, origin = "1970-01-01 09:00:00"), "%Y/%m/%d(%a) %H:%M:%S")
			header <- paste(post$Number, post$Name, email, datetime);
			message <- paste("--", header, "--", post$Message, sep = "\n");
			return(message);
		})
{
	if (!is.numeric(by) || by <= 0 || round(by) != by)
	{
		stop("'by' must be a positive integer");
	}

	count <- length(index);
	output <- NULL;
	if (by >= count)
	{
		output <- list(index);
	}
	else if (by == 1)
	{
		output <- mapply(list, index);
	}
	else
	{
		from <- seq(1, count, by = by);
		to <- c(from[-1] - 1, count);
		output <- mapply(function(a, b) list(index[a : b]), from, to);
	}

	count <- length(output);
	position <- 1;
	option <- NULL;
	while (position <= count)
	{
		if (is.null(option))
		{
			post <- thread[output[[position]], ];
			message <- format(post);
			cat(message, sep = "\n", fill = TRUE);
		}
		if (position != count)
		{
			option <- readline()[1];
			if (option == "q" || option == "Q")
			{
				break();
			}
			else if (option == "h" || option == "?")
			{
				cat("q, Q\tQuit printing\nh, ?\tShow this help", fill = TRUE);
				next();
			}
		}
		position <- position + 1;
		option <- NULL;
	}
}
