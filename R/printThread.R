## スレッドの内容をコンソールに出力します。
r2ch.printThread <- function(thread, index = 1 : nrow(thread), by = 1,
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
