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

## バイト列を Shift_JIS でエンコードします。
r2ch.rawToChar <- function(bytes)
{
	if (is.null(bytes))
	{
		return("");
	}
	
	string <- rawToChar(bytes);
	string <- iconv(string, from = r2ch.specification["encoding"], sub = "byte");

	return(string);
}

## 文字列を Shift_JIS に変換した後，バイト列にデコードします。
r2ch.charToRaw <- function(string)
{
	if (is.null(string))
	{
		return(NULL);
	}

	string <- iconv(string, to = r2ch.specification["encoding"]);
	bytes <- charToRaw(string);

	return(bytes);
}

## バイト列に指定した文字列が含まれるかを調べます。
## 指定した文字列が開始するインデックスのベクトルを返します。
r2ch.matches <- function(bytes, value)
{
	if (is.null(bytes))
	{
		return(NULL);
	}
	
	value <- r2ch.charToRaw(value);
	
	indices <- 1 : length(bytes);
	result <- indices[mapply(function (i) return(all(bytes[i : (i + length(value) - 1)] == value)), indices)];
	return(result);
}

## バイト列中に含まれる文字列を異なる文字列に変換します。
r2ch.replaceBytes <- function(bytes, old.value, new.value)
{
	split <- r2ch.splitBytes(bytes, old.value);
	split.length <- length(split);
	replacement <- r2ch.charToRaw(new.value);

	last <- split[[split.length]];
	result <- lapply(split[-split.length], c, replacement);
	result <- unlist(c(result, last));
	return(result);
}

## バイト列を指定した文字列で分割します。
r2ch.splitBytes <- function(bytes, separator)
{
	separator.raw <- r2ch.charToRaw(separator);
	indices <- r2ch.matches(bytes, separator);
	index.start <- c(1, indices + length(separator.raw));
	index.end <- c(indices - 1, length(bytes));
	list.length <- length(index.start);
	
	result <- NULL;
	for (i in 1 : list.length)
	{
		if (index.start[i] <= index.end[i])
		{
			s <- bytes[index.start[i] : index.end[i]];
			result <- c(result, list(s))
		}
		else if (i != list.length)
		{
			result <- c(result, list(NULL));
		}
	}
	
	return(result);
}
