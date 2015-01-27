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

## 指定した URI のデータをバイナリとして取得し， LF (0A) で区切ったリストを返します。
r2ch.getURI <- function(uri, buffer.size = 0x1000)
{
	if (buffer.size <= 0)
	{
		stop("buffer.size must be greater than 0.");
	}
	if (length(uri) > 1)
	{
		warning("only the first element will be used.");
	}
	
	bytes <- NULL;

	connection <- url(uri[1], open = "rb");
	while (length(buffer <- readBin(connection, raw(), buffer.size)) > 0)
	{
		bytes <- c(bytes, buffer);
	}
	close(connection);
	
	newline <- "\n";
	bytes <- r2ch.splitBytes(bytes, newline);

	invisible(bytes);
}
