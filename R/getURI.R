## 指定した URI のデータをバイナリとして取得し， LF (0A) で区切ったリストを返します。
r2ch.getURI <- function(uri)
{
	if (length(uri) > 1)
	{
		warning("only the first element will be used.");
	}
	
	bytes <- NULL;
	buffer.size <- 0x1000;  # 4 KiB

	connection <- url(uri[1], open = "rb");
	while (length(buffer <- readBin(connection, raw(), buffer.size)) > 0)
	{
		bytes <- c(bytes, buffer);
	}
	close(connection);
	
	newline <- r2ch.specification["newline"];
	bytes <- r2ch.splitBytes(bytes, newline);

	invisible(bytes);
}
