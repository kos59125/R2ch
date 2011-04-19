## Ensure you have already read all sources in R2ch/R like the following.
# for (src in list.files("R")) source(paste("R", src, sep = "/"));

# 板一覧を取得。
bbs <- r2ch.getBoardList();
head(bbs);

# Windows 板のスレッド情報を取得。
win <- r2ch.getSubjectList(bbs[bbs$Name == "Windows", "URI"]);
head(win);

# Windows 板の 10 番目のスレッドを取得。
thread <- r2ch.getThread(win[10, "DAT"]);
head(thread);

# 取得したスレッドを 3 レスずつ出力。
r2ch.printThread(thread, by = 3);
# [Enter] で次レス表示。
# ? + [Enter] でヘルプ表示。
# q + [Enter] で中断。
