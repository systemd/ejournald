-task({"build:nif", "Build the ejournald NIF library"}).
-task({"clean:nif", "Clean the ejournald NIF library"}).

run("build:nif", _) ->
    tetrapak:outputcmd(tetrapak:subdir("c_src"), "make", [cflags(), "all"]);

run("clean:nif", _) ->
    tetrapak:outputcmd(tetrapak:subdir("c_src"), "make", [cflags(), "clean"]).

cflags() ->
    ["CFLAGS=", "-O2 ", ["-I", code:root_dir(), "/erts-", erlang:system_info(version), "/include"]].
