Test fetches predefined repositories and tries to parse each scala file with dotty parser.

For some new dotty constructs parsing support is not yet added and those files
containing them are explicitly excluded from parsing.

If you change parser for dotty and are unsure of your changes you should run this test.

Test still tries to parse files that are excluded, maybe your changes actually fixed something.
In this case you will get information about that in test output, please update excluded files in that case.

Test also produces performance statistics, you can compare times before and after your changes to see
if you haven't introduced any noticeable slowdown.

As a reference running on my(kpbochenek) computer:

Dotty codebase(hash: 85d1322c4e8a7254b67dd7b88fa8fdf87b4dac72):
Files parsed correctly 775
Files errored: 50
Time taken: 7757ms
Lines parsed: ~130k
Parsing speed per 1k lines ===> 59 ms/1klines

Execute `bloop test communitytest` to run community tests.
