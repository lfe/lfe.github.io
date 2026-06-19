# LFE Friday — Core Set Plan

*Generated: 2026-06-04*

This plan covers the **176 uncovered modules** in the core OTP set (stdlib,
kernel, erts, crypto, ssl, inets, mnesia, tools, sasl, ssh, os_mon,
runtime_tools, parsetools, syntax_tools, xmerl). Modules are organized into
thematic arcs — multi-week sequences that build on each other — mirroring the
series' proven approach (the ETS and digraph arcs were its strongest runs).

Partially-covered modules (lists, string, erlang, etc.) are included where
significant gaps remain.


## Arc 1: Maps and Modern Data Structures (8 weeks)

The single biggest gap. maps was introduced in OTP 17 (2014) — after the
original series started but never covered.

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 1 | maps | maps:new/0, from_list/1, to_list/1, put/3 | Basics, comparison with dict — get/2 & get/3 covered 2026-06-19 |
| 2 | maps | maps:merge/2, merge_with/3, update/3, update_with/3 | Mutations, merge_with is OTP 24+ |
| 3 | maps | maps:filter/2, filtermap/2, map/2, fold/3, foreach/2 | Functional operations |
| 4 | maps | maps:keys/1, values/1, find/2, is_key/2, size/1, groups_from_list/2 | Querying and grouping |
| 5 | maps | maps:intersect/2, intersect_with/3, iterator/1, next/1 | Set ops and lazy iteration |
| 6 | sets | sets module | Modern set implementation (OTP 24 rewrote internals) |
| 7 | gb_sets | gb_sets module | Balanced-tree sets, when to prefer over sets/ordsets |
| 8 | gb_trees | gb_trees module | General balanced trees, comparison with maps |

**Why this arc first:** maps is the most-used data structure in modern
Erlang/LFE. Starting here signals the revival is forward-looking, not just
picking up where 2016 left off.


## Arc 2: The lists Module — Unfinished Business (6 weeks)

The original series covered 6 list functions. There are 60+ more.

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 9 | lists | lists:map/2, foldl/3, foldr/3 | The workhorses |
| 10 | lists | lists:sort/1, sort/2, usort/1, merge/1, merge/2 | Sorting and merging |
| 11 | lists | lists:zip/2, zip3/3, unzip/1, unzip3/1, zipwith/3 | Zipping |
| 12 | lists | lists:partition/2, splitwith/2, takewhile/2, search/2 | Partitioning (pairs with original dropwhile post) |
| 13 | lists | lists:nth/2, last/1, reverse/1, flatten/1, append/1 | Structural operations |
| 14 | lists | lists:seq/2, seq/3, duplicate/2, enumerate/1 | Generators, enumerate is OTP 25+ |


## Arc 3: OTP Behaviours (10 weeks)

The biggest conceptual gap. The original series never touched OTP behaviours.

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 15 | proc_lib | proc_lib:spawn/1, start_link/3, init_ack/1 | Foundation: what OTP processes really are |
| 16 | gen_server | gen_server part 1: init, handle_call, handle_cast | The workhorse behaviour |
| 17 | gen_server | gen_server part 2: handle_info, terminate, code_change | Lifecycle and hot code |
| 18 | gen_server | gen_server part 3: multi_call, reply, format_status | Advanced patterns |
| 19 | gen_statem | gen_statem part 1: callback_mode, state functions | The modern FSM |
| 20 | gen_statem | gen_statem part 2: state_enter, timeouts, postpone | Advanced state machine features |
| 21 | gen_event | gen_event: add_handler, notify, call | Event managers |
| 22 | supervisor | supervisor part 1: one_for_one, one_for_all, rest_for_one | Supervision strategies |
| 23 | supervisor | supervisor part 2: dynamic children, child specs | Dynamic supervision |
| 24 | supervisor_bridge | supervisor_bridge + sys module | Wrapping non-OTP processes, sys tracing |


## Arc 4: I/O and Files (8 weeks)

Another major gap — the entire file and I/O layer.

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 25 | io | io:format/2, fwrite/2, get_line/1, read/1 | Console I/O |
| 26 | io_lib | io_lib:format/2, write/1, print/1, scan/2 | Formatting and parsing |
| 27 | io_ansi | io_ansi module | Terminal colors and styling (new OTP module) |
| 28 | file | file:read_file/1, write_file/2, open/2, close/1 | Basic file operations |
| 29 | file | file:consult/1, path_open/3, list_dir/1, make_dir/1 | Directory ops, Erlang term files |
| 30 | filename | filename:join/1, split/1, basename/1, extension/1, rootname/1 | Path manipulation |
| 31 | filelib | filelib:wildcard/1, ensure_dir/1, is_dir/1, file_size/1 | Expands on the single existing post |
| 32 | file_sorter | file_sorter module | Large-file external sorting |


## Arc 5: Strings and Text (6 weeks)

The original string posts covered deprecated functions. Time for the modern API.

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 33 | string | string:slice/2, slice/3, find/2, split/2, trim/1 | Modern unicode-aware API (OTP 20+) |
| 34 | string | string:replace/3, pad/2, titlecase/1, casefold/1, to_graphemes/1 | Advanced text manipulation |
| 35 | unicode | unicode:characters_to_binary/1, characters_to_list/1 | Encoding conversions |
| 36 | re | re:run/2, run/3, compile/1, replace/3, split/2 | Regular expressions |
| 37 | uri_string | uri_string:parse/1, compose/1, normalize/1 | URI handling (replaces http_uri) |
| 38 | json | json module | JSON encoding/decoding (OTP 27+, native!) |


## Arc 6: Networking (8 weeks)

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 39 | inet | inet:getaddr/2, gethostname/0, parse_address/1, setopts/2 | Network primitives |
| 40 | gen_tcp | gen_tcp:listen/2, accept/1, connect/3, send/2, recv/2 | TCP client/server |
| 41 | gen_tcp | gen_tcp active mode, {active, once}, packet options | Advanced TCP patterns |
| 42 | gen_udp | gen_udp:open/1, send/4, recv/2 | UDP |
| 43 | ssl | ssl:connect/3, listen/2, handshake/1 | TLS client/server |
| 44 | ssh | ssh:connect/3, ssh_connection:session_channel/2, exec/4 | SSH client |
| 45 | ssh | ssh:daemon/2, ssh_sftp:start_channel/1 | SSH server, SFTP |
| 46 | httpc | httpc deep dive: streaming, async, SSL options | Expands on the single existing post |


## Arc 7: The erlang Module — BIF Safari (6 weeks)

Only 3 functions covered out of 200+ BIFs.

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 47 | erlang | erlang:spawn/1,3, spawn_link/1,3, spawn_monitor/1,3 | Process creation |
| 48 | erlang | erlang:send/2, self/0, process_info/1, is_alive/0 | Process introspection |
| 49 | erlang | erlang:monitor/2, demonitor/1, link/1, unlink/1 | Links and monitors |
| 50 | erlang | erlang:system_info/1, memory/0, statistics/1 | System introspection |
| 51 | erlang | erlang:binary_to_term/1, term_to_binary/1, phash2/1 | Serialization and hashing |
| 52 | erlang | erlang:make_ref/0, monotonic_time/0, unique_integer/0 | Time and uniqueness |


## Arc 8: Concurrency Primitives (4 weeks)

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 53 | atomics | atomics module | Lock-free atomic integers (OTP 21+) |
| 54 | counters | counters module | Mutable counter arrays (OTP 21+) |
| 55 | persistent_term | persistent_term module | Global immutable storage (OTP 21+) |
| 56 | erpc | erpc module | Modern remote procedure calls (OTP 23+) |


## Arc 9: Profiling and Debugging (8 weeks)

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 57 | dbg | dbg:tracer/0, tpl/2, p/2, stop/0 | Trace-based debugging |
| 58 | trace | trace module | New tracing API (OTP 27+) |
| 59 | tprof | tprof module | Time profiling (OTP 27+, replaces eprof) |
| 60 | eprof | eprof module | Time profiling (classic) |
| 61 | fprof | fprof module | Flame-graph-style profiling |
| 62 | cprof | cprof module | Call count profiling |
| 63 | cover | cover module | Code coverage analysis |
| 64 | xref | xref module | Cross-reference analysis |


## Arc 10: Logging and SASL (4 weeks)

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 65 | logger | logger part 1: log/2, set_handler_config/2, adding handlers | Modern logging (OTP 21+) |
| 66 | logger | logger part 2: filters, formatters, custom handlers | Advanced logger |
| 67 | application | application:start/1, get_env/2, ensure_all_started/1 | Application lifecycle |
| 68 | systools | systools:make_script/1, make_tar/1 + release_handler | Releases |


## Arc 11: Mnesia (4 weeks)

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 69 | mnesia | mnesia part 1: create_table/2, write/1, read/2, transaction/1 | Basics |
| 70 | mnesia | mnesia part 2: select/2, match_object/1, index_read/3 | Querying |
| 71 | mnesia | mnesia part 3: table types, disc_copies, disc_only_copies | Storage backends |
| 72 | mnesia | mnesia part 4: add_table_copy/3, change_table_copy_type/3 | Distribution |


## Arc 12: Miscellaneous Essentials (8 weeks)

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 73 | os | os:cmd/1, getenv/1, system_time/0, type/0 | OS interaction |
| 74 | math | math module | Trig, log, exp, pow, pi, sqrt |
| 75 | rand | rand:uniform/0, uniform/1, seed/1, normal/0 | Random numbers (replaces random) |
| 76 | binary | binary:split/2, match/2, part/2, compile_pattern/1 | Binary processing |
| 77 | proplists | proplists:get_value/2, expand/2, lookup_all/2 | Property lists |
| 78 | orddict | orddict module | Ordered dictionaries (comparison with maps) |
| 79 | zip | zip:create/2, extract/1, list_dir/1 | Zip archives (compare with erl_tar) |
| 80 | base64 | base64:encode/1, decode/1, mime_encode/1 | Base64 encoding |


## Arc 13: Crypto (3 weeks)

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 81 | crypto | crypto:hash/2, mac/3, strong_rand_bytes/1 | Hashing and MACs |
| 82 | crypto | crypto:crypto_one_time/4, generate_key/2 | Encryption and key generation |
| 83 | crypto | crypto:sign/4, verify/5 | Digital signatures |


## Arc 14: Developer Tools (6 weeks)

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 84 | escript | escript module | Building standalone scripts |
| 85 | beam_lib | beam_lib module | BEAM file introspection |
| 86 | erl_scan | erl_scan + erl_parse | Tokenizing and parsing Erlang |
| 87 | erl_eval | erl_eval module | Runtime expression evaluation |
| 88 | erl_pp | erl_pp module | Pretty-printing Erlang terms |
| 89 | shell_docs | shell_docs module | Programmatic access to documentation |


## Arc 15: New in Modern OTP (4 weeks)

Posts highlighting what's new since the original series ended.

| Week | Module | Topic | Notes |
|------|--------|-------|-------|
| 90 | json | json module (if not already covered in Arc 5) | OTP 27+ native JSON |
| 91 | graph | graph module | OTP 29, modern graph API |
| 92 | argparse | argparse module | OTP 26+, CLI argument parsing |
| 93 | zstd | zstd module | OTP 29, Zstandard compression |


## Remaining Core Modules (Standalone Posts)

These modules are worth individual posts but don't fit neatly into arcs:

| Module | Application | Topic |
|--------|-------------|-------|
| array | stdlib | Functional arrays |
| dets | stdlib | Disk-based ETS |
| digraph_utils | stdlib | Companion to the covered digraph module |
| disk_log | kernel | Disk-based logging |
| global | kernel | Global name registration |
| global_group | kernel | Node groups |
| inet_res | kernel | DNS resolution |
| init | erts | System initialization |
| instrument | runtime_tools | Memory instrumentation |
| lcnt | tools | Lock counting |
| make | tools | Erlang make |
| msacc | runtime_tools | Microstate accounting |
| net_adm | kernel | Network admin (ping, names) |
| net_kernel | kernel | Distribution kernel |
| peer | stdlib | Peer node management (OTP 25+) |
| pg | kernel | Process groups (OTP 23+) |
| pool | stdlib | Worker pool |
| qlc | stdlib | Query list comprehensions |
| rpc | kernel | Remote procedure calls (classic) |
| scheduler | runtime_tools | Scheduler utilization |
| seq_trace | kernel | Sequential tracing |
| socket | kernel | Low-level socket API (OTP 22+) |
| sofs | stdlib | Sets of sets |
| system_information | runtime_tools | System information collection |
| tags | tools | Emacs TAGS generation |
| zlib | erts | zlib compression |

Plus specialized modules from ssh (10), ssl (4), inets (10), os_mon (5),
sasl (4), parsetools (2), syntax_tools (9), and xmerl (7) that could each
get standalone posts or mini-arcs depending on audience interest.


## Pacing

At one post per week, the 15 arcs alone cover **93 weeks** (~1.8 years). With
the standalone posts, there's material for **130+ weeks** (~2.5 years) from
the core set alone — before touching the extended set.

A realistic launch cadence might be:
- Start with **Arc 1 (maps)** — high impact, signals modernity
- Alternate between long arcs and short ones to maintain variety
- Sprinkle standalone posts between arcs as palette cleansers
- The **OTP behaviours arc** should come early (within the first 6 months) — it's the single most requested topic for LFE learners
